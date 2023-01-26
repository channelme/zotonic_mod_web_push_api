%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2023 Channel.me
%% @doc Web Push API for Zotonic

%% Copyright 2023 Channel.me
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_web_push_api).
-author("Maas-Maarten Zeeman <maas@channel.me>").

-mod_title("Web Push API").
-mod_description("Web Push API for Zotonic").
-mod_provides([]).
-mod_depends([]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    init/1,
    event/2
]).

-export([
    encrypt/3,
    make_audience/1
]).

init(_Context) ->
    ok.

event(#postback{message={store_subscription, _Args}}, Context) ->
    case z_acl:user(Context) of
        undefined ->
            Context;
        UserId ->
            Endpoint = z_context:get_q(<<"endpoint">>, Context),
            ExpirationTime = z_context:get_q(<<"expirationTime">>, Context),
            Keys = z_context:get_q(<<"keys">>, Context),

            Subscription = #{
                             endpoint => Endpoint,
                             keys => Keys
                            },
            Subscription1 = case ExpirationTime of
                                undefined ->
                                    Subscription;
                                _ ->
                                    Subscription#{ expirationTime => ExpirationTime }
                            end,

            %%ApplicationServerKey = base64:decode(maps:get(<<"applicationServerKey">>, Options)),
            %% The hash of the application server key under which the subscription was stored.
            %% [TODO] Check if het key auth gedeelte gebruikt kan worden als Id van de subscription.
            KeyHash = z_utils:hex_sha(base64url:decode(m_web_push_api:public_key(Context))),

            %% Note: the data can also include an expiry date, but this is never set by browsers
            Props = [{propb, ?DB_PROPS(Subscription1)}, % Store the subscription.
                     {prop1, KeyHash}   % store hash of the key. Can be used check if the subscription is ready.
                    ],

            Id = z_utils:hex_sha(Endpoint),
            {ok, _IdnId} = m_identity:insert(UserId, web_push_api_subscription, Id, Props, Context),

            Context
    end.

encrypt(Message, #{ keys := Keys }=Subscription, PaddingLength) ->
    Padding = make_padding(PaddingLength),

    Plaintext = <<Padding/binary, Message/binary>>,

    ClientPublicKey = base64url:decode(maps:get(<<"p256dh">>, Keys)),
    ClientAuthToken = base64url:decode(maps:get(<<"auth">>, Keys)),

    ?assertEqual(16, size(ClientAuthToken)),
    ?assertEqual(65, size(ClientPublicKey)),

    Salt = crypto:strong_rand_bytes(16),

    {ServerPublicKey, ServerPrivateKey} = crypto:generate_key(ecdh, prime256v1),

    SharedSecret = crypto:compute_key(ecdh, ClientPublicKey, ServerPrivateKey, prime256v1),

    Prk = hkdf(ClientAuthToken, SharedSecret, <<"Content-Encoding: auth", 0>>, 32),

    CryptoContext = create_context(ClientPublicKey, ServerPublicKey),
    
    ContentEncryptionKeyInfo = create_info(<<"aesgcm">>, CryptoContext),
    ContentEncryptionKey = hkdf(Salt, Prk, ContentEncryptionKeyInfo, 16),

    NonceInfo = create_info(<<"nonce">>, CryptoContext),
    Nonce = hkdf(Salt, Prk, NonceInfo, 12),

    Ciphertext = encrypt_payload(Plaintext, ContentEncryptionKey, Nonce),

    #{ciphertext => Ciphertext,
      salt => Salt,
      server_public_key => ServerPublicKey}.

create_context(ClientPublicKey, ServerPublicKey) when size(ClientPublicKey) == 65 andalso size(ServerPublicKey) == 65 -> 
  <<0, 65:16/unsigned-big-integer, ClientPublicKey/binary, 65:16/unsigned-big-integer, ServerPublicKey/binary>>.

create_info(Type, CryptoContext) when byte_size(CryptoContext) == 135 ->
    <<"Content-Encoding: ", Type/binary, 0, "P-256", CryptoContext/binary>>.

get_headers(Audience, Expiration, Context) ->
    ExpirationTimestamp = z_datetime:timestamp() + Expiration,

    PublicKey = base64url:decode(m_config:get_value(?MODULE, public_key, Context)),
    PrivateKey = base64url:decode(m_config:get_value(?MODULE, private_key, Context)),
    SubjectEmail = z_convert:to_binary(m_config:get_value(?MODULE, subject_email, Context)),

    Payload = jsx:encode(#{ aud => Audience, exp => ExpirationTimestamp, sub => SubjectEmail }),

    jwk =
      if otp_version < 24 do
        {:ECPrivateKey, 1, private_key, {:namedCurve, {1, 2, 840, 10045, 3, 1, 7}}, public_key}
      else
        {:ECPrivateKey, 1, private_key, {:namedCurve, {1, 2, 840, 10045, 3, 1, 7}}, public_key,
         nil}
      end
      |> JOSE.JWK.from_key()

    {_, jwt} = JOSE.JWS.compact(JOSE.JWT.sign(jwk, %{"alg" => "ES256"}, payload))
    headers(content_encoding, jwt, vapid[:public_key])


send_push(Message, #{ endpoint := Endpoint }=Subscription, AuthToken, TTL) ->
    Payload = encrypt(Message, Subscription),

    Audience = make_audience(Endpoint)

    Headers = [],


    % Auth = crypto:sign(, )
    ok.

hkdf(IKM, Info, Salt, Length) ->
    Prk = crypto:mac(hmac, sha256, Salt, IKM),
    binary:part(crypto:mac(hmac, sha256, Prk, <<Info/binary, 1>>), 0, Length).

encrypt_payload(Plaintext, ContentEncryptionKey, Nonce) ->
    {Ciphertext, Ciphertag} = crypto:crypto_one_time_aead(
                                  aes_128_gcm,
                                  ContentEncryptionKey,
                                  Nonce,
                                  Plaintext,
                                  <<"">>,
                                  true
                                 ),
    <<Ciphertext/binary, Ciphertag/binary>>.

make_padding(Length) ->
    Padding = binary:copy(<<0>>, Length),
    <<Length:16/unsigned-big-integer, Padding/binary>>.

make_audience(Endpoint) ->
    #{ scheme := Scheme, host := Host } = uri_string:parse(Endpoint),
    <<Scheme/binary, "://", Host/binary>>.

