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

-define(MAX_PAYLOAD_LENGTH, 4078).

-export([
    init/1,
    event/2
]).

-export([
    encrypt/3,
    make_audience/1,

    send/3,

    send_push/3, send_push/5
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

encrypt(Message, #{ keys := Keys }, PaddingLength)
  when byte_size(Message) + PaddingLength =< ?MAX_PAYLOAD_LENGTH ->
    Padding = make_padding(PaddingLength),

    Plaintext = <<Padding/binary, Message/binary>>,

    ClientPublicKey = base64url:decode(maps:get(<<"p256dh">>, Keys)),
    ClientAuthToken = base64url:decode(maps:get(<<"auth">>, Keys)),

    ?assertEqual(16, size(ClientAuthToken)),
    ?assertEqual(65, size(ClientPublicKey)),

    Salt = crypto:strong_rand_bytes(16),

    {ServerPublicKey, ServerPrivateKey} = crypto:generate_key(ecdh, prime256v1),

    SharedSecret = crypto:compute_key(ecdh, ClientPublicKey, ServerPrivateKey, prime256v1),
    Prk = hkdf(SharedSecret, <<"Content-Encoding: auth", 0>>, ClientAuthToken, 32),

    CryptoContext = create_context(ClientPublicKey, ServerPublicKey),
    
    ContentEncryptionKeyInfo = create_info(<<"aesgcm">>, CryptoContext),
    ContentEncryptionKey = hkdf(Prk, ContentEncryptionKeyInfo, Salt, 16),

    NonceInfo = create_info(<<"nonce">>, CryptoContext),
    Nonce = hkdf(Prk, NonceInfo, Salt, 12),

    Ciphertext = encrypt_payload(Plaintext, ContentEncryptionKey, Nonce),

    #{ciphertext => Ciphertext,
      salt => Salt,
      server_public_key => ServerPublicKey}.

create_context(ClientPublicKey, ServerPublicKey) when size(ClientPublicKey) == 65 andalso size(ServerPublicKey) == 65 -> 
    <<0, 65:16/unsigned-big-integer, ClientPublicKey/binary, 65:16/unsigned-big-integer, ServerPublicKey/binary>>.

create_info(Type, CryptoContext) when byte_size(CryptoContext) == 135 ->
    <<"Content-Encoding: ", Type/binary, 0, "P-256", CryptoContext/binary>>.


%% Exported api to send messages to a specific user id
send(UserId, Message, Context) ->
    Payload = jsx:encode(Message),

    case m_identity:get_rsc_by_type(UserId, web_push_api_subscription, Context) of
        [] ->
            ?LOG_WARNING(#{text => <<"No web push api subscriptions found">>,
                           user_id => UserId
                          }),
            ok;
        Subscriptions ->
            [ begin
                  S = proplists:get_value(propb, SubProps),
                  send_push(Payload, S, Context)
              end || SubProps <- Subscriptions ],
            ok
    end.


%% Note fixed TTL.
send_push(Message, Subscription, Context) ->
    send_push(Message, Subscription, nil, 0, Context).

send_push(Message, #{ endpoint := Endpoint }=Subscription, AuthToken, TTL, Context) ->
    Payload = encrypt(Message, Subscription, 0),
    Audience = make_audience(Endpoint),

    ?DEBUG(Payload),

    %% Note in OTP24, the keys have to be erlang strings. From 15.2.2 on these
    %% can be binaries as well.
    Headers = [
               { "TTL", z_convert:to_binary(TTL) },
               { "Content-Encoding", <<"aesgcm">> },
               { "Encryption", <<"salt=", (base64url:encode(maps:get(salt, Payload)))/binary >> }

               | get_headers(Audience, base64url:encode(maps:get(server_public_key, Payload)), 12 * 3600, Context)
              ],

    ?DEBUG(Headers),

    Request = {
      Endpoint,
      Headers,
      "application/octetstream",
      maps:get(ciphertext, Payload)
     },

    ?DEBUG(httpc:request(post, Request, [{ssl, [{versions, ['tlsv1.2', 'tlsv1.3']}]}], [])),

    ok.

hkdf(IKM, Info, Salt, Length) ->
    Prk = crypto:mac(hmac, sha256, Salt, IKM),
    InfoMac = crypto:mac(hmac, sha256, Prk, <<Info/binary, 1>>),
    binary:part(InfoMac, 0, Length).

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

get_headers(Audience, ServerPublicKey, Expiration, Context) ->
    ExpirationTimestamp = z_datetime:timestamp() + Expiration,

    PublicKey = m_config:get_value(?MODULE, public_key, Context),
    PrivateKey = m_config:get_value(?MODULE, private_key, Context),
    SubjectEmail = z_convert:to_binary(m_config:get_value(?MODULE, subject_email, Context)),

    Payload = #{ aud => Audience,
                 exp => ExpirationTimestamp,
                 sub => SubjectEmail },

    ?DEBUG(Payload),

    JWK = to_jwk_key(PublicKey, PrivateKey),
    JWT = erljwt:create(es256, Payload, JWK),

    [{"Authorization", <<"WebPush ", JWT/binary>> },
     {"Crypto-Key", <<"dh=", ServerPublicKey/binary, $;, "p256ecdsa=", PublicKey/binary>>}
     ].

to_jwk_key(PublicKey, PrivateKey) ->
    PK = base64url:decode(PublicKey),
    {X, Y} = public_key_to_x_y(PK),
    
    #{ d => PrivateKey,
       kty => <<"EC">>,
       crv => <<"P-256">>,
       x => base64url:encode(X),
       y => base64url:encode(Y)
     }.

public_key_to_x_y(<< 16#04, X:32/binary, Y:32/binary >>) ->
    {X, Y};
public_key_to_x_y(<< 16#04, X:48/binary, Y:48/binary >>) ->
    {X, Y};
public_key_to_x_y(<< 16#04, X:66/binary, Y:66/binary >>) ->
    {X, Y}.

