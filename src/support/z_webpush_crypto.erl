%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2023 Channel.me
%% @doc Web Push API Crypto routines.


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

-module(z_webpush_crypto).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("stdlib/include/assert.hrl").
-define(MAX_PAYLOAD_LENGTH, 4078).

-export([
    generate_vapid_key/0,

    make_request/4
]).

% @doc Generate a VAPID key for the server.
generate_vapid_key() ->
    {Public, Private} = crypto:generate_key(ecdh, prime256v1),
    #{ publicKey => base64url:encode(Public),
       privateKey => base64url:encode(Private)}.

% @doc Create a request tuple which can be used to send a push notification to a client.
make_request(Message, #{ endpoint := Endpoint }=Subscription, Options, Context) ->
    #{ salt := Salt,
       server_public_key := ServerPublicKey,
       ciphertext := CipherText } = encrypt(Message, Subscription, 0),

    %% Note in OTP24, the keys have to be erlang strings. From 15.2.2 on these
    %% can be binaries as well.
    CryptoHeaders = get_headers(Endpoint, base64url:encode(ServerPublicKey), Context),

    TTL = maps:get(ttl, Options, 0),
    Headers = [
               { "TTL", [z_convert:to_binary(TTL)] },
               { "Content-Encoding", [<<"aesgcm">>] },
               { "Encryption", [<<"salt=", (base64url:encode(Salt))/binary>>] }
               | CryptoHeaders
              ],

    Headers1 = case maps:get(topic, Options, undefined) of
                   undefined ->
                       Headers;
                   Topic ->
                       [ {"Topic", [z_convert:to_binary(Topic)]} | Headers ]
               end,

   { Endpoint, Headers1, "application/octetstream", CipherText }.

%%
%% Helpers
%%

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

get_headers(Endpoint, ServerPublicKey, Context) ->
    PublicKey = m_config:get_value(mod_web_push_api, public_key, Context),
    PrivateKey = m_config:get_value(mod_web_push_api, private_key, Context),
    SubjectEmail = z_convert:to_binary(m_config:get_value(mod_web_push_api, subject_email, Context)),
    JWTExpiration = m_config:get_value(mod_web_push_api, jwt_expire, 12 * 3600, Context),
    JWTExpirationTimestamp = z_datetime:timestamp() + JWTExpiration,

    Audience = make_audience(Endpoint), 

    Payload = #{ aud => Audience,
                 exp => JWTExpirationTimestamp,
                 sub => <<"mailto:", SubjectEmail/binary>> },

    JWK = to_jwk_key(PublicKey, PrivateKey),
    JWT = erljwt:create(es256, Payload, JWK),

    [{"Authorization", [<<"WebPush ", JWT/binary>>] },
     {"Crypto-Key", [<<"dh=", ServerPublicKey/binary, $;, "p256ecdsa=", PublicKey/binary>>]}
     ].

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

create_context(ClientPublicKey, ServerPublicKey) when size(ClientPublicKey) == 65 andalso size(ServerPublicKey) == 65 -> 
    <<0, 65:16/unsigned-big-integer, ClientPublicKey/binary, 65:16/unsigned-big-integer, ServerPublicKey/binary>>.


create_info(Type, CryptoContext) when byte_size(CryptoContext) == 135 ->
    <<"Content-Encoding: ", Type/binary, 0, "P-256", CryptoContext/binary>>.

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

make_audience(Endpoint) ->
    #{ scheme := Scheme, host := Host } = uri_string:parse(Endpoint),
    <<Scheme/binary, "://", Host/binary>>.
