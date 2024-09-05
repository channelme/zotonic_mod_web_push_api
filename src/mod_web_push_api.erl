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

%%
%% Push API documentation for the server side communication. 
%% https://autopush.readthedocs.io/en/latest/http.html
%%

%% 
%% Example::
%%
%% ```
%% mod_web_push_api:send(1,
%%                       #{ type => notification,
%%                          data => #{ title => <<"Hello">>,
%%                                     options => #{ body => <<"World">>,
%%                                                   data => #{ url => <<"/page/123">> }
%%                                                }
%%                                  }
%%                       },
%%                       #{ ttl => 3600 },
%%                       z:c(site_cafe)).
%% ```
%%
%% When this message is received by to cotonic service worker the 
%% `showNotification` method of the registration will be called. 
%% See: https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification
%%
%% The optional url field in the extra data field in the notification
%% can be used by the service worker to redirect the user to the correct page.
%%

-export([
    init/1,
    terminate/2
]).

-export([
    send/3, send/4,

    task_send/4
]).

%% @doc Initialize the web push api module. 
init(Context) ->
    %% Make sure the module is configured.
    _ = m_web_push_api:public_key(Context),

    start_http_client(Context),
    ok.

%% @doc Terminate the web push api module. 
terminate(_Reason, Context) ->
    stop_http_client(Context),
    ok.


%% Exported api to send messages to a specific user id
send(UserId, Message, Context) ->
    send(UserId, Message, #{ ttl => 0 }, Context).

send(UserId, Message, Options, Context) ->
    case m_identity:get_rsc_by_type(UserId, web_push_api_subscription, Context) of
        [] ->
            ?LOG_WARNING(#{text => <<"No web push api subscriptions found">>,
                           user_id => UserId
                          }),
            ok;
        Subscriptions ->
            [ begin
                  {id, Id} = proplists:lookup(id, Subscription),
                  UniqueKey = unique_key(Id, Message, Options),
                  z_pivot_rsc:insert_task_after(0, ?MODULE, task_send, UniqueKey, [Id, Message, Options], Context)
              end || Subscription <- Subscriptions ],
            ok
    end.

% @doc Send the push notification. Async called by the task manager.
-spec task_send(m_rsc:resource_id(), term(), map(), z:context()) -> ok | {delay, non_neg_integer()} | no_return().
task_send(Id, Message, Options, Context) ->
    Payload = jsx:encode(Message),

    case m_identity:get(Id, Context) of
        undefined ->
            %% The subscription is probably deleted.
            ok;
        Props ->
            Subscription = proplists:get_value(propb, Props),

            case send_push(Payload, Subscription, Options, Context) of
                ok ->
                    ok;
                retry ->
                    throw(retry);
                {delay, After} ->
                    ?LOG_INFO(#{text => <<"Endpoint requested retry">>, 'after' => After }),
                    {delay, After};
                remove_subscription ->
                    ?LOG_INFO(#{text => <<"Endpoint requested deletion of subscription">>, id => Id }),
                    m_identity:delete(Id, Context),
                    ok
            end
    end.

%%
%% Helpers
%%

% @doc Start the http client which will be used to send send push
% messages to the endpoints.
start_http_client(Context) ->
    Name = name(Context),
    case inets:start(httpc, [{profile, Name}]) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

% @doc The http client is no longer needed.
stop_http_client(Context) ->
    Name = name(Context),
    inets:stop(httpc, Name).

send_push(Message, Subscription, Options, Context) ->
    Request = z_webpush_crypto:make_request(Message, Subscription, Options, Context),

    %% TODO ssl certificate check.
    %%
    case httpc:request(post, Request, [{ssl, [{versions, ['tlsv1.2', 'tlsv1.3']}]}], [], name(Context)) of
        {ok, {{_, 201, _}, _ResponseHeaders, _Body}} ->
            %% Ok... message recognized and sent.
            ok;
        {ok, {{_, 404, _}, _ResponseHeaders, _Body}} ->
            %% Not Found
            remove_subscription;
        {ok, {{_, 410, _}, _ResponseHeaders, _Body}} ->
            %% Gone
            remove_subscription;
        {ok, {{_, 429, _}, ResponseHeaders, _Body}} ->
            After = get_retry_after(ResponseHeaders),
            {delay, After};
        {ok, {{_, Code, _}, _ResponseHeaders, _Body}} when Code >= 400 andalso Code < 500 ->
            %% Request error... report and drop;
            ?LOG_ERROR(#{text => <<"Could not send push message: request error.">>,
                         error => Code
                        }),
            ok;
        {ok, {{_, Code, _}, _ResponseHeaders, _Body}} when Code >= 500 andalso Code < 600 ->
            %% Server error... retry later
            ?LOG_ERROR(#{text => <<"Could not send push message: server error">>,
                         error => Code
                        }),
            maybe_retry(Options);
        {error, Err} ->
            %% Server not reachable... retry later.
            ?LOG_ERROR(#{text => <<"Could not send push message.">>,
                         error => Err }),
            maybe_retry(Options)
    end.

%% Get the retry after value from the response headers.
get_retry_after([]) ->
    120;
get_retry_after([{H, V} | Rest]) ->
    case z_string:to_lower(H) of
        <<"retry-after">> ->
            retry_value(V);
        _ ->
            get_retry_after(Rest)
    end.

%% The retry value can either be a http-date or an integer.
retry_value(Value) ->
    case catch cow_date:parse_date(Value) of
        {{_,_,_},{_,_,_}}=Date ->
            Date;
        {'EXIT', _} ->
            z_convert:to_integer(Value)
    end.

% Make a unique key, which optionally uses the topic option
% of the message. This allows updating messages while they
% are in the task-queue for retrying.
unique_key(Id, _Message, #{ topic := Topic }) ->
     Phash = z_convert:to_binary(erlang:phash2({Id, Topic})),
     <<"push-api-", Phash/binary>>;
unique_key(Id, Message, _Options) ->
     Phash = z_convert:to_binary(erlang:phash2({Id, Message})),
     <<"push-api-", Phash/binary>>.

% Retry sending the notification when the TTL > 0.
maybe_retry(#{ ttl := 0 }) ->
    %% No retry needed when TTL = 0
    ok;
maybe_retry(#{ ttl := TTL }) when TTL > 0 ->
    retry.

% Return a name. This is used to get a private http client for
% the module.
name(Context) ->
    z_utils:name_for_site(?MODULE, z_context:site(Context)).

