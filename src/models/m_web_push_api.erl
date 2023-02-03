%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2023 Channel.me
%% @doc Web Push Api Model.

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


-module(m_web_push_api).
-author("Maas-Maarten Zeeman <maas@channel.me>").

-behaviour(zotonic_model).

-include_lib("zotonic_core/include/zotonic.hrl").


-export([
    public_key/1
]).

-export([
    m_get/3,
    m_post/3
]).

m_get([User, <<"subscriptions">> | Rest], _Msg, Context) ->
    UserId = m_rsc:rid(User, Context),
    case z_acl:is_admin(Context) orelse UserId =:= z_acl:user(Context) of
        true ->
            UserId = m_rsc:rid(User, Context),
            Result = m_identity:get_rsc_by_type(UserId, web_push_api_subscription, Context),
            {ok, {Result, Rest}};
        false ->
            {error, eaccess}
    end;

m_get([<<"public_key">> | Rest], _Msg, Context) ->
    case get_public_key(Context) of
        {ok, PublicKey} ->
            {ok, {PublicKey, Rest}};
        {error, _}=Error->
            Error
    end;

m_get(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p lookup: ~p", [?MODULE, V]),
    {error, unknown_path}.

m_post(V, _Msg, _Context) ->
    ?LOG_INFO("Unknown ~p post: ~p", [?MODULE, V]),
    {error, unknown_path}.

%%
%% API
%%

public_key(Context) ->
    {ok, PublicKey} = get_public_key(Context),
    PublicKey.

%%
%% Helpers
%%

get_public_key(Context) ->
    case m_config:get_value(mod_web_push_api, public_key, z_acl:sudo(Context)) of
        undefined ->
            {error, not_configured};
        PublicKey ->
            {ok, PublicKey}
    end.

