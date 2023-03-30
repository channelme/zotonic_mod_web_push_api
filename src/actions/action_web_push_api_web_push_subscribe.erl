%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2023 Channel.me
%% @doc Web Push Api Subscribe Action.

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


-module(action_web_push_api_web_push_subscribe).

-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    PublicKey = m_web_push_api:public_key(Context),

    OnSuccess = proplists:get_all_values(on_success, Args),

    RenderContext = z_context:new(Context),
    SuccessScript = z_render:get_script(z_render:wire(lists:flatten(OnSuccess), RenderContext)),

    Script = ["cotonic.broker.call('model/webPush/post/subscribe', { applicationServerKey: \"", PublicKey, "\",
                                                                     userVisibleOnly: true })
                 .then(msg => {
                     return cotonic.broker.call('bridge/origin/model/web_push_api/post/store_subscription', msg.payload);
                 })
                 .then(msg => {", SuccessScript, "})
                 .catch((err) => {
                     console.warn('Could not subscribe', err);
                 });
             "],
    {Script, Context}.

