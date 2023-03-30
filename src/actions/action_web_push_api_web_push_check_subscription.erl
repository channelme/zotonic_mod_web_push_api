%%
%%
%%

-module(action_web_push_api_web_push_check_subscription).

-include_lib("zotonic_core/include/zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    OnSubscribed = proplists:get_all_values(on_subscribed, Args),
    OnUnsubscribed = proplists:get_all_values(on_unsubscribed, Args),
    OnNotSupported = proplists:get_all_values(on_not_supported, Args),

    RenderContext = z_context:new(Context),

    SubscribedScript = z_render:get_script(z_render:wire(lists:flatten(OnSubscribed), RenderContext)),
    UnsubscribedScript = z_render:get_script(z_render:wire(lists:flatten(OnUnsubscribed), RenderContext)),
    NotSupportedScript = z_render:get_script(z_render:wire(lists:flatten(OnNotSupported), RenderContext)),

    Script = ["cotonic.broker.call('model/webPush/get/subscription') 
               .then( (msg) => {
                     const subscription = msg.payload;
                     if(subscription && subscription.endpoint) {", SubscribedScript, "}
                     else if (subscription && subscription.error) {", NotSupportedScript, "}
                     else {", UnsubscribedScript, "}
               })
             "],

    {Script, Context}.
