

%% Move to mqtt module...

-module(action_web_push_api_call).

-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    {topic, Topic} = proplists:lookup(topic, Args),
    {ok, Topic1} = z_mqtt:map_topic( Topic, Context ),
    Topic2 = z_mqtt:flatten_topic( Topic1 ),
    TopicJS = z_utils:js_escape( Topic2 ),

    Message = case proplists:get_value(js_msg, Args) of
        undefined ->
            z_utils:js_object(proplists:delete(topic, Args));
        Msg ->
            case Msg of
                {trust, JsValue} -> JsValue;
                _ -> js_value(z_utils:js_escape(Msg))
            end
    end,

    Script = iolist_to_binary([<<"cotonic.broker.call('">>, TopicJS, <<"',">>, Message, <<").then((msg, bindings) => console.log(bindings, msg));">>]),
    {Script, Context}.

%%
%% Helpers
%%

js_value(Str) when is_binary(Str) orelse is_list(Str) -> [$", Str, $"];
js_value(Int) when is_integer(Int) -> z_convert:to_list(Int);
js_value(true) -> <<"true">>;
js_value(false) -> <<"false">>;
js_value(undefined) -> <<"undefined">>.

