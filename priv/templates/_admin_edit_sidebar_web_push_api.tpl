{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Web Push Notification _}{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-web-push{% endblock %}

{% block widget_content %}
<div class="form-group">
    <div>
        {% if m.web_push_api[id].subscriptions | length as nr_subscriptions %}
            <p class="help-block">{{ nr_subscriptions }} {_ Push subscriptions stored. _}</p>
        {% else %}
            <p class="help-block">{_ No push subscriptions found. _}</p>
        {% endif %}
    </div>

    <div>
        {% live template="_admin_web_push_api_state.tpl" topic="webPush/event/#" id=id %} 

        {#
         # In order to subscribe, the browser of the user has to have:
         #   - A working service worker.
         #   - Allowed notifications.
         #   - A web push subscription to the browser.
         #}
        {% javascript %}
             function webPushSubscribe() {
                 const publicKey = "{{ m.web_push_api.public_key }}";
                 cotonic.broker.call("model/webPush/post/subscribe", { applicationServerKey: publicKey,
                                                                       userVisibleOnly: true })
                 .then(msg => {
                     return cotonic.broker.call("bridge/origin/model/web_push_api/post/store_subscription", msg.payload);
                 })
                 .then(msg => {
                     // Success..
                 })
                 .catch((err) => {
                     console.warn("Could not subscribe", err);
                 });
             }

             function webPushUnsubscribe() {
                 cotonic.broker.publish("model/webPush/post/unsubscribe");
             }

             function checkSubscription() {
                 cotonic.ready
                 .then( () => cotonic.broker.call("model/webPush/get/subscription") )
                 .then( (msg) => {
                     const subscription = msg.payload;
                     if(subscription && subscription.endpoint) {
                         cotonic.broker.publish("webPush/event/subscribed");
                     } else if (subscription && subscription.error) {
                         cotonic.broker.publish("webPush/event/not_supported");
                     } else {
                         cotonic.broker.publish("webPush/event/unsubscribed");
                     }
                 });
             }
             checkSubscription();

             // Register subscribe and unsubscribe
             cotonic.ready
             .then( () => {
                 cotonic.broker.subscribe("webPush/post/subscribe", webPushSubscribe);
                 cotonic.broker.subscribe("webPush/post/unsubscribe", webPushUnsubscribe);
                 cotonic.broker.subscribe("model/webPush/event/#", checkSubscription);
             })
         {% endjavascript %}
    </div>
</div>
{% endblock %}
