{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Web Push Notification _}
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-web-push{% endblock %}

{% block widget_content %}
<div class="form-group">
    <div>
        {% for s in m.web_push_api[id].subscriptions %}
            {# [TODO] make this something more useful #}
            {{ s | pprint }}
        {% empty %}
            <p class="help-block">{_ No subscriptions found. _}</p>
        {% endfor %}
    </div>

    <div>
        {% wire name="store_subscription" postback={store_subscription} delegate="mod_web_push_api" %}
        {% wire id=#webpush_subscribe.id action={script script="webPushSubscribe()" } %}
        <button id="{{ #webpush_subscribe.id }}">{_ Subscribe _}</button>

        {% wire id=#webpush_unsubscribe.id action={publish topic="model/webPush/post/unsubscribe" } %}
        <button id="{{ #webpush_unsubscribe.id }}">{_ Unsubscribe _}</button>


        {#
         # In order to subscribe, the browser of the user has to have:
         #   - A working service worker.
         #   - Allowed notifications.
         #   - A web push subscription to the browser.
         #}

         {% javascript %}
             window.webPushSubscribe = function() {
                 const publicKey = "{{ m.web_push_api.public_key }}";
                 cotonic.broker.call("model/webPush/post/subscribe", { applicationServerKey: publicKey,
                                                                       userVisibleOnly: true })
                 .then(msg => z_event("store_subscription", msg.payload))
                 .catch((err) => {
                     console.warn("Could not subscribe", err);
                 });
             }

             cotonic.ready.then(function() {
                 cotonic.broker.subscribe("model/notification/event/state", function(m) {
                     console.log("ready");
                 })
             });

             // This can be done to check if the current subscription is still valid.
             // The server key could have been changed.
             cotonic.ready
                 .then(() => cotonic.broker.call("model/webPush/get/subscription"))
                 .then((connection) => console.log("conn", connection.payload))
         {% endjavascript %}
    </div>
</div>
{% endblock %}
