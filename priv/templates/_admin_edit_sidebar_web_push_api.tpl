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
        {% wire type={mqtt topic="webPush/post/subscribe"} action={web_push_subscribe} %}
        {% wire type="load" action={web_push_check_subscription
                                       on_subscribed={publish topic="webPush/event/subscribed"}
                                       on_unsubscribed={publish topic="webPush/event/unsubscribed"}
                                       on_not_supported={publish topic="webPush/event/not_supported"} }
         %}
        {% live template="_admin_web_push_api_state.tpl" topic="webPush/event/#" id=id %} 
    </div>
</div>
{% endblock %}
