{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Web Push Notification _}
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}sidebar-web-push{% endblock %}

{% block widget_content %}
<div class="form-group">
    <div>
        {#
         # In order to subscribe, the browser of the user has to have:
         #   - A working service worker.
         #   - Allowed notifications.
         #   - A web push subscription to the browser.
         #}

         {% javascript %}
             cotonic.ready.then(function() {
                 cotonic.broker.subscribe("model/notification/event/state", function(m) {
                     console.log("ready");
                 })
             })
         {% endjavascript %}§
    </div>
</div>
{% endblock %}
