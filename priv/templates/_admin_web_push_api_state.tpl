{% if q.message.topic == "webPush/event/subscribed" %}
<p class="help-block">{_ ✅ This browser is subscribed. _}</p>
<button class="btn btn-danger"
        data-onclick-topic="webPush/post/unsubscribe">
    {_ Unsubscribe _}
</button>
{% elif q.message.topic == "webPush/event/unsubscribed" %}
<p class="help-block">{_ This browser is not subscribed. _}</p>
<button class="btn btn-success"
        data-onclick-topic="webPush/post/subscribe">
    {_ Subscribe _}
</button>
{% elif q.message.topic == "webPush/event/not_supported" %}
    <p class="help-block">{_ ⚠️ This browser does not support subscribing to web push notifications. _}</p>
{% else %}
    <p class="help-block">{_ ... _}.</p>
{% endif %}
