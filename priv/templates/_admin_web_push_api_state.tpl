{% if q.message.topic == "webPush/event/subscribed" or q.message.payload === true %}
    <p class="help-block">{_ ✅ This browser is subscribed. _}</p>
    <button class="btn btn-danger"
            data-onclick-topic="model/webPush/post/unsubscribe">
        {_ Unsubscribe _}
    </button>
{% elif q.message.topic == "webPush/event/unsubscribed" or q.message.payload === false %}
    <p class="help-block">{_ This browser is not subscribed. _}</p>
    <button class="btn btn-success"
            data-onclick-topic="webPush/post/subscribe">
        {_ Subscribe _}
    </button>
{% elif q.message.topic == "webPush/event/not_supported" %}
    <p class="help-block">{_ ⚠️  This browser does not support web push notifications. _}</p>
{% else %}
    <p class="help-block">{_ ... _}</p>
{% endif %}
