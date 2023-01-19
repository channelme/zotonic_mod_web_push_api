/**
 * Copyright 2023 The Cotonic Authors. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS-IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

var cotonic = cotonic || {};

(function(cotonic) {
"use strict";

    function requestPermission(msg, bindings) {
        if(!window.Notification || Notification.permission === "denied") {
            maybeRespond("denied", msg);
        } else if(Notification.permission === "granted") {
            maybeRespond("granted", msg);
        } else {
            let called = false;

            function cb(result) {
                if(called) return;

                called = true;

                publishCurrentState();
                maybeRespond(result, msg);
            }

            const promise = Notification.requestPermission(cb);
            if(promise && typeof promise.then === "function") {
                promise.then(cb)
            }
        }
    }

    function publishCurrentState() {
        let state;

        if(!window.Notification) {
            state = "unsupported";
        } else {
            state = Notification.permission;
        }

        cotonic.broker.publish("model/notification/event/state", state, { retain: true });
    }

    function init() {
        publishCurrentState();

        cotonic.broker.subscribe("model/notification/post/requestPermission", requestPermission);

        cotonic.broker.publish("model/notification/event/ping", "pong", { retain: true });
    }

    function maybeRespond(result, msg) {
        if(msg.properties.response_topic) {
            cotonic.broker.publish(msg.properties.response_topic, result);
        }
    }
    
    init();
}(cotonic));
