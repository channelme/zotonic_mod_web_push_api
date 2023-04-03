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

    function getRegistration() {
        return new Promise((resolve, reject) => {
            if(!navigator.serviceWorker) {
                throw new Error("No service worker")
            }

            resolve(navigator.serviceWorker.ready)
        });
    }
    
    function getSubscription() {
        return getRegistration()
            .then(registration => {
                if(!registration.pushManager) {
                    throw new Error("No push manager");
                }
                
                return registration.pushManager.getSubscription();
            });
    }

    function unsubscribe(msg) {
        getSubscription()
        .then(subscription => {
            if(subscription === null) {
                maybeRespond(true, msg);
            } else {
                return subscription.unsubscribe();
            }
        })
        .then((result) => {
            reportCurrentState();
            maybeRespond(result, msg);
        })
        .catch((err) => {
            reportCurrentState();
            maybeRespond({error: err}, msg);
        })
    }

    function subscribe(msg) {
        getRegistration()
        .then(registration => {
            if(registration.pushManager) {
                return registration.pushManager.subscribe(msg.payload);
            } else {
                throw new Error("No push manager");
            }
        })
        .then(subscription => {
            reportCurrentState();
            maybeRespond(subscription?subscription.toJSON():null, msg);
        })
        .catch((err) => {
             console.warn(err);
             reportCurrentState();
             maybeRespond({error: err}, msg);
        })
    }

    function publishCurrentState(state) {
        cotonic.broker.publish("model/webPush/event/state", state, { retain: true });
    }
    
    function reportCurrentState() {
        getSubscription()
        .then((subscription) => {
            if(subscription && subscription.endpoint) {
                publishCurrentState(true);
            } else {
                publishCurrentState(false);
            }
        })
        .catch((err) => {
            publishCurrentState(false);
        })
    }
    
    function init() {
        cotonic.broker.subscribe("model/webPush/get/subscription", (msg) => {
            getSubscription()
            .then(subscription => {
                maybeRespond(subscription?subscription.toJSON():null, msg)
            })
            .catch(err => {
                maybeRespond({error: err}, msg)
            })
        });

        cotonic.broker.subscribe("model/webPush/post/subscribe", subscribe);
        cotonic.broker.subscribe("model/webPush/post/unsubscribe", unsubscribe);

        cotonic.broker.publish("model/webPush/event/ping", "pong", { retain: true });

        reportCurrentState();
    }

    function maybeRespond(result, msg) {
        if(msg && msg.properties.response_topic) {
            cotonic.broker.publish(msg.properties.response_topic, result);
        }
    }

    cotonic.ready.then(init);
}(cotonic));
