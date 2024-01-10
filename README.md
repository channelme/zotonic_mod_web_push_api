# zotonic_mod_web_push_api
# 

Web Push API for Zotonic

# Configuration

The module needs 3 config variables.

Use `mod_web_push_api` as module name and create the following config settings:

  - `private_key`: The vapid private key to use.
  - `public_key`: The vapid public key to user.
  - `subject_email`: This email address will be sent to the endpoint servers which manage the push api subscriptions.

You can generate a new vapid key by running in the zotonic debug console:

``` erlang
> z_webpush_crypto:generate_vapid_key().
#{privateKey =>
      <<"8E85ai4_********************">>,
  publicKey =>
      <<"BGgwFt8c_9-**************************">>}
```

# Setting up

The zotonic admin panel has a new side panel which can be used to subscribe the logged on user to web push subscriptions. 

<img width="461" alt="Screen Shot 2023-02-03 at 16 07 14" src="https://user-images.githubusercontent.com/1024972/216637292-c7c1aea9-1626-4cbb-b43f-e460db0d50b7.png">

# Sending Notifications

```erlang
> mod_web_push_api:send(1, #{ type => notification, data => #{ title => <<"Hello">>, options => #{ body => <<"World">>, data => #{ url => <<"/page/123">>} }}}, #{ ttl => 3600 }, z:c(your_site)).
```

This will sent a notification to all currently setop subscriptions for user 1 (Admin). When a subscription has been setup the notification will be received by the service worker of the browser, which will display the notification.

<img width="394" alt="Screen Shot 2023-02-03 at 16 18 21" src="https://user-images.githubusercontent.com/1024972/216640147-760b74de-3df6-4d8e-82c6-30848e8317bd.png">


# TODO

- [x] Queing notifications when the endpoint errors or is offline.
- [x] Retry sending notifications.
- [x] Add configuration panel to the admin.
- [ ] Setup ssl validation for the endpoint connections.
