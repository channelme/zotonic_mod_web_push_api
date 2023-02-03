# zotonic_mod_web_push_api

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

