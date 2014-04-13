
require.config
  baseUrl: "js/modules"
  paths:
    'jquery': '../vendor/jquery/jquery'
    'knockout': '../vendor/knockout.js/knockout'
    'gapi': 'https://plus.google.com/js/client:plusone'
  shim:
    gapi:
      exports: 'gapi'


define (require) ->
  $    = require('jquery')
  ko   = require('knockout')
  gapi = require('gapi')

  ko.applyBindings new ->
    @userinfo   = ko.observable()
    @triedLogin = ko.observable false
    @loginError = ko.observable null

    login = (code) -> $.ajax
      type: "POST"
      url:  "/api/login"
      data: JSON.stringify {code: code}

    window.signinCallback = (authResult) =>
      if (authResult['status']['signed_in'])
        @loginError null

        # Send the auth code to the server which uses it to
        # log in the user and query the user's info from Google.
        login(authResult['code']).then (res) =>
          @triedLogin true
          @userinfo res.userinfo
      else
        @triedLogin true
        @loginError authResult['error']

    requestClientId = () -> $.ajax
      accepts: "json"
      type: "GET"
      url: "/api/clientid"

    requestClientId().then (res) ->
      gapi.signin.render "signinButton",
        clientid: res.clientId
        scope: "profile"
        cookiepolicy: "single_host_origin"
        callback: "signinCallback"

    return this
