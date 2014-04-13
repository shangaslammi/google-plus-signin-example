
require.config
  baseUrl: "js/modules"
  paths:
    'jquery': '../vendor/jquery/jquery'
    'knockout': '../vendor/knockout.js/knockout'
    'gapi': 'https://plus.google.com/js/client:plusone'
  shim:
    gapi:
      exports: 'gapi'

window.signinCallback = (authResult) ->
  if (authResult['status']['signed_in'])
    console.log "signed in", authResult
    document.getElementById('gSigninWrapper').setAttribute('style', 'display: none')
  else
    console.log 'Sign-in state: ', authResult['error']

define (require) ->
  $    = require('jquery')
  gapi = require('gapi')

  requestSession = () -> $.ajax
    accepts: "json"
    type: "PUT"
    url: "/api/session"

  requestSession().then (res) ->

    gapi.signin.render "signinButton",
      clientid: res.clientId
      scope: "profile"
      cookiepolicy: "single_host_origin"
      callback: "signinCallback"
