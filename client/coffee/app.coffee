
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
  gapi = require('gapi')

  console.log gapi

  gapi.signin.render "signinButton",
    clientid: "764745625437-2b2lo6jusqkoc0lboumg50ned3tbqi0g.apps.googleusercontent.com"
    scope: "profile"
    cookiepolicy: "single_host_origin"
    callback: "signinCallback"
