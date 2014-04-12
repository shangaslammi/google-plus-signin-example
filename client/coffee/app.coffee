
require.config
  baseUrl: "js/modules"
  paths:
    'jquery': '../vendor/jquery/jquery'
    'knockout': '../vendor/knockout.js/knockout'

window.signinCallback = (authResult) ->
  if (authResult['status']['signed_in'])
    document.getElementById('signinButton').setAttribute('style', 'display: none')
  else
    console.log 'Sign-in state: ', authResult['error']

define (require) ->
  gapi.signin.render("signInButton")
