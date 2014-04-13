# Google+ Sign-in Example using Haskell and Scotty

You need [node.js](http://nodejs.org/) for compiling the client-side assets and
somewhat recent versions of [GHC](https://www.haskell.org/ghc/) and
[`cabal`](http://www.haskell.org/cabal/) to run the Haskell server.

In order to run the example yourself you need to register an application in
the [Google Developer Console](https://developers.google.com/console), enable
Google+ API access and create a new client id in the Credentials section of your
new application.

Next, create a `key.json` file in the project's root folder using the
`key.sample.json` file as a template and fill in the client id and client secret
from your Google project.

To install all library dependencies, run

    npm install

To start the server run

    npm start

By default the test page will be accessible at https://localhost:3000/ .
