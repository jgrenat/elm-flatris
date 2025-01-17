# Elm Flatris

## Fork by jgrenat

This game was forked during a video to add a new functionality: adding a new type of block – the bomb – that makes surronding blocks explode.

You can see the video there: https://www.youtube.com/watch?v=fXoFiW_nIgw

A playable version is available there: https://jgrenat.github.io/elm-flatris/

## Original README 

A [Flatris](https://github.com/skidding/flatris) clone in Elm.

[![Screenshot](elm-flatris.png)](https://unsoundscapes.itch.io/flatris)

Current demo can be seen [here](https://unsoundscapes.itch.io/flatris).

## Features

* works on both desktop and mobile
* renders the grid with `elm/svg`
* preserves the game state in `localStorage` using ports, just try to reload the page while playing!

## Instructions to run

1. Install elm [elm-lang.org/install](http://elm-lang.org/install)
2. Clone this repo and `cd` into it
3. Run `elm make src/Main.elm --output elm.js`
4. Open `index.html` in the browser
