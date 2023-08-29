#!/usr/bin/env bash

elm-live src/Minesweeper.elm --hot --host=localhost --dir=dist -- --output=dist/build.js --debug
