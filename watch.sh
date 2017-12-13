#!/bin/bash

pug --watch --out templates/dist templates/ & \
    node-sass ./scss/app.scss ./static/app.css --watch & \
    CompileDaemon -build="elm-make elm/AddTag.elm --output=static/js/elm.js" -command="echo ok" -exclude=".#*" -exclude-dir="vendor" -exclude-dir=".git" -exclude-dir="public" -exclude-dir="images" -pattern="elm/.+elm"
