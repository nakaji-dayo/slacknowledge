#!/bin/bash

set -e

service mysql start

export MYSQL_HOST=127.0.0.1
export MYSQL_USER=root
export MYSQL_PASSWORD=root

echo "create database slacknowledge" | mysql -u root -proot -h localhost
migrate -database "mysql://root:root@tcp(localhost:3306)/slacknowledge" -path migrations/ up

pug --out templates/dist templates/

stack setup --resolver=lts-9.14 --allow-different-user
stack build --allow-different-user

mkdir -p dist
cp `stack exec which slacknowledge-api` dist/slacknowledge-api
cp `stack exec which slacknowledge-bot` dist/slacknowledge-bot
ls -l dist
pwd
