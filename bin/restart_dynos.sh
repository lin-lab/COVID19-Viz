#!/bin/bash

APP_ID_OR_NAME=hsph-covid-study
curl -n -X DELETE https://api.heroku.com/apps/$APP_ID_OR_NAME/dynos \
    -H "Authorization: Bearer ${HEROKU_API_KEY}" \
    -H "Content-Type: application/json" \
    -H "Accept: application/vnd.heroku+json; version=3"
