# COVID19-Viz

RShiny visualization of COVID-19 Rt.

## How to run the App

If you downloaded the app locally, have all the dependencies, and wish to run
it, simply open the `app.R` file in RStudio and click the `Run App` button.

## Structure of the App

The main code for the website is in `app.R`. There are some static files in the
`assets` folder. The code to clean the Rt data and merge it with the shapefiles
for the maps is in `clean_data.R`. The resultant `rds` files are stored in
`clean_data`.

## Build Docker

`docker build -t local-covid-rt`

`docker run -p 8080:8080 -e PORT=8080 local-covid-rt`

Open web browser to [http://localhost:8080](http://localhost:8080).

## Instructions for Deploying on Heroku with HMDC

See [here](https://hmdc.gitbook.io/r/).
