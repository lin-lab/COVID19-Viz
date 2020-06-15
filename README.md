# COVID19-Viz

RShiny visualization of COVID-19 effective reproduction number (Rt). Based on
data from the Johns Hopkins University Center for Systems Science and
Engineering (JHU-CSSE) [Coronavirus Resource
Center](https://github.com/CSSEGISandData/COVID-19). Rt calculated using
[EpiEstim](https://cran.r-project.org/package=EpiEstim) ([Cori, A., et al.,
2013](https://doi.org/10.1093/aje/kwt133).

## How to Run the app

If you downloaded the app locally, have all the dependencies, and wish to run
it, simply open the `app.R` file in RStudio and click the `Run App` button.

## Download our Data

Please see the data files in the `clean_data` folder for the cleaned data that
was displayed on the website.

## Code Structure

+ The code to download the data is in `01_download_rt.sh`.
+ The code to clean the Rt data and merge it with the shapefiles is in
  `02_clean_data.R`. This code generates some `rds` files that are stored in
  `clean_data`.
+ The RShiny app code is in `app.R`.
+ Running the `prep_data.sh` bash file will run `01_download_rt.sh` and
  `02_clean_data.R`. You only need to do this if you want to reproduce the data
  cleaning and merging steps.

## Build Docker

`docker build -t local-covid-rt`

`docker run -p 8080:8080 -e PORT=8080 local-covid-rt`

Open web browser to [http://localhost:8080](http://localhost:8080).

## Instructions for Deploying on Heroku with HMDC

See [here](https://hmdc.gitbook.io/r/).
