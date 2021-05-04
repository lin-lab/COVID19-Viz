# COVID-19 Spread Mapper

RShiny visualization of COVID-19 effective reproduction number (Rt). Based on
data from the Johns Hopkins University Center for Systems Science and
Engineering (JHU-CSSE) [Coronavirus Resource
Center](https://github.com/CSSEGISandData/COVID-19).

Rt calculation method described in [our preprint](https://www.medrxiv.org/content/10.1101/2021.03.12.21253496v1).

Code to calculate Rt is located in [our COVID19-Rt Github repo](https://github.com/lin-lab/COVID19-Rt),
and data preprocessing code is located in the [COVID-data-cleaning repo](https://github.com/lin-lab/COVID-data-cleaning).

## How to Run the app

If you downloaded the app locally, have all the dependencies, and wish to run
it, simply open the `app.R` file in RStudio and click the `Run App` button.

## Download our Data

Please see the data files in the `clean_data_pois` folder for the cleaned data that
was displayed on the website.

The `clean_data` folder has Rt estimates that were generated using
[EpiEstim](https://cran.r-project.org/package=EpiEstim) ([Cori, A., et al.,
2013](https://doi.org/10.1093/aje/kwt133)). These files are not used anymore for
our site but are provided for archival purposes.


## Code Structure

### Main Code

+ The code to download the data is in `01_download_rt.sh`.
+ The code to clean the Rt data and merge it with the shapefiles is in
  `02_clean_data.R`. This code generates some `rds` files that are stored in
  `clean_data_pois`.
+ The RShiny app code is in `app.R`.
+ Running the `prep_data.sh` bash file will run `01_download_rt.sh`,
  `02_clean_data.R`, and `03_upload_to_aws.sh`. You only need to do this if you
  want to reproduce the data cleaning and merging steps. The third step will not
  work because you aren't authorized to upload to our AWS folder.

### Auxiliary Code

+ `04_minify_js.sh` contains a script to minimize our custom javascript code.
  Running this produces a minified javascript file, which speeds up loading time
  of the webpage.
+ `assets`: Miscellaneous images, html, and static markdown pages and includes
  in our site.
+ `src`: Folder containing auxiliary R code.
+ `www`: Folder containing javascript code.
+ `raw_data`: Folder where the downloaded data from `01_download_rt.sh` goes.

## Build Docker

This builds a Docker container that mimics how the site is deployed in practice.

`docker build -t local-covid-rt`

`docker run -p 8080:8080 -e PORT=8080 local-covid-rt`

Open web browser to [http://localhost:8080](http://localhost:8080).

## Instructions for Deploying on Heroku with HMDC

The website is deployed via Heroku. See [here](https://hmdc.gitbook.io/r/) for
instructions on how to set it up and for more info.
