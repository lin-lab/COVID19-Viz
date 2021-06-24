library(RSocrata)
library(optparse)

option_list <- list(
  make_option(c("-o", "--output"), default = "data.rds",
              help = "Output location for rds file; [Default: %default]")
)

opts <- parse_args(OptionParser(option_list = option_list))

APP_TOKEN <- Sys.getenv("RSOCRATA_TOKEN")
cat("Reading county-level vaccine data from socrata...\n")
county_vax_orig <- read.socrata("https://data.cdc.gov/resource/8xkx-amqh.json",
                                app_token = APP_TOKEN)
outdir <- dirname(opts$output)
if (!dir.exists(outdir)) {
  dir.create(outdir)
}
saveRDS(county_vax_orig, opts$output)
cat("Done reading vaccination data!\n")
