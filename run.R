# obtain port argument (if given)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
        port <- "12000"
} else if (length(args) == 1) {
  port <- args[1]
}

print(paste("Listening on port", port))

shiny::runApp(
  appDir = getwd(),
  host = "0.0.0.0",
  port = as.numeric(port)
)
