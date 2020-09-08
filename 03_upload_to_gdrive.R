library(googledrive)

# upload current data
upload_res <- drive_upload("clean_data/rt_table_export.csv",
                           path = "COVID19 Website Data Tables/",
                           overwrite = TRUE)
upload_res

# upload current time
cur_time <- Sys.time()
last_updated <- sprintf("Last updated: %s", format(cur_time, usetz = TRUE))
write(last_updated, "clean_data/last_updated.txt")
upload_time <- drive_upload("clean_data/last_updated.txt",
                            path = "COVID19 Website Data Tables/",
                            overwrite = TRUE)
upload_time
