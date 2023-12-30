
##load permanent df to store all submitted samples
##Only has to be performed once
pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

#Create df for data entry into sql
participants_df <- data.frame(title = character(),
                              first_name = character(),
                              last_name = character(),
                              job_title = character(),
                              location = character(),
                              email = character(),
                              picture = character(),
                              note = character(),
                              date = character(),
                              image_ext = character(),
                              row_id = character(),
                              stringsAsFactors = FALSE)

dbWriteTable(pool, "participants_df", participants_df, temporary = FALSE, overwrite = FALSE)
data <- dbReadTable(pool, "participants_df")

##################Load data in sql database from Excel#################

# add a unique row id
unique_id <- function(data){
    replicate(nrow(data), UUIDgenerate())
}

participants_data <- read_excel("participant_directory_df.xlsx")
participants_data$date <- format(Sys.Date())
participants_data$row_id <- unique_id(participants_data)
query <- sqlAppendTable(pool, "participants_df", participants_data, row.names = FALSE)
dbExecute(pool, query)
