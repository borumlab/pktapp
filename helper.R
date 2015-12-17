# mandatory fields in the form
fields_mandatory <- c(
  "NAME"
)

# all fields in the form we want to save
fields_all <- c(
  "NAME",
  "NUMBER_OF_DAYS",
  "TYPE_OF_INFO",
  "COMMENTS"
)

# get current Epoch time
get_time_epoch <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

get_save_fxn <- function(type) {
  fxn <- sprintf("save_data_%s", type)
  stopifnot(existsFunction(fxn))
  fxn
}
save_data <- function(data, type) {
  fxn <- get_save_fxn(type)
  do.call(fxn, list(data))
}

# decide which function to use to load based on storage type
get_load_fxn <- function(type) {
  fxn <- sprintf("load_data_%s", type)
  stopifnot(existsFunction(fxn))
  fxn
}
load_data <- function(type) {
  fxn <- get_load_fxn(type)
  data <- do.call(fxn, list())
  
  # Just for a nicer UI, if there is no data, construct an empty
  # dataframe so that the colnames will still be shown
  if (nrow(data) == 0) {
    data <-
      matrix(nrow = 0, ncol = length(fields_all),
             dimnames = list(list(), fields_all)) %>%
      data.frame
  }
  data %>% dplyr::arrange(desc(timestamp))
}

#### Method 1: Local text files ####

results_dir <- "responses"
save_data_flatfile <- function(data) {
  data <- t(data)
  file_name <- paste0(
    paste(
      get_time_human(),
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
  
  # write out the results
  write.csv(x = data, file = file.path(results_dir, file_name),
            row.names = FALSE, quote = TRUE)
}
load_data_flatfile <- function() {
  files <- list.files(file.path(results_dir), full.names = TRUE)
  data <-
    lapply(files, read.csv, stringsAsFactors = FALSE) %>%
    do.call(rbind, .)
  data
}
