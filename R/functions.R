#' Read in one nurses´ stress data file
#'
#' @param file_path path to the data file
#' @param max_rows maximum number of rows to read
#'
#' @returns outputs a data frame/tibble
#'
read <- function(file_path, max_rows = 10) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}




#' Map function
#'
#' @param filename
#'
#' @returns all the datafiles combined together
#'
read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    purrr::map(read) |>
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}




#' Creating function for getting ID
#'
#' @returns data with id
#'
get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        pattern = "/stress/[:alnum:]{2}/"
      ) |>
        stringr::str_remove("/stress/") |>
        stringr::str_remove("/"),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}




#'Making a summerise function
#'
#' @param data
#'
#' @returns summerise in collums

#'
summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}




#' Tidy up the dates in the survey results
#'
#' @param data
#'
#' @returns tidyed dataset
#'
tidy_survey_dates <- function(data) {
  tidied <- data |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = as_datetime(paste(date, start_time)),
      end_datetime = as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) |>
    dplyr::select(-c(date, start_time, end_time, duration))
  return(tidied)
}


#' Maked the dataset longer
#'
#' @param data
#'
#' @returns longer dataset
#'
survey_to_long <- function(data) {
  longer <- data |>
    dplyr::select(id, datetime_id, start_datetime, end_datetime) |>
    tidyr::pivot_longer(c(start_datetime, end_datetime), names_to = NULL, values_to = "collection_datetime") |>
    dplyr::group_by(pick(-collection_datetime)) |>
    tidyr::complete(collection_datetime = seq(min(collection_datetime),
                                              max(collection_datetime),
                                              by = 60
    )) |>
    ungroup()
  return(longer)
}

