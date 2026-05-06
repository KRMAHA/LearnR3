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
