
#---- Importing function ----
#' General Import function
#'
#' @param file_path File path to raw data
#' @param n Number of rows to import
#'
#' @returns data.frame of n rows of the raw data
import_dime <- function(file_path,n=100) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = n)
  return(data)
}



#---- Importing function 2 ----
#' More general import function
#'
#' @param type
#' @param patient
#' @param n
#'
#' @returns data.frame of n rows of the raw data
import_data_snake <- function(type_sort,id,n=50) {
  data <- here::here(paste0("data-raw/dime/",as.character(type_sort),"/",as.character(id),".csv")) |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = as.numeric(n))
  return(data)
}
