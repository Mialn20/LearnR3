
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




#---- Importing function 3 ----

#' importing csv
#'
#' @param folder_path path to the folder containing the csv files
#'
#' @returns data.frame of all the csv files in the folder rbinded together
import_csv <- function(folder_path){

  data_files <- here::here(folder_path) |>
    fs::dir_ls(glob = "*.csv")

  return_data <- data_files |>
    purrr::map(import_dime) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(return_data)
}



#' get_participant_id from a dataset
#'
#' @param Import_file data.frame containing the dataset from import_csv
#'
#' @returns

get_participant_id <- function(Import_file){
  return_data <- Import_file |>
    dplyr::mutate(
      id = stringr::str_extract(file_path_id,"[:digit:]+\\.csv$",) |>
        stringr::str_remove("\\.csv$") |>
        as.integer(),
      .before = file_path_id
    ) |>
    select(-file_path_id)

  return(return_data)
}



#' preparing the dates
#' @param data data.frame containing the dataset from import_csv
#' @param column name of the column containing the date

prepare_dates <- function(data,column){
  data_output <- data |>
    dplyr::mutate(
      date = lubridate::as_date({{column}}),
      hour = lubridate::hour({{column}}),
      .before = {{column}}
    ) |>
    dplyr::select(-{{column}})

  return(data_output)
}




## cleaning functions

#' Cleans CGM data
#'
#' @param data to clean
#'
#' @returns clean data
clean_cgm <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    dplyr::rename(glucose = historic_glucose_mmol_l) |>
    prepare_dates(device_timestamp)
  return(cleaned)
}

#' Cleans sleep data
#'
#' @param data clean
#'
#' @returns clean data

clean_sleep <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    dplyr::rename(datetime = date) |>
    prepare_dates(datetime)
  return(cleaned)
}
