
import_function <- function(folder_path,format){
  data_files <- here::here(folder_path) |>
    fs::dir_ls(regexp = paste0(format,"\\.parquet"),recurse = TRUE)

  return_data <- data_files |>
    purrr::map(arrow::read_parquet) |>
    purrr::list_rbind(names_to = "file_path_id") |>
    dplyr::slice_head(n = 100)
  return(return_data)
}


get_participant_id <- function(Import_file){
  return_data <- Import_file |>
    dplyr::mutate(
      id = stringr::str_extract(file_path_id,"[:digit:]+\\_[:digit:]") |>
        stringr::str_remove("\\_[:digit:]$") |>
        as.integer(),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)

  return(return_data)
}


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


join_all_full <- function(data_list) {
  purrr::reduce(data_list, full_join)
}
