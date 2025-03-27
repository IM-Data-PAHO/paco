#' Fetch a Table from a public WIISEMart endpoint
#'
#' This function retrieves a specified table from WIISE and returns it as a data frame.
#'
#' @param api Character. A string specifying the base URL of the API endpoint.
#' @param table_name Character. A string specifying the name of the table to fetch from the API.
#' @param min_year Numeric (optional). A value specifying the minimum year of the data to fetch.
#' @param max_year Numeric (optional). A value specifying the maximum year of the data to fetch.
#' @param filters List (optional). A named list where each name is an additional column to filter by and each value is a character vector of values to include.
#'
#' @return A data frame containing the data from the specified table.
#' 
#' @examples
#' \dontrun{
#' fetch_from_WIISE(
#'   api = "the_API_link",
#'   table_name = "your_table_name",
#'   min_year = 2022,
#'   max_year = 2023,
#'   filters = list(
#'     COUNTRY = c("COUNTRY_1", "COUNTRY_2"),
#'     INDICATOR = c("INDICATOR_1", "INDICATOR_2")
#'   )
#' )
#' }
#' 
#' @import rio
#' @import utils
#' 
#' @export
fetch_from_WIISE <- function(api, table_name, min_year = NULL, max_year = NULL, filters = list()) {
  
  # construct base URL
  base_url <- paste0(api, "/WIISE/", table_name) 
  
  # construct filter conditions
  filter_conditions <- c()
  
  # add year filters
  if (!is.null(min_year)) {
    filter_conditions <- c(filter_conditions, paste0("YEAR%20ge%20", min_year))
  }
  
  if (!is.null(max_year)) {
    filter_conditions <- c(filter_conditions, paste0("YEAR%20le%20", max_year))
  }
  
  # add conditions from filters list
  if (length(filters) > 0) {
    for (col in names(filters)) {
      values <- filters[[col]]
      if (length(values) > 0) {
        encoded_values <- paste(sprintf("%%27%s%%27", values), collapse = ",")
        condition <- paste0(col, "%20in%20(", encoded_values, ")")
        filter_conditions <- c(filter_conditions, condition)
      }
    }
  }
  
  # combine filter conditions with ' and '
  if (length(filter_conditions) > 0) {
    filter_query <- paste(filter_conditions, collapse = "%20and%20")
    filter_encoded <- URLencode(filter_query, reserved = TRUE)
  } else {
    filter_encoded <- NULL
  }
  
  # construct query parameters
  query_params <- list(`$format` = "csv")
  if (!is.null(filter_encoded)) {
    query_params[["$filter"]] <- filter_encoded
  }
  
  # construct URL with query parameters
  query_string <- paste0(names(query_params), "=", query_params, collapse = "&")
  url <- paste0(base_url, "?", query_string)
  
  # fetch table from URL
  table <- import(url, format = "csv")
  return(table)
}
