#' Fetch a Table from a public WIISEMart endpoint
#'
#' This function retrieves a specified table from WIISE and returns it as a data frame.
#'
#' @param api A character string specifying the base URL of the API endpoint.
#' @param table_name A character string specifying the name of the table to fetch from the API.
#' @param min_year A numeric value specifying the minimum year of the data to fetch.
#' @param max_year A numeric value specifying the maximum year of the data to fetch.
#'
#' @return A data frame containing the data from the specified table.
#' 
#' @import rio
#' @import utils
#' 
#' @export
fetch_from_WIISE <- function(api, table_name, min_year = NULL, max_year = NULL) {
  
  # construct base URL
  base_url <- paste0(api, "/WIISE/", table_name) 
  
  # construct filter conditions
  filter_conditions <- c()
  
  if (!is.null(min_year)) {
    filter_conditions <- c(filter_conditions, paste0("YEAR ge ", min_year))
  }
  
  if (!is.null(max_year)) {
    filter_conditions <- c(filter_conditions, paste0("YEAR le ", max_year))
  }
  
  # combine filter conditions with ' and '
  if (length(filter_conditions) > 0) {
    filter_query <- paste(filter_conditions, collapse = " and ")
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
  
  # TODO: check if valid URL
  
  # fetch table from URL
  table <- import(url, format = "csv")
  return(table)
}