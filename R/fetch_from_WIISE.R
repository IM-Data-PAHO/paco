#' Fetch a Table from a public WIISEMart endpoint
#'
#' This function retrieves a specified table from WIISE and returns it as a data frame.
#'
#' @param api Character. A string specifying the base URL of the API endpoint.
#' @param table_name Character. A string specifying the name of the table to fetch from the API.
#' @param min_year Numeric (optional). A value specifying the minimum year of the data to fetch.
#' @param max_year Numeric (optional). A value specifying the maximum year of the data to fetch.
#' @param filters List (optional). A named list where each name is an additional column to filter by and each value is a character vector of values to include.
#' @param method Character (optional). Either "get" or "post". Defaults to "get" and automatically changes to "post" if URL size surpasses 2048 characters.
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
#'   ),
#'   method = "get"
#' )
#' }
#' 
#' @import rio
#' @import httr
#' @import dplyr
#' @import jsonlite
#' 
#' @export
fetch_from_WIISE <- function(api, table_name, min_year = NULL, max_year = NULL,
                             filters = list(), method = "get") {
  
  # construct base URL
  base_url <- paste0(api, "/WIISE/", table_name)
  
  # Helper: build $filter clause
  build_filter_clause <- function() {
    clauses <- character()
    if(!is.null(min_year)) clauses <- c(clauses, paste0("YEAR ge ", min_year))
    if(!is.null(max_year)) clauses <- c(clauses, paste0("YEAR le ", max_year))
    for(col in names(filters)) {
      vals <- paste0("'", filters[[col]], "'", collapse = ",")
      clauses <- c(clauses, paste0(col, " in (", vals, ")"))
    }
    if(length(clauses) > 0) paste(clauses, collapse = " and ") else NULL
  }
  
  filter_str <- build_filter_clause()
  
  # === Method Selection ===
  if(method == "get") {
    query <- list(`$format` = "csv")
    if (!is.null(filter_str)) query[["$filter"]] <- URLencode(filter_str, reserved = TRUE)
    query_str <- paste0(names(query), "=", query, collapse = "&")
    url <- paste0(base_url, "?", query_str)
    
    if(method == "get" && nchar(url) > 2048) {
      cat("URL exceeds 2048 characters, switching to POST request.")
      method <- "post"
    } else {
      return(import(url, format = "csv"))
    }
  }
  
  if(method == "post") {
    post_url <- paste0(base_url, "/$query")
    body_str <- if (!is.null(filter_str)) paste0("$filter=", filter_str) else ""
    res <- POST(
      url = post_url,
      add_headers("Content-Type" = "text/plain"),
      body = body_str,
      encode = "raw"
    )
    res_json <- jsonlite::fromJSON(httr::content(res, as = "text"), flatten = TRUE)
    
    # convert to data frame
    df <- as.data.frame(res_json$value) %>%
      mutate(across(where(is.character), ~ na_if(.x, "NA")))
    
    return(df)
  }
  
  stop("Invalid method, must be 'get' or 'post'.")
}