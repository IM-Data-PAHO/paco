#' Create an R script following the PAHO/CIM standard.
#'
#' @param script_name The name of the script (without .R extension).
#' @param path The path where the script should be created. Defaults to "scripts" folder in the current working directory.
#' @param author The author's name. Defaults to "Insert author name here".
#' @param email The author's email. Defaults to "Insert author e-mail here".
#' @return NULL
#' @export
create_script <- function(script_name, 
                          path = "./scripts", 
                          author = "Insert author name here", 
                          email = "Insert author e-mail here") {
  # ensure script name is valid
  if (missing(script_name) || script_name == "") {
    stop("You must provide a script name.")
  }
  
  # path to the templates
  template_dir <- system.file("templates", package = "paco")
  
  if(template_dir == "") {
    stop("Template directory not found. Make sure the package is installed correctly.")
  }
  
  # create the directory if it doesn't exist yet
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  # calculate dashes to maintain alignment
  script_header <- paste0("# ", script_name, ".R ")
  total_width <- 80 
  dash_count <- max(1, total_width - nchar(script_header))  # At least one dash
  dash_line <- paste0(script_header, strrep("-", dash_count))
  
  # copy and modify script template
  script_template <- file.path(template_dir, "script_template.txt")
  script_content <- readLines(script_template)
  script_content <- gsub("<file_header>", dash_line, script_content)
  script_content <- gsub("<author>", author, script_content)
  script_content <- gsub("<email>", email, script_content)
  script_content <- gsub("<creation_date>", Sys.Date(), script_content)
  
  # define script file path
  script_file <- file.path(path, paste0(script_name, ".R"))
  
  # save script
  writeLines(script_content, script_file)
  
  message("Script created successfully at: ", script_file)
  invisible(NULL)
}
