#' Create a directory structure according to PAHO/CIM standards.
#'
#' @param project_name The name of the project (used in the README.txt file). Defaults to "Insert project name here".
#' @param path The path where the project directory should be created. Defaults to current working directory.
#' @return NULL
#' @export
init <- function(project_name = "Insert project name here", path = ".") {
  # Define the folder structure
  folders <- c("scripts", "output", "docs", "data", "styles")
  
  # Create the folders
  for (folder in folders) {
    dir.create(file.path(path, folder), showWarnings = FALSE)
  }
  
  # Path to the templates
  template_dir <- system.file("templates", package = "paco")
  
  if (template_dir == "") {
    stop("Template directory not found. Make sure the package is installed correctly.")
  }
  
  # Copy and modify the README.txt
  readme_template <- file.path(template_dir, "README_template.txt")
  readme_content <- readLines(readme_template)
  readme_content <- gsub("<project_name>", project_name, readme_content)
  readme_content <- gsub("<creation_date>", Sys.Date(), readme_content)
  writeLines(readme_content, file.path(path, "README.txt"))
  
  # Copy the requirements.R template
  requirements_template <- file.path(template_dir, "requirements_template.R")
  file.copy(requirements_template, file.path(path, "requirements.R"))
  
  # Copy the PAHO_styles.R template
  PAHO_styles_template <- file.path(template_dir, "PAHO_styles_template.R")
  file.copy(PAHO_styles_template, file.path(path, "/styles/PAHO_styles.R"))
  
  message("Directory initialized successfully.")
  invisible(NULL)
}
