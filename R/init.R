#' Create a directory structure according to PAHO/CIM standards.
#'
#' @param project_name The name of the project (used in the \code{README.txt} and \code{project_name.Rproj} files). Defaults to "Insert project name here".
#' @param path The path where the project directory should be created. Defaults to current working directory.
#' @param rproj Whether to create a \code{project_name.Rproj} file. Default is \code{FALSE}. When in use, requires an explicit \code{project_name}.
#' @return NULL
#' @export
init <- function(project_name = "Insert project name here", path = ".", rproj = FALSE) {
  if(rproj && missing(project_name)) {
    stop("Provide project_name when rproj = TRUE.", call. = FALSE)
  }

  # Define the folder structure
  folders <- c("scripts", "output", "docs", "data", "styles")
  
  # Create the folders
  for(folder in folders) {
    dir.create(file.path(path, folder), showWarnings = FALSE)
  }
  
  # Path to the templates
  template_dir <- system.file("templates", package = "paco")
  
  if(template_dir == "") {
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
  file.copy(PAHO_styles_template, file.path(path, "styles", "PAHO_styles.R"))

  # Copy the RStudio_project_template.Rproj template
  if(rproj) {
    RStudio_project_template <- file.path(template_dir, "RStudio_project_template.Rproj")
    file.copy(RStudio_project_template, file.path(path, paste0(project_name, ".Rproj")))
  }
  
  message("Directory initialized successfully.")
  invisible(NULL)
}
