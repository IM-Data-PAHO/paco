#' Initialize a new RStudio project with a particular directory structure.
#'
#' @param project_name The name of the project (used in the README.txt and .Rproj file)
#' @param path The path where the project directory should be created
#' @return NULL
#' @export
init <- function(project_name, path = ".") {
  # Create the main project directory
  dir.create(file.path(path, project_name), showWarnings = FALSE)
  
  # Define the folder structure
  folders <- c("scripts", "output", "docs", "data", "styles")
  
  # Create the folders
  for (folder in folders) {
    dir.create(file.path(path, project_name, folder), showWarnings = FALSE)
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
  writeLines(readme_content, file.path(path, project_name, "README.txt"))
  
  # Copy the requirements.R template
  requirements_template <- file.path(template_dir, "requirements_template.R")
  file.copy(requirements_template, file.path(path, project_name, "requirements.R"))
  
  # Copy the PAHO_styles.R template
  PAHO_styles_template <- file.path(template_dir, "PAHO_styles_template.R")
  file.copy(PAHO_styles_template, file.path(path, project_name, "/styles/PAHO_styles.R"))
  
  # Copy and rename the .Rproj template
  rproj_template <- file.path(template_dir, "RStudio_project_template.Rproj")
  rproj_destination <- file.path(path, project_name, paste0(project_name, ".Rproj"))
  file.copy(rproj_template, rproj_destination)
  
  message("Project directory initialized successfully.")
  invisible(NULL)
}
