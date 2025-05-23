% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_script.R
\name{create_script}
\alias{create_script}
\title{Create an R script following the PAHO/CIM standard.}
\usage{
create_script(
  script_name,
  path = "./scripts",
  author = "Insert author name here",
  email = "Insert author e-mail here"
)
}
\arguments{
\item{script_name}{The name of the script (without .R extension).}

\item{path}{The path where the script should be created. Defaults to "scripts" folder in the current working directory.}

\item{author}{The author's name. Defaults to "Insert author name here".}

\item{email}{The author's email. Defaults to "Insert author e-mail here".}
}
\description{
Create an R script following the PAHO/CIM standard.
}
