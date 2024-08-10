# requirements.R ---------------------------------------------------------------
# Description: Package requirements for the project.
# ______________________________________________________________________________

# load pacman if necessary
if (!require('pacman')) install.packages('pacman'); library('pacman')

# load other libraries with pacman
pacman::p_load(
  paco,
  tidyverse,
  rio,
  ggplot2
)
