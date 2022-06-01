#############################################################
# Essential R packages: RAS module #
#############################################################

# This script uses the p_load() function from pacman R package, 
# which installs if package is absent, and loads for use if already installed


# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman")


# Packages available from CRAN
##############################
pacman::p_load(
  
  # Project and file management
  #############################
  here,      # file paths relative to R project root folder
  rio,       # import/export of many types of data
  openxlsx,  # import/export of multi-sheet Excel workbooks
  rmarkdown, # reading and printing RAS R markdown guide
  knitr,     # reading and printing RAS R markdown guide
  
  # Package install and management
  ################################
  pacman,   # package install/load
  remotes,  # install from github
  
  # General data management
  #########################
  tidyverse,   # includes many packages for tidy data wrangling:
  #dplyr,      # data management
  #tidyr,      # data management
  #ggplot2,    # data visualization
  #stringr,    # work with strings and characters
  #forcats,    # work with factors 
  #lubridate,  # work with dates
  #purrr       # iteration and working with lists
  janitor,     # data cleaning
  plotly,      # Interactively inspecting exploratory graphs
  
  # Exploratory graphs:
  #########################
  metR,         # Filled contours used with ggplot2
  gridExtra,	 # Plotting ggplot2 graphs side by side
 
  
  # Summary tables:
  #########################
  flextable,   # Creating printable summary tables
  officer,     # Fine tuning flextables
  DT, 			   # Interactive tables
  broom,       # Extracting summary information from models
  gtsummary,   # making descriptive and statistical tables
  rstatix,     # quickly run statistical tests and summaries
  scales,      # helper functions
  
  # RAS analysis:
  #########################
  # Package functions will be explained in the guide
  srvyr,
  epikit,
  Hmisc,
  survey,
  gmodels,
  epiR, 
  multcomp,
  leaps,
  car,
  lme4,
  lmerTest,
  performance,
  optimx,
  randomNames

)


# Additional package for mixed effects modelling:
remotes::install_github("goodekat/redres")

#####################################################
