#############################################################
# Essential R packages: RAS module #
#############################################################

# This script uses the p_load() function from pacman R package, 
# which installs if package is absent, and loads for use if already installed

# Packages to install *before* running pacman pload():
#####################################################

# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman", quiet = TRUE)

# Ensure the package "remotes" is installed
if (!require("remotes")) install.packages("remotes", quiet = TRUE)


# Install and/or load required packages:
########################################
pacman::p_load(
  
  # Project and file management
  #############################
  here,      # file paths relative to R project root folder
  rio,       # import/export of many types of data
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
  matchmaker,  # data cleaning
  
  # Summary tables:
  #########################
  flextable,   # Creating printable summary tables
  officer,     # Fine tuning flextables
  gtsummary,   # making descriptive and statistical tables
  smd,         # Helper functions for gtsummary tables
  labelled,    # Labelling variables and values
  scales,      # helper functions
  Hmisc,       # Summary functions
  
  # Analysis:
  #########################
  pps,         # Probability proportional to size (PPS) sampling
  sampler,     # Calculate sample size
  srvyr,       # Create survey design and calculate vaccination coverage
  survey,      # Support for srvyr functions
  fixest,      # Multivariable models with multiple fixed effects
  broom.mixed  # Extract tidy table of mixed effects model results
)

# Check for and install Github packages:
pacman::p_load_gh(
  
  # Packages for survey analysis and mixed effects:
  #################################################
  "R4EPI/sitrep"  # Function wrappers for survey analysis
)

###########################################################################