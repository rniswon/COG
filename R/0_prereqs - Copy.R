#  ------------------------------------------------------------------------#
# run prerequisites----
#  ------------------------------------------------------------------------#


# set paths for remote inputs and outputs
input_data_raw <- "D:/OneDrive - DOI/COG/data-raw"
output_results <- "D:/OneDrive - DOI/COG/results"

# load packages (install if necessary)
library(arrow)
library(foreach)
library(doParallel)
library(fable)
library(tsibble)
library(feasts)
library(lubridate)
library(scales)
library(ggplot2)
library(plyr)
library(stringr)
library(readr)
library(purrr)
library(broom)
library(tidyr)
library(tibble)
library(dplyr)

# set seed for reproducibility
.seed <- 42
