#  ------------------------------------------------------------------------#
# run prerequisites----
#  ------------------------------------------------------------------------#


# load packages (install if necessary)
library(svglite)
library(magrittr)
library(foreach)
library(doParallel)
library(caret)
library(quantreg)
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
