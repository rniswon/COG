#  ------------------------------------------------------------------------#
# analyze water use associated with continuous oil and gas (COG) development----
#  ------------------------------------------------------------------------#


# read code with prerequisites
source("Code/0prereqs.R")

# read code for wrangling, modeling and visualizing
source("Code/1wrangle.R")
source("Code/2model.R")
source("Code/3visualize.R")

# create directories for raw data, data and product
if (!dir.exists("Raw")) dir.create("Raw")
if (!dir.exists("Data")) dir.create("Data")
if (!dir.exists("Product")) dir.create("Product")



#  ------------------------------------------------------------------------#
# initialize process for water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) the water-use analyses make confidence intervals around the parameter estimates
#   2) an appropriate confidence level must be chosen

# set confidence level
.conlev <- 0.95



#  ------------------------------------------------------------------------#
# input to water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) to run the following code chunks (including sections B.1. and C.1.) as is, the data must be structured as follows:
#     a) rows of observations for individual oil and gas wells;
#     b) columns of variables for subwatershed, yearmonth and volume;
#     c) see "well_fracking_water_reported.csv" files for the appropriate data structure
#   2) the script may throw an error if data have the following limitations:
#     a) missing one or more of the listed variables;
#     b) lacking observations by individual well
#   3) the script can be modified as appropriate to run in the following conditions:
#     a) a COG play may have data available at a finer temporal resolution;
#     b) a COG play likely will not have data available on water used in cementing or drilling of oil and gas wells

# read data for water used for fracking, cementing and drilling of oil and gas wells; change file names as appropriate
datf <- read_rds("Data/well_fracking_direct_water_reported.rds")
datc <- read_rds("Data/well_cementing_direct_water_estimated.rds")
datd <- read_rds("Data/well_drilling_direct_water_estimated.rds")
dati <- read_rds("Data/well_indirect_water_estimated.rds")

## NOTE:
#   1) the following function is also returned as a ".rds" file in the "Data" folder

# wrangle available water-use data to necessary structure for modeling; list available data in "data" argument
dat <- .wrangle(data = list(datf, datc, datd, dati))
rm(datf, datc, datd, dati)

print("input to water-use analysis done")



#  ------------------------------------------------------------------------#
# process for water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) the following function is also returned as a ".rds" file in the "Product" folder

# model mean and 5th, 50th and 95th percentile water use for uses as a function of the number of wells using linear and quantile regression with leave-one-out cross-validation
dat <- read_rds("Data/dat.rds")
mod <- .model(data = dat,
              parm = c("Mean", "P50", "P5", "P95"),
              conlev = .conlev)

print("process for water-use analysis done")



#  ------------------------------------------------------------------------#
# output from water-use analysis----
#  ------------------------------------------------------------------------#


# write and plot data on water use associated with COG development
mod <- read_rds("Product/mod.rds")
.visualize(data = mod)

## NOTE:
#   1) the data are structured as follows:
#     a) rows of estimated water-use coefficients by use;
#     b) columns of variables for parameter estimates and confidence intervals by parameter
#   2) the data are plotted as water use against number of wells (grouped by yearmonth and subwatershed)
#   3) the preceding function is only returned as ".csv" and ".svg" files in the "Product" folder (not as an object in the environment)

print("output from water-use analysis done")



#  ------------------------------------------------------------------------#
# output from water-use analysis for data release----
#  ------------------------------------------------------------------------#


# write data on predictions and coefficients of water use associated with COG development
pred <- read_csv("Product/predictions.csv")
coef <- read_csv("Product/coefficients.csv")
.visualize_data_release(data = pred,
                        data2 = coef)

## NOTE:
#   1) the data are structured as follows:
#     a) rows of estimated water-use predictions and coefficients by use;
#     b) columns of variables for parameter estimates and confidence intervals by parameter
#   2) the preceding function is returned as ".csv" files in the "Product" folder (not as an object in the environment)

print("output from water-use analysis for data release done")
