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
# A.0. initialize process for water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) if data are available preceding and following COG development, then run the following code chunk;
#   2) if not, then an appropriate year that COG development began must be selected using judgment

# # model year that COG development began by estimating breakpoint in volume of water used; change file names as appropriate
# datfd = read_csv("Data/well_fracking_direct_water_reported.csv")
# .brkpt = .model_breakpoint(data=list(datfd))

## COMMENT:
#   a) judgment must be used to choose an appropriate year that COG development began;
#   b) plot the water-use data (volume against year) to verify that the selected year is reasonable

# set breakpoint year
.brkpt = 2010

## NOTE:
#   1) the water-use analyses (sections B.1., B.2. and B.3.) make confidence intervals around the parameter estimates
#   2) an appropriate confidence level must be chosen

# set confidence level
.conlev = 0.95


#####  ------------------------------------------------------------------------#
# A.1. input to direct water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) to run the following code chunks (including sections B.1. and C.1.) as is, the data must be structured as follows:
#     a) rows of observations for individual oil and gas wells;
#     b) columns of variables for state, county, year and volume;
#     c) see "well_fracking_water_reported.csv" files for the appropriate data structure
#   2) the script may throw an error if data have the following limitations:
#     a) missing one or more of the listed variables;
#     b) lacking observations by individual well (for example, observations may be aggregated by county or by unit area)
#   3) the script can be modified as appropriate to run in the following conditions:
#     a) a COG play may have data available at a finer temporal resolution;
#     b) a COG play likely will not have data available on water used in cementing or drilling of oil and gas wells

# read data for water used for fracking, cementing and drilling of oil and gas wells; change file names as appropriate
datfd = read_csv("Data/well_fracking_direct_water_reported.csv")
datcd = read_csv("Data/well_cementing_direct_water_estimated.csv")
datdd = read_csv("Data/well_drilling_direct_water_estimated.csv")

## NOTE:
#   1) the following function is also returned as a ".rds" file in the "Data" folder

# wrangle available direct water-use data to necessary structure for modeling; list available data in "data" argument
datdir = .wrangle_direct(data=list(datfd, datcd, datdd),
                         brkpt=.brkpt)

print("input to direct water-use analysis done")


#  ------------------------------------------------------------------------#
# A.2. input to indirect water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) to run the following code chunks (including sections B.2. and C.2.) as is, the data must be structured as follows:
#     a) rows of observations for water use reported or estimated by water permit, water right, county, etc.;
#     b) columns of variables for state, county, source (ground or surface), type (fresh or saline), use (water depot, etc.), relation to COG development (yes, no, maybe, NA, etc.), year and volume
#     c) see "state_water_reported.csv" for the appropriate data structure
#   2) the script may throw an error if data have the following limitations:
#     a) missing one or more of the listed variables;
#     b) lacking observations by individual well (for example, observations may be aggregated by county or by unit area)
#   3) the script can be modified as appropriate to run in the following conditions:
#     a) a COG play may have data available at a finer temporal resolution;
#     b) a COG play may have data available on some particular indirect uses at oil and gas wells

# read data for water used for whatever other purposes at oil and gas wells; change file names as appropriate
datwi = read_csv("Data/state_indirect_water_reported.csv")

## NOTE:
#   1) the following function is also returned as a ".rds" file in the "Data" folder

# wrangle available indirect water-use data to necessary structure for modeling; list available data in "data" argument
datdir = read_rds("Data/datdir.rds")
datind = .wrangle_indirect(data=list(datwi),
                           data2=datdir,
                           brkpt=.brkpt)

print("input to indirect water-use analysis done")


# #  ------------------------------------------------------------------------#
# # A.3. input to ancillary water-use analysis----
# #  ------------------------------------------------------------------------#
#
#
# ## NOTE:
# #   1) the following data may be the same as for the indirect water-use analysis;
# #   2) if the data are the same as for the indirect water-use analysis, see the preceding "NOTE" (section A.2.) for details about running the following code chunks; if not, then:
# #   3) to run the following code chunks (including sections B.3. and C.3.) as is, the data must be structured as follows:
# #     a) rows of observations for water use compiled by county;
# #     b) columns of variables for state, county, use, year and volume;
# #     c) see "nation_water_compiled.csv" for the appropriate data structure
# #   4) the data are unlikely to have limitations as they are uniformly available nationwide from the U.S. Geological Survey
# #   5) the script can be modified as appropriate if a COG play has data available at a finer spatial or temporal resolution
#
# # read data for water used for other non-COG mining purposes in counties, cities or states; change file names as appropriate
# datwa = read_csv("Data/nation_ancillary_water_compiled.csv")
#
# ## NOTE:
# #   1) to run the following code chunks (including sections B.3. and C.3.) as is, the data must be structured as follows:
# #     a) rows of modeled climate values aggregated to county;
# #     b) columns of variables for state, county, year, total precipitation and average temperature;
# #     c) see "climate_modeled.csv" for the appropriate data structure
# #   2) the data are unlikely to have limitations as they are uniformly available nationwide from the PRISM Climate Group
# #   3) the script can be modified as appropriate if a COG play has data available at a finer spatial or temporal resolution
#
# # read data for modeled climate; change file names as appropriate
# datc = read_csv("Data/climate_modeled.csv")
#
# ## NOTE:
# #   1) to run the following code chunks (including sections B.3. and C.3.) as is, the data must be structured as follows:
# #     a) rows of estimated population values aggregated to county;
# #     b) columns of variables for state, county, year and population;
# #     c) see "population_estimated.csv" for the appropriate data structure
# #   2) the data are unlikely to have limitations as they are uniformly available nationwide from the U.S. Census Bureau
# #   3) the script can be modified as appropriate if a COG play has data available at a finer spatial resolution
#
# # read data for estimated population; change file names as appropriate
# datp = read_csv("Data/population_estimated.csv")
#
# ## NOTE:
# #   1) the following function is also returned as a ".rds" file in the "Data" folder
#
# # wrangle available ancillary water-use data to necessary structure for modeling; list available data in "data" argument
# datdir = read_rds("Data/datdir.rds")
# datind = read_rds("Data/datind.rds")
# datanc = .wrangle_ancillary(data=list(datwa),
#                             data2=datdir,
#                             data3=datc,
#                             data4=datp,
#                             data5=datind,
#                             brkpt=.brkpt)
#
# print("input to ancillary water-use analysis done")
#
#
# #  ------------------------------------------------------------------------#
# # A.4. input to population analysis----
# #  ------------------------------------------------------------------------#
#
#
# ## COMMENT:
# #   a) the following function is not needed for the water-use analysis per se
#
# ## NOTE:
# #   1) the following function is also returned as a ".rds" file in the "Data" folder
#
# # read data for estimated population; change file names as appropriate
# datp = read_csv("Data/population_estimated.csv")
#
# # read data for wells counted; change file names as appropriate
# datfd = read_csv("Data/well_fracking_direct_water_reported.csv")
#
# # wrangle available population data to necessary structure for modeling
# datpop = .wrangle_population(data=datp,
#                              data2=datfd,
#                              brkpt=.brkpt)
#
# print("input to population analysis done")



#  ------------------------------------------------------------------------#
# B.1. process for direct water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) the following function is also returned as a ".rds" file in the "Product" folder

# model mean and 5th, 50th and 95th percentile water use for direct uses as a function of the number of wells using linear and quantile regression with leave-one-out cross-validation
datdir = read_rds("Data/datdir.rds")
moddir = .model_direct(data=datdir,
                       parm=c("Mean", "P5", "P50", "P95"),
                       catg="Direct",
                       conlev=.conlev)

print("process for direct water-use analysis done")


#  ------------------------------------------------------------------------#
# B.2. process for indirect water-use analysis----
#  ------------------------------------------------------------------------#


## NOTE:
#   1) the following function is also returned as a ".rds" file in the "Product" folder

# model mean and 5th, 50th and 95th percentile water use for indirect uses as a function of the number of wells using linear and quantile regression with leave-one-out cross-validation
datind = read_rds("Data/datind.rds")
modind = .model_indirect(data=datind,
                         parm=c("Mean", "P5", "P50", "P95"),
                         catg="Indirect",
                         conlev=.conlev)

print("process for indirect water-use analysis done")


# #  ------------------------------------------------------------------------#
# # B.3. process for ancillary water-use analysis----
# #  ------------------------------------------------------------------------#
#
#
# ## NOTE:
# #   1) the following function is also returned as a ".rds" file in the "Product" folder
#
# # model mean and 5th, 50th and 95th percentile water use for ancillary uses as a function of the number of wells using linear and quantile regression with leave-one-out cross-validation
# datanc = read_rds("Data/datanc.rds")
# modanc = .model_ancillary(data=datanc,
#                           parm=c("Mean", "P5", "P50", "P95"),
#                           catg="Ancillary",
#                           conlev=.conlev)
#
# print("process for ancillary water-use analysis done")
#
#
# #  ------------------------------------------------------------------------#
# # B.4. process for population analysis----
# #  ------------------------------------------------------------------------#
#
#
# ## COMMENT:
# #   a) the following function is not needed for the water-use analysis per se
#
# ## NOTE:
# #   1) the following function is also returned as a ".rds" file in the "Product" folder
#
# # model mean population as a function of the number of wells using linear regression
# datpop = read_rds("Data/datpop.rds")
# modpop = .model_population(data=datpop,
#                            conlev=.conlev)
#
# print("process for population analysis done")



#  ------------------------------------------------------------------------#
# C.1. output from direct water-use analysis----
#  ------------------------------------------------------------------------#


# write and plot data on direct water use associated with COG development
moddir = read_rds("Product/moddir.rds")
.visualize_direct(data=moddir,
                  catg="direct")

## NOTE:
#   1) the data are structured as follows:
#     a) rows of estimated water-use coefficients by direct use;
#     b) columns of variables for parameter estimates and confidence intervals by parameter
#   2) the data are plotted as direct water use against number of wells (grouped by year and county)
#   3) the preceding function is only returned as ".csv" and ".svg" files in the "Product" folder (not as an object in the environment)

print("output from direct water-use analysis done")


#  ------------------------------------------------------------------------#
# C.2. output from indirect water-use analysis----
#  ------------------------------------------------------------------------#


# write and plot data on indirect water use associated with COG development
modind = read_rds("Product/modind.rds")
.visualize_indirect(data=modind,
                    catg="indirect")

## NOTE:
#   1) the data are structured as follows:
#     a) rows of estimated water-use coefficients for indirect uses;
#     b) columns of variables for parameter estimates and confidence intervals by parameter
#   2) the data are plotted as indirect water use against number of wells (grouped by year and county)
#   3) the preceding function is only returned as ".csv" and ".svg" files in the "Product" folder (not as an object in the environment)

print("output from indirect water-use analysis done")


# #  ------------------------------------------------------------------------#
# # C.3. output from ancillary water-use analysis----
# #  ------------------------------------------------------------------------#
#
#
# # write and plot data on ancillary water use associated with COG development
# modanc = read_rds("Product/modanc.rds")
# .visualize_ancillary(data=modanc,
#                      catg="ancillary")
#
# ## NOTE:
# #   1) the data are structured as follows:
# #     a) rows of estimated water-use coefficients by ancillary use;
# #     b) columns of variables for parameter estimates and confidence intervals by parameter
# #   2) the data are plotted as ancillary water use against number of wells (grouped by year and county)
# #   3) the preceding function is only returned as ".csv" and ".svg" files in the "Product" folder (not as an object in the environment)
#
# print("output from ancillary water-use analysis done")
#
#
# #  ------------------------------------------------------------------------#
# # C.4. output from population analysis----
# #  ------------------------------------------------------------------------#
#
#
# ## COMMENT:
# #   a) the following function is not needed for the water-use analysis per se
#
# # write and plot data on population associated with COG development
# modpop = read_rds("Product/modpop.rds")
# .visualize_population(data=modpop)
#
# ## NOTE:
# #   1) the data are structured as follows:
# #     a) rows of estimated population coefficient;
# #     b) columns of variables for parameter estimate and confidence interval
# #   2) the data are plotted as population against number of wells (grouped by year) and against year
# #   3) the preceding function is only returned as ".csv" and ".svg" files in the "Product" folder (not as an object in the environment)
#
# print("output from population analysis done")


#  ------------------------------------------------------------------------#
# C.5. output from water-use analysis for data release----
#  ------------------------------------------------------------------------#


# write data on predictions and coefficients of water use associated with COG development
preddir = read_csv("Product/predictions_direct.csv")
predind = read_csv("Product/predictions_indirect.csv")
# predanc = read_csv("Product/predictions_ancillary.csv")
coefdir = read_csv("Product/coefficients_direct.csv")
coefind = read_csv("Product/coefficients_indirect.csv")
# coefanc = read_csv("Product/coefficients_ancillary.csv")
.visualize_data_release(data=preddir,
                        data2=predind,
                        # data3=predanc,
                        data4=coefdir,
                        data5=coefind)
                        # data5=coefind,
                        # data6=coefanc)

## NOTE:
#   1) the data are structured as follows:
#     a) rows of estimated water-use predictions and coefficients by direct, indirect or ancillary use;
#     b) columns of variables for parameter estimates and confidence intervals by parameter
#   2) the preceding function is returned as ".csv" files in the "Product" folder (not as an object in the environment)

print("output from water-use analysis for data release done")
