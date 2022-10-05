#  ------------------------------------------------------------------------#
# munge raw data release files for running water use analysis----
#  ------------------------------------------------------------------------#


# read code with prerequisites
source("Code/0prereqs.R")



#  ------------------------------------------------------------------------#
# make working data files with appropriate structure----
#  ------------------------------------------------------------------------#


# make reported and estimated well direct water use data from FracFocus and IHS Markit
temp = read_csv("Raw/fracfocus_short.csv", guess_max=10000) %>%
  # rename(Volume = TotalBaseWaterVolume,
  #        Depth = TotalVerticalDepth) %>%
  rename(Volume = VolMeanPerAPI,
         Depth = TVD,
         Year = job_end_date) %>%
  # mutate(Count = 1)
  mutate(Year = format(Year, "%Y"),
         Count = 1) %>%
  group_by(Year) %>%
  filter(n() > 1) %>%
  ungroup()
dat = temp %>%
  mutate(Use = "Hydraulic fracturing",
         Volume = Volume / 1e6) %>%
  filter(Volume > 0)
qile = quantile(dat$Volume)
iqr = IQR(dat$Volume)
dat %<>%
  filter(Volume < qile[["75%"]] + iqr * 3,
         Volume > qile[["25%"]] - iqr * 1.5) %>%
  select(State, County, Use, Year, Volume, Count) %>%
  group_by(State, County, Use, Year) %>%
  summarize(across(everything(), ~sum(.x, na.rm=T)), .groups="drop")
# temp2 = read_csv("Raw/IHS_Markit_Water_Treatment_2000_2019.csv", guess_max=10000) %>%
#   mutate(Use = "Hydraulic fracturing",
#          Volume = Volume / 1e6) %>%
#   select(State, County, Use, Year, Volume, Count) %>%
#   group_by(State, County, Use, Year) %>%
#   summarize(across(everything(), ~sum(.x, na.rm=T)), .groups="drop")
dat %<>%
  # full_join(temp2) %>%
  # group_by(State, County, Use, Year) %>%
  # summarize(across(everything(), ~max(.x, na.rm=T)), .groups="drop") %>%
  arrange(State, County, Year)
write_csv(dat, "Data/well_fracking_direct_water_reported.csv")
temp %<>%
  filter(Depth > 0)
qile = quantile(temp$Depth)
iqr = IQR(temp$Depth)
temp %<>%
  filter(Depth < qile[["75%"]] + iqr * 3,
         Depth > qile[["25%"]] - iqr * 1.5)
dat = temp %>%
  # ratio of one-quarter volume of annulus to volume of borehole (0.25) and about one-half weight of water (5 gallons) to weight of cement per cubic foot assuming 1 cubic foot of volume per foot of depth
  mutate(Use = "Cementing",
         Volume = round(Depth * 0.25 * 6, 0),
         Volume = Volume / 1e6) %>%
  select(State, County, Use, Year, Volume, Count) %>%
  arrange(State, County, Year)
write_csv(dat, "Data/well_cementing_direct_water_estimated.csv")
dat = temp %>%
  # ratio of about double volume of water (7.5 gallons) to volume of borehole per foot assuming 1 cubic foot of volume per foot of depth
  mutate(Use = "Drilling",
         Volume = round(Depth * 7.5 * 2, 0),
         Volume = Volume / 1e6) %>%
  select(State, County, Use, Year, Volume, Count) %>%
  arrange(State, County, Year)
write_csv(dat, "Data/well_drilling_direct_water_estimated.csv")


# make reported well indirect water use data from TX WDB, in million gallons
dat = read_csv("Raw/PointSourceData_Tx_1980_2018.csv", guess_max=10000) %>%
  rename(Use = UseType) %>%
  filter(SS_Pur != "Pur",
         Volume > 0,
         Year >= 1981) %>%
  mutate(State = "Texas",
         County = str_to_title(County),
         RelatedtoCOG = str_to_title(RelatedtoCOG),
         Use = case_when(Use == "Mining" & RelatedtoCOG == "Yes" ~ "Indirect",
                         TRUE ~ Use),
         Use = case_when(Use == "Industrial" & RelatedtoCOG != "No" ~ "Indirect",
                         TRUE ~ Use),
         Volume = Volume / 1e6) %>%
  filter(Use == "Indirect") %>%
  select(State, County, Use, Year, Volume) %>%
  arrange(State, County, Year)
write_csv(dat, "Data/state_indirect_water_reported.csv")


# # make estimated population data from U.S. Census Bureau
# dat = read_csv("Raw/USCBpopulation_1980_2019.csv", guess_max=10000) %>%
#   rename(Persons = Population) %>%
#   filter(Year >= 1981) %>%
#   arrange(State, County, Year)
# write_csv(dat, "Data/population_estimated.csv")
#
#
# # make modeled climate data from PRISM Climate Group
# dat = read_csv("Raw/PRISMclimate_1981_2019.csv", guess_max=10000) %>%
#   rename(TotalPpt = Precipitation_mm, AverageTmp = Temperature_C) %>%
#   arrange(State, County, Year)
# write_csv(dat, "Data/climate_modeled.csv")
#
#
# # read compiled water-use data from U.S. Geological Survey and ScienceBase
# fil = c("1985/us85co.xls", "1990/us90co.txt", "1995/usco1995.xls", "2000/usco2000.xls", "2005/usco2005.xls", "2010/usco2010.xlsx")
# for (ifil in fil) {
#   temp = try(download.file(paste0("https://water.usgs.gov/watuse/data/", ifil), destfile=paste0("Raw/", str_split(ifil, "/", simplify=T)[, 2]), mode="wb", cacheOK=F))
#   if ("try-error" %in% class(temp)) {
#     stop("file not downloaded because url has changed or libcurl is outdated or not installed; verify that url has not changed or that libcurl is installed and updated (go to http://curl.haxx.se/download.html); water-use data may be downloaded from the National Water Information System Web interface, Water Data for the Nation, at https://waterdata.usgs.gov/nwis/wu")
#   }
# }
# fil = str_split(fil, "/", simplify=T)[, 2] %>%
#   c("usco2015v2.0.xlsx")
# temp = try(download.file("https://www.sciencebase.gov/catalog/file/get/5af3311be4b0da30c1b245d8?f=__disk__29%2Fc0%2F51%2F29c051a5166ae254b942322f77b02edcda0822ac", destfile=paste0("Raw/", tail(fil, 1)), mode="wb", cacheOK=F))
# if ("try-error" %in% class(temp)) {
#   stop("file not downloaded because url has changed (go to https://doi.org/10.5066/F7TB15V5) or libcurl is outdated or not installed; verify that url has not changed or that libcurl is installed and updated (go to http://curl.haxx.se/download.html); water-use data may be downloaded from the National Water Information System Web interface, Water Data for the Nation, at https://waterdata.usgs.gov/nwis/wu")
# }
#
# # subset data for state, counties and water-use categories of interest
# wu = list()
# fipsst = formatC(c(35, 48), width=2, flag="0")
# fipscnty1 = formatC(sort(c(6, 28, seq(1, 61, by=2))), width=3, flag="0")
# fipscnty2 = formatC(seq(1, 507, by=2), width=3, flag="0")
# fipsstcnty = c(paste(fipsst[1], fipscnty1, sep="_"), paste(fipsst[2], fipscnty2, sep="_"))
# wu$`1985` = type_convert(read_xls(paste0("Raw/", fil[1]), col_types="text")) %>%
#   mutate(temp = paste(scode, area, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(scode, area, "la-total", "do-sstot", "in-wtotl", "ir-frtot", "ls-total", "mi-total", "ps-total", "pt-wtotl")
# wu$`1990` = type_convert(read_tsv(paste0("Raw/", fil[2]), col_types=cols(.default=col_character()))) %>%
#   mutate(temp = paste(scode, area, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(scode, area, "la-total", "do-sstot", "in-wtotl", "ir-frtot", "ls-total", "mi-total", "ps-total", "pt-wtotl")
# wu$`1995` = type_convert(read_xls(paste0("Raw/", fil[3]), col_types="text")) %>%
#   mutate(temp = paste(StateCode, CountyCode, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(StateCode, CountyCode, "LA-WTotl", "DO-WTotl", "IN-WTotl", "IR-WTotl", "LS-WTotl", "MI-WTotl", "PS-WTotl", "PT-WTotl")
# wu$`2000` = type_convert(read_xls(paste0("Raw/", fil[4]), col_types="text")) %>%
#   mutate(temp = paste(STATEFIPS, COUNTYFIPS, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(STATEFIPS, COUNTYFIPS, "LA-WFrTo", "DO-WFrTo", "IN-Wtotl", "IT-WFrTo", "LS-WFrTo", "MI-Wtotl", "PS-WFrTo", "PT-Wtotl")
# wu$`2005` = type_convert(read_xls(paste0("Raw/", fil[5]), col_types="text")) %>%
#   mutate(temp = paste(STATEFIPS, COUNTYFIPS, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(STATEFIPS, COUNTYFIPS, "LA-WFrTo", "DO-WFrTo", "IN-Wtotl", "IR-WFrTo", "LS-WFrTo", "MI-Wtotl", "PS-Wtotl", "PT-Wtotl")
# wu$`2010` = type_convert(read_xlsx(paste0("Raw/", fil[6]), col_types="text")) %>%
#   mutate(temp = paste(STATEFIPS, COUNTYFIPS, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(STATEFIPS, COUNTYFIPS, "AQ-WTotl", "DO-WFrTo", "IN-Wtotl", "IR-WFrTo", "LI-WFrTo", "MI-Wtotl", "PS-Wtotl", "PT-Wtotl")
# wu$`2015` = type_convert(read_xlsx(paste0("Raw/", fil[7]), col_types="text", skip=1)) %>%
#   mutate(temp = paste(STATEFIPS, COUNTYFIPS, sep="_")) %>%
#   filter(temp %in% fipsstcnty) %>%
#   select(STATE, COUNTY, STATEFIPS, COUNTYFIPS, "AQ-Wtotl", "DO-WFrTo", "IN-Wtotl", "IR-WFrTo", "LI-WFrTo", "MI-Wtotl", "PS-Wtotl", "PT-Wtotl")
#
# # combine data for compilation years by water-use categories
# st = wu$`2015`$STATE %>%
#   recode("NM" = "New Mexico", "TX" = "Texas")
# cnty = wu$`2015`$COUNTY %>%
#   str_replace_all(" County", "")
# wu$`2015` %<>% select(-STATE, -COUNTY)
# wu %<>%
#   map(function(x) add_column(x, State = st, County = cnty)) %>%
#   map(function(x) {names(x) = c("STATEFIPS", "COUNTYFIPS", "Aquaculture", "Domestic", "Industrial", "Irrigation", "Livestock", "Mining", "Public supply", "Thermoelectric power", "State", "County"); return(x)}) %>%
#   bind_rows(.id="Year") %>%
#   mutate(Year = as.numeric(Year)) %>%
#   select(State, County, everything(), -STATEFIPS, -COUNTYFIPS) %>%
#   pivot_longer(-c(State, County, Year), names_to="Use", values_to="Volume") %>%
#   mutate(STCNTY = paste(State, County, sep="_"))
#
# # linearly interpolate values for years with missing data
# dat = list()
# ctr = 1
# for (iuse in unique(wu$Use)) {
#   for (istcnty in unique(wu$STCNTY)) {
#     temp = wu %>%
#       filter(Use == iuse,
#              STCNTY == istcnty)
#     temp2 = data.frame(State = str_split(istcnty, "_", simplify=T)[1],
#                        County = str_split(istcnty, "_", simplify=T)[2],
#                        Year = c(1981:2019)[!1981:2019 %in% unique(temp$Year)],
#                        Use = iuse,
#                        Volume = NA,
#                        STCNTY = istcnty)
#     temp %<>%
#       bind_rows(temp2) %>%
#       arrange(Year)
#     if (length(na.omit(temp$Volume)) == 0) {
#       temp$Volume = 0
#     } else if (length(na.omit(temp$Volume)) == 1) {
#       temp$Volume = unique(na.omit(temp$Volume))
#     } else {
#       temp %<>%
#         mutate(x = seq(1, n()),
#                Volume = approx(x, Volume, x, rule=2)$y) %>%
#         select(-x)
#     }
#     dat[[ctr]] = temp
#     ctr = ctr + 1
#   }
# }
# dat %<>%
#   bind_rows() %>%
#   filter(!Use %in% c("Aquaculture", "Irrigation", "Livestock")) %>%
#   mutate(Volume = Volume * 365) %>%
#   arrange(match(Use, c("Domestic", "Public supply", "Industrial", "Mining", "Thermoelectric power"))) %>%
#   select(State, County, Use, everything(), -STCNTY)
#
# # make compiled water-use data from U.S. Geological Survey
# write_csv(dat, "Data/nation_ancillary_water_compiled.csv")
