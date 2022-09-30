#  ------------------------------------------------------------------------#
# munge raw data release files for running water use analysis----
#  ------------------------------------------------------------------------#


# read code with prerequisites
source("Code/0prereqs.R")



#  ------------------------------------------------------------------------#
# make working data files with appropriate structure----
#  ------------------------------------------------------------------------#


# make reported and estimated well direct water use data from FracFocus and IHS Markit
dat <- read_csv("Raw/fracfocus_short.csv", col_select = -1, guess_max = 10000) %>%
  rename(Volume = VolMeanPerAPI,
         Depth = TVD,
         Subwatershed = huc12,
         YearMonth = job_end_date) %>%
  mutate(YearMonth = format(YearMonth, "%Y%m"),
         Subwatershed = as.character(Subwatershed),
         Volume = round(Volume / 1e6, 6),
         Count = 1,
         temp = str_sub(YearMonth, 1, 4)) %>%
  group_by(temp) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(Subwatershed, YearMonth, Volume, Count, Depth) %>%
  arrange(Subwatershed, YearMonth)
datf <- dat %>%
  filter(Volume > 0) %>%
  select(-Depth)
datf %<>%
  filter(#Volume < quantile(datf$Volume, probs = 0.75, na.rm = TRUE) + IQR(datf$Volume, na.rm = TRUE) * 3,
         #log(Volume) > quantile(log(datf$Volume), probs = 0.25, na.rm = TRUE) - IQR(log(datf$Volume), na.rm = TRUE) * 3,
         Volume >= 100e-6,
         Volume < 75) %>%
  group_by(Subwatershed, YearMonth) %>%
  summarize(across(c(Volume, Count), ~sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(Use = "Fracturing")
write_rds(datf, "Data/well_fracking_direct_water_reported.rds")
dat %<>%
  filter(Depth > 0,
         #Depth < quantile(dat$Depth, probs = 0.75, na.rm = TRUE) + IQR(dat$Depth, na.rm = TRUE) * 3,
         #log(Depth) > quantile(log(dat$Depth), probs = 0.25, na.rm = TRUE) - IQR(log(dat$Depth), na.rm = TRUE) * 3,
         Depth >= 100,
         Depth < 25000)
datc <- dat %>%
  # ratio of one-quarter volume of annulus to volume of borehole (0.25) and about one-half weight of water (6 gallons) to weight of cement per cubic foot assuming 1 cubic foot of volume per foot of depth
  mutate(Volume = round(Depth * 0.25 * 6, 0),
         Volume = Volume / 1e6) %>%
  select(-Depth) %>%
  mutate(Use = "Cementing")
write_rds(datc, "Data/well_cementing_direct_water_estimated.rds")
datd <- dat %>%
  # ratio of about double volume of water (7.5 gallons) to volume of borehole per foot assuming 1 cubic foot of volume per foot of depth
  mutate(Volume = round(Depth * 7.5 * 2, 0),
         Volume = Volume / 1e6) %>%
  select(-Depth) %>%
  mutate(Use = "Drilling")
write_rds(datd, "Data/well_drilling_direct_water_estimated.rds")
dati <- dat %>%
  # proportion of about half of average per-well cementing and drilling and half of per-well cementing and drilling volume
  mutate(Volume = round(sum(datc$Volume, datd$Volume) / (sum(datc$Count, datd$Count) / 2) * 0.5 + (datd$Volume + datc$Volume) * 0.5, 6)) %>%
  select(-Depth) %>%
  mutate(Use = "Indirect")
write_rds(dati, "Data/well_indirect_water_estimated.rds")
