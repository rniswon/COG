#  ------------------------------------------------------------------------#
# wrangle input data----
#  ------------------------------------------------------------------------#



#  ------------------------------------------------------------------------#
# model year that COG development began by estimating breakpoint in volume of water used using segmented linear regression----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct water-use input data as data frames
.model_breakpoint = function(data=list(data)) {

  if (any(sapply(list(data), is.null))) stop("one or more necessary arguments missing")

  # get all direct water-use input data
  data %<>% reduce(full_join)

  # summarize volume by year
  dat = data %>%
    group_by(Year) %>%
    summarize(Volume = sum(Volume, na.rm=T), .groups="drop")

  # fit linear regression of volume against year
  mod = glm(Volume ~ Year, data=dat)

  # estimate breakpoint by fitting segmented relationship to fitted linear model of volume against year
  brkpt = segmented(mod, seg.Z=~Year, psi=list(Year=NA), control=seg.control(n.boot=0, it.max=100, fix.npsi=F, K=round(length(unique(dat$Year)) / 3)))
  if (is.null(brkpt$psi)) {
    print("no breakpoint estimated")
  } else {
    print(brkpt$psi[, "Est."])
  }
  windows()
  plot(dat$Year, dat$Volume, type="b")
}



#  ------------------------------------------------------------------------#
# A.1. wrangle direct water-use input data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct water-use input data for fracking, cementing, drilling or other uses as data frames;
#   brkpt = estimated breakpoint from ".model_breakpoint" function as numeric vector
.wrangle_direct = function(data=list(data),
                           brkpt=.brkpt) {

  if (any(sapply(list(data, brkpt), is.null))) stop("one or more necessary arguments missing")

  # get all direct water-use input data
  data %<>% reduce(full_join)

  # for each use
  if ("Hydraulic fracturing" %in% unique(data$Use)) {
    data %<>% arrange(match(Use,  "Hydraulic fracturing"))
  }
  datdir = list()
  for (iuse in unique(data$Use)) {

    # summarize each direct water use volume and number of wells by state, county and year for years following estimated breakpoint
    datdir[[iuse]] = data %>%
      filter(Use == iuse,
             Year >= brkpt) %>%
      select(-Use) %>%
      group_by(State, County, Year) %>%
      summarize(across(everything(), ~sum(.x, na.rm=T)), .groups="drop") %>%
      arrange(Year)

    # apply year average by county of drilling and cementing water use per well to wells in years and counties with only hydraulic fracturing water use
    if (iuse %in% c("Cementing", "Drilling") & "Hydraulic fracturing" %in% unique(data$Use)) {
      idatd = datdir[[iuse]]
      datfd = datdir[["Hydraulic fracturing"]]
      temp = inner_join(datfd, idatd, by=c("State", "County", "Year"))
      if (any(!c(nrow(datfd), nrow(idatd)) %in% nrow(temp))) {

        # linearly interpolate values of drilling and cementing water use for years and counties with missing data
        dat = semi_join(idatd, datfd, by=c("State", "County")) %>%
          mutate(Volume = Volume / Count,
                 stcnty = paste(State, County, sep="_"))
        dat2 = list()
        for (istcnty in unique(dat$stcnty)) {
          temp = dat %>%
            filter(stcnty == istcnty)
          if (all(unique(dat$Year) %in% unique(temp$Year))) {
            dat2[[istcnty]] = temp
          } else {
            temp2 = data.frame(State = str_split(istcnty, "_", simplify=T)[1],
                               County = str_split(istcnty, "_", simplify=T)[2],
                               Year = unique(dat$Year)[!unique(dat$Year) %in% unique(temp$Year)],
                               Volume = NA,
                               Count = NA)
            temp %<>%
              bind_rows(temp2) %>%
              arrange(Year)
            if (length(na.omit(temp$Volume)) == 0) {
              temp$Volume = 0
            } else if (length(na.omit(temp$Volume)) == 1) {
              temp$Volume = unique(na.omit(temp$Volume))
            } else {
              temp %<>%
                mutate(x = cumsum(c(0, diff(temp$Year))),
                       Volume = approx(x, Volume, x, rule=2)$y) %>%
                select(-x)
            }
            dat2[[istcnty]] = temp
          }
        }
        dat2 %<>%
          bind_rows() %>%
          select(-stcnty)
        temp = dat %>%
          group_by(Year) %>%
          summarize(Volume = mean(Volume, na.rm=T), .groups="drop")
        dat = anti_join(datfd, idatd, by=c("State", "County"))
        if (any(!dat$Year %in% temp$Year)) {
          temp2 = data.frame(Year = unique(dat$Year)[!unique(dat$Year) %in% unique(temp$Year)],
                             Volume = NA)
          temp %<>%
            bind_rows(temp2) %>%
            arrange(Year)
          if (length(na.omit(temp$Volume)) == 0) {
            temp$Volume = 0
          } else if (length(na.omit(temp$Volume)) == 1) {
            temp$Volume = unique(na.omit(temp$Volume))
          } else {
            temp %<>%
              mutate(x = cumsum(c(0, diff(temp$Year))),
                     Volume = approx(x, Volume, x, rule=2)$y) %>%
              select(-x)
          }
        }
        dat %<>%
          mutate(Volume = temp$Volume[match(Year, temp$Year)]) %>%
          full_join(dat2, .) %>%
          inner_join(datfd, ., by=c("State", "County", "Year")) %>%
          mutate(Volume = Volume.y * Count.x) %>%
          rename(Count = Count.x) %>%
          select(State, County, Year, Volume, Count)
      }
      datdir[[iuse]] = dat
    }
  }

  # summarize all direct water use volume and number of wells by state, county and year
  if (!"Direct" %in% unique(data$Use)) {
    datdir[["Direct"]] = reduce(datdir, full_join, by=c("State", "County", "Year", "Volume", "Count")) %>%
      group_by(State, County, Year) %>%
      summarize(Volume = sum(Volume, na.rm=T),
                Count = first(Count), .groups="drop") %>%
      arrange(Year)
  }

  # make direct water-use input data by use
  datdir %<>%
    ldply(data.frame, .id="Use") %>%
    mutate(Volume = round(Volume, 6)) %>%
    as_tibble()
  write_rds(datdir, "Data/datdir.rds")
}


# #  ------------------------------------------------------------------------#
# # A.2. wrangle indirect water-use input data----
# #  ------------------------------------------------------------------------#
# 
# 
# # make function with following arguments:
# #   data = indirect water-use input data for whatever uses as data frames;
# #   data2 = direct water-use input data from ".wrangle_direct" function as data frame;
# #   brkpt = estimated breakpoint from ".model_breakpoint" function as numeric vector
# .wrangle_indirect = function(data=list(data),
#                              data2=datdir,
#                              brkpt=.brkpt) {
# 
#   if (any(sapply(list(data, data2, brkpt), is.null))) stop("one or more necessary arguments missing")
# 
#   # get all indirect water-use input data
#   data %<>% reduce(full_join)
# 
#   # for each use
#   datind = list()
#   for (iuse in unique(data$Use)) {
# 
#     # summarize each indirect water use volume by state and year for years following estimated breakpoint
#     datind[[iuse]] = data %>%
#       filter(Use == iuse,
#              Year >= brkpt) %>%
#       group_by(State, Year) %>%
#       summarize(Volume = sum(Volume, na.rm=T), .groups="drop") %>%
#       arrange(Year)
# 
#     # summarize all direct water use volume and number of wells by state and year
#     datdir = data2 %>%
#       filter(Use == "Direct") %>%
#       group_by(State, Year) %>%
#       summarize(Volume = sum(Volume, na.rm=T),
#                 Count = sum(Count, na.rm=T), .groups="drop")
# 
#     # apply average of indirect water use per well by year and state to years with only direct water use
#     dat = list()
#     for (ist in unique(data$State)) {
#       idati = datind[[iuse]] %>%
#         filter(State == ist)
#       idatd = datdir %>%
#         filter(State == ist)
#       temp = inner_join(idatd, idati, by=c("State", "Year"))
#       if (any(!c(nrow(idatd), nrow(idati)) %in% nrow(temp))) {
# 
#         # linearly interpolate values of indirect water use for years with missing data
#         temp = inner_join(idati, idatd, by=c("State", "Year")) %>%
#           rename(Volume = Volume.x) %>%
#           mutate(Volume = Volume / Count) %>%
#           select(-Volume.y)
#         temp2 = unique(idatd$Year)
#         temp2 = data.frame(State = ist,
#                            Year = temp2[!temp2 %in% unique(temp$Year)],
#                            Volume = NA,
#                            Count = NA)
#         temp %<>%
#           bind_rows(temp2) %>%
#           arrange(Year)
#         if (length(na.omit(temp$Volume)) == 0) {
#           temp$Volume = 0
#         } else if (length(na.omit(temp$Volume)) == 1) {
#           temp$Volume = unique(na.omit(temp$Volume))
#         } else {
#           temp %<>%
#             mutate(Step = seq(1, n()),
#                    Volume = approx(Step, Volume, Step, rule=2)$y) %>%
#             select(-Step)
#         }
#         temp %<>% mutate(Count = idatd$Count)
#         dat[[ist]] = temp
#       } else {
#         dat[[ist]] = idati %>%
#           mutate(Count = idatd$Count,
#                  Volume = Volume / Count)
#       }
#     }
#     dat %<>% reduce(full_join)
#     datind[[iuse]] = dat
# 
#     # apply average indirect water use per well by year to states with only direct water use
#     idati = datind[[iuse]]
#     if (any(!datdir$State %in% idati$State)) {
#       temp = idati %>%
#         group_by(Year) %>%
#         summarize(Volume = mean(Volume, na.rm=T), .groups="drop")
#       if (any(!datdir$Year %in% idati$Year)) {
# 
#         # linearly interpolate values of indirect water use for years with missing data
#         temp2 = unique(datdir$Year)
#         temp2 = data.frame(Year = temp2[!temp2 %in% unique(temp$Year)],
#                            Volume = NA,
#                            Count = NA)
#         temp %<>%
#           bind_rows(temp2) %>%
#           arrange(Year)
#         if (length(na.omit(temp$Volume)) == 0) {
#           temp$Volume = 0
#         } else if (length(na.omit(temp$Volume)) == 1) {
#           temp$Volume = unique(na.omit(temp$Volume))
#         } else {
#           temp %<>%
#             mutate(Step = seq(1, n()),
#                    Volume = approx(Step, Volume, Step, rule=2)$y) %>%
#             select(-Step)
#         }
#       }
#       dat = full_join(idati, datdir, by=c("State", "Year", "Count")) %>%
#         rename(Volume = Volume.x) %>%
#         select(-Volume.y)
#       temp2 = dat %>%
#         filter(is.na(Volume)) %>%
#         mutate(Volume = temp$Volume[match(Year, temp$Year)])
#       dat %<>%
#         anti_join(temp2, by="State") %>%
#         full_join(temp2) %>%
#         mutate(Volume = Volume * Count) %>%
#         arrange(Year, State)
#     }
#     datind[[iuse]] = dat
# 
#     # make indirect volume by state and year to indirect volume by state, year and county for post-COG years as proportional value (ratio of number of wells in any county to number of wells in all state)
#     idati = datind[[iuse]]
#     datdir = data2 %>%
#       filter(Use == "Direct")
#     temp = match(paste0(datdir$State, datdir$Year), paste0(idati$State, idati$Year))
#     temp2 = table(temp) %>%
#       .[unique(temp)]
#     datdir %<>% select(County, Count)
#     datind[[iuse]] = idati %>%
#       rename(Count2 = Count) %>%
#       uncount(weights=temp2) %>%
#       arrange(Year) %>%
#       bind_cols(datdir) %>%
#       mutate(Volume = Volume * Count / Count2) %>%
#       select(State, County, Year, Volume, Count)
#   }
# 
#   # summarize all indirect water use volume and number of wells by state, county and year
#   if (!"Indirect" %in% unique(data$Use)) {
#     datind[["Indirect"]] = reduce(datind, full_join, by=c("State", "County", "Year", "Volume", "Count")) %>%
#       group_by(State, County, Year) %>%
#       summarize(Volume = sum(Volume, na.rm=T),
#                 Count = first(Count), .groups="drop") %>%
#       arrange(Year)
#   }
# 
#   # make indirect water-use input data by use
#   datind %<>%
#     ldply(data.frame, .id="Use") %>%
#     mutate(Volume = round(Volume, 6)) %>%
#     as_tibble()
#   write_rds(datind, "Data/datind.rds")
# }
# 
# 
# #  ------------------------------------------------------------------------#
# # A.3. wrangle ancillary water-use input data----
# #  ------------------------------------------------------------------------#
# 
# 
# # make function with following arguments:
# #   data = ancillary water-use input data for whatever uses as data frames;
# #   data2 = direct water-use input data from ".wrangle_direct" function as data frame;
# #   data3 = modeled climate data as data frame;
# #   data4 = estimated population data as data frame;
# #   data5 = indirect water-use input data from ".wrangle_indirect" function as data frame;
# #   brkpt = estimated breakpoint from ".model_breakpoint" function as numeric vector
# .wrangle_ancillary = function(data=list(data),
#                               data2=datdir,
#                               data3=datc,
#                               data4=datp,
#                               data5=datind,
#                               brkpt=.brkpt) {
# 
#   if (any(sapply(list(data, data2, data3, data4, data5, brkpt), is.null))) stop("one or more necessary arguments missing")
# 
#   # get all ancillary water-use input data
#   data %<>% reduce(full_join)
# 
#   # get ancillary water use, climate and population data for state-county combinations associated with COG
#   stcnty = data2 %>%
#     filter(Year >= brkpt) %>%
#     group_by(State, County) %>%
#     summarize(Count = n(), .groups="drop") %>%
#     filter(Count >= 1) %>%
#     select(State, County)
#   datanc = data %>%
#     filter(State %in% stcnty$State,
#            County %in% stcnty$County) %>%
#     group_by(Use, State, County, Year) %>%
#     summarize(Volume = sum(Volume, na.rm=T), .groups="drop")
#   datclim = data3 %>%
#     filter(State %in% stcnty$State & County %in% stcnty$County)
#   datpopn = data4 %>%
#     filter(State %in% stcnty$State & County %in% stcnty$County)
# 
#   # add all missing combinations of ancillary water use for use, state, county and year with zero for volume
#   temp = datanc %>%
#     mutate(STCNTY = paste(State, County, sep="_")) %>%
#     select(-State, -County) %>%
#     expand(Use, STCNTY, Year) %>%
#     mutate(State = str_split(STCNTY, "_", simplify=T)[, 1],
#            County = str_split(STCNTY, "_", simplify=T)[, 2]) %>%
#     select(Use, State, County, Year)
#   datanc %<>% right_join(temp, by=c("Use", "State", "County", "Year"))
#   datanc$Volume %<>% replace_na(0)
# 
#   # combine ancillary water use, climate and population
#   datanc %<>%
#     left_join(datclim, by=c("State", "County", "Year")) %>%
#     left_join(datpopn, by=c("State", "County", "Year")) %>%
#     mutate(County = as.factor(County))
# 
#   # for each use
#   dat = dat2 = list()
#   for (iuse in unique(data$Use)) {
# 
#     # skip uses without USGS category
#     if (!iuse %in% c("Aquaculture", "Domestic", "Industrial", "Irrigation", "Livestock", "Mining", "Public supply", "Thermoelectric power")) next
# 
#     # get ancillary volume for years preceding (pre-COG) or years following (post-COG) estimated breakpoint
#     datancpre = datancpost = datanc %>%
#       filter(Use == iuse)
#     datancpre %<>% filter(between(Year, brkpt - 10, brkpt - 1))
#     datancpost %<>% filter(Year >= brkpt)
# 
#     # skip uses without enough data
#     if (nrow(datancpre) < 5 | nrow(datancpost) < 5) next
# 
#     # fit linear regressions of response against all possible combinations of predictors for pre-COG years
#     vrbs = c("Year", "County", "TotalPpt", "AverageTmp", "Persons")
#     idx = unlist(lapply(1:length(vrbs), function(x) combn(1:length(vrbs), x, simplify=F)), recursive=F)
#     form = lapply(idx, function(x) paste("Volume ~", paste(vrbs[x], collapse=" + ")))
#     mod = lapply(form, function(x) glm(as.formula(x), data=datancpre))
# 
#     # get model with least information loss for pre-COG years unless most complex model is within half-percent of best score
#     idx = which.min(lapply(mod, function(x) (x)$aic))
#     aic = modify_depth(mod, .depth=1, .f="aic") %>%
#       .[idx]
#     aic[[2]] = mod[[length(mod)]]$aic
#     if (aic[[2]] <= aic[[1]] * 1.005) {
#       idx = length(mod)
#     }
#     mod %<>% .[[idx]]
# 
#     # get predicted values for post-COG years, and change negative values to zero
#     pred = predict(mod, newdata=datancpost, type="response") %>%
#       data.frame("Fitted" = .) %>%
#       mutate(Fitted = ifelse(Fitted < 0, 0, Fitted))
# 
#     # get ancillary volume by year for post-COG years as residual value (difference of observed from predicted values), and change negative values to zero
#     datancpost %<>%
#       mutate(County = as.character(County)) %>%
#       bind_cols(pred) %>%
#       mutate(Residual = Volume - Fitted,
#              Residual = ifelse(Residual < 0, 0, Residual)) %>%
#       select(State, County, Year, Volume, Residual)
# 
#     # get direct water use
#     datdir = data2 %>%
#       filter(Use == "Direct") %>%
#       select(-Use)
#     datdir2 = datdir %>%
#       group_by(Year) %>%
#       summarize(Count = sum(Count, na.rm=T), .groups="drop")
# 
#     # make ancillary volume by state, year and county for post-COG years with values proportional to number of wells (ratio of number of wells in any county to number of wells in all state)
#     dat[[iuse]] = datancpost %>%
#       group_by(Year) %>%
#       summarize(Residual = sum(Residual, na.rm=T), .groups="drop") %>%
#       full_join(datancpost, ., by=c("Year")) %>%
#       full_join(., datdir2, by=c("Year")) %>%
#       full_join(., datdir, by=c("State", "County", "Year")) %>%
#       mutate(Count = ifelse(is.na(Count.y), 0, Count.y),
#              Volume = Residual.y * Count / Count.x) %>%
#       filter(Count != 0,
#              Volume != 0) %>%
#       select(State, County, Year, Volume, Count) %>%
#       arrange(Year)
# 
#     # make ancillary volume by state, year and county for post-COG years with values proportional to volumes (ratio of volume in any county to volume in all state)
#     dat2[[iuse]] = datancpost %>%
#       group_by(Year) %>%
#       summarize(Volume = sum(Volume, na.rm=T),
#                 Residual = sum(Residual, na.rm=T), .groups="drop") %>%
#       full_join(datancpost, ., by=c("Year")) %>%
#       full_join(., datdir2, by=c("Year")) %>%
#       mutate(Count = Count * Volume.x / Volume.y,
#              Count = ifelse(Count > 0 & Count < 1, 1, round(Count)),
#              Volume = Residual.y * Volume.x / Volume.y) %>%
#       filter(Count != 0,
#              Volume != 0) %>%
#       select(State, County, Year, Volume, Count)
# 
#     # if ancillary water-use input data for mining also has direct and indirect water use, get ancillary volume as residual value
#     if (iuse == "Mining") {
# 
#       # get direct and indirect water use
#       datdir = data2 %>%
#         filter(Use == "Direct") %>%
#         select(-Use)
#       datdir2 = datdir %>%
#         group_by(Year) %>%
#         summarize(Count = sum(Count, na.rm=T),
#                   Volume = sum(Volume, na.rm=T), .groups="drop")
#       datind = data5 %>%
#         filter(Use == "Indirect") %>%
#         select(-Use)
#       datind2 = datind %>%
#         group_by(Year) %>%
#         summarize(Volume = sum(Volume, na.rm=T), .groups="drop")
# 
#       # get residual values
#       temp = datancpost %>%
#         group_by(Year) %>%
#         summarize(Volume = sum(Volume, na.rm=T),
#                   Residual = sum(Residual, na.rm=T), .groups="drop") %>%
#         full_join(datancpost, ., by=c("Year")) %>%
#         full_join(., datdir2, by=c("Year")) %>%
#         full_join(., datind2, by=c("Year"))
#       for (ist in unique(temp$State)) {
#         temp2 = temp %>%
#           filter(State == ist) %>%
#           group_by(Year) %>%
#           summarize(Residual = mean(Residual.y),
#                     Volume = mean(Volume.x.x + Volume.y.y)) %>%
#           mutate(Residual = Residual - Volume) %>%
#           pull(Residual)
# 
#         # skip states with negative residual values
#         if (any(temp2 < 0, na.rm=T)) next
# 
#         # make ancillary volume by state, year and county for post-COG years with values proportional to number of wells (ratio of number of wells in any county to number of wells in all state)
#         temp2 = temp %>%
#           full_join(., datdir, by=c("State", "County", "Year")) %>%
#           filter(State == ist) %>%
#           mutate(Count = ifelse(is.na(Count.y), 0, Count.y),
#                  Volume = Volume.x.x + Volume.y.y,
#                  Volume = (Residual.y - Volume) * Count / Count.x,
#                  Volume = ifelse(Volume < 0, 0, Volume)) %>%
#           filter(Count != 0,
#                  Volume != 0) %>%
#           select(State, County, Year, Volume, Count)
#         temp3 = dat[[iuse]] %>%
#           filter(State != ist)
#         dat[[iuse]] = full_join(temp2, temp3) %>%
#           arrange(Year)
# 
#         # make ancillary volume by state, year and county for post-COG years with values proportional to volumes (ratio of volume in any county to volume in all state)
#         temp2 = temp %>%
#           mutate(Count = Count * Volume.x / Volume.y,
#                  Count = ifelse(Count > 0 & Count < 1, 1, round(Count)),
#                  Volume = Volume.x.x + Volume.y.y,
#                  Volume = (Residual.y - Volume) * Volume.x / Volume.y,
#                  Volume = ifelse(Volume < 0, 0, Volume)) %>%
#           filter(Count != 0,
#                  Volume != 0) %>%
#           select(State, County, Year, Volume, Count)
#         temp3 = dat2[[iuse]] %>%
#           filter(State != ist)
#         dat2[[iuse]] = full_join(temp2, temp3) %>%
#           arrange(Year)
#       }
#     }
# 
#     if (all(datancpost$Residual == 0)) {
#       dat[[iuse]] = dat2[[iuse]] = NULL
#     }
#   }
# 
#   # summarize all ancillary volume and number of wells by state, county and year
#   if (any(names(dat) %in% c("Domestic", "Public supply"))) {
#     dat[["Ancillary"]] = bind_rows(dat, .id="Use") %>%
#       filter(Use %in% c("Domestic", "Public supply")) %>%
#       group_by(State, County, Year) %>%
#       summarize(Volume = sum(Volume, na.rm=T),
#                 Count = round(mean(Count)), .groups="drop") %>%
#       arrange(Year)
#     temp = which(names(dat) %in% c("Domestic", "Public supply")) %>%
#       tail(1)
#     dat %<>%
#       .[c(1:temp, length(dat), (temp + 1):(length(dat) - 1))]
#   }
#   if (any(names(dat2) %in% c("Domestic", "Public supply"))) {
#     dat2[["Ancillary"]] = bind_rows(dat2, .id="Use") %>%
#       filter(Use %in% c("Domestic", "Public supply")) %>%
#       group_by(State, County, Year) %>%
#       summarize(Volume = sum(Volume, na.rm=T),
#                 Count = round(mean(Count)), .groups="drop") %>%
#       arrange(Year)
#     temp = which(names(dat2) %in% c("Domestic", "Public supply")) %>%
#       tail(1)
#     dat2 %<>%
#       .[c(1:temp, length(dat2), (temp + 1):(length(dat2) - 1))]
#   }
# 
#   # make ancillary water-use input data by use for water withdrawn from sources or distributed to wells
#   dat %<>%
#     ldply(data.frame, .id="Use") %>%
#     mutate(Case = "DistributedtoWells") %>%
#     as_tibble()
#   dat2 %<>%
#     ldply(data.frame, .id="Use") %>%
#     mutate(Case = "WithdrawnfromSources") %>%
#     as_tibble()
#   datanc = bind_rows(dat, dat2) %>%
#     mutate(Use = as.character(Use),
#            Volume = round(Volume, 6))
#   write_rds(datanc, "Data/datanc.rds")
# }
# 
# 
# #  ------------------------------------------------------------------------#
# # A.4. wrangle population data----
# #  ------------------------------------------------------------------------#
# 
# 
# # make function with following arguments:
# #   data = estimated population data as data frame;
# #   data2 = counted well data as data frame;
# #   brkpt = estimated breakpoint from ".model_breakpoint" function as numeric vector
# .wrangle_population = function(data=datp,
#                                data2=datfd,
#                                brkpt=.brkpt) {
# 
#   if (any(sapply(list(data, data2, brkpt), is.null))) stop("one or more necessary arguments missing")
# 
#   # get state-county combinations with at least 1 well for years following estimated breakpoint
#   stcnty = data2 %>%
#     filter(Year >= brkpt) %>%
#     select(State, County) %>%
#     distinct()
# 
#   # get population estimates for state-county combinations associated with COG
#   datpop = data %>%
#     filter(State %in% stcnty$State & County %in% stcnty$County) %>%
#     group_by(State, Year) %>%
#     summarize(Persons = sum(Persons, na.rm=T), .groups="drop")
# 
#   # make population estimates by state and year for post-COG years with number of wells lagged by zero through three years
#   dat = data2 %>%
#     group_by(State, Year) %>%
#     summarize(Wells = sum(Count, na.rm=T), .groups="drop")
#   datpop %<>%
#     right_join(dat, by=c("State", "Year")) %>%
#     mutate(Wells1 = lag(Wells, 1),
#            Wells2 = lag(Wells, 2),
#            Wells3 = lag(Wells, 3),
#            Wells4 = lag(Wells, 4),
#            Wells5 = lag(Wells, 5)) %>%
#     rename(Wells0 = Wells) %>%
#     filter(Year >= brkpt)
# 
#   # make population data
#   write_rds(datpop, "Data/datpop.rds")
# }
