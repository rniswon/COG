#  ------------------------------------------------------------------------#
# wrangle input data----
#  ------------------------------------------------------------------------#



#  ------------------------------------------------------------------------#
# wrangle water-use input data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct and indirect water-use input data for fracking, cementing, drilling or indirect uses as data frames;
.wrangle <- function(data = list(data)) {

  if (any(sapply(list(data), is.null))) stop("one or more necessary arguments missing")

  # get all water-use input data
  data %<>% reduce(full_join)

  # for each use
  if ("Fracturing" %in% unique(data$Use)) {
    data %<>% arrange(match(Use,  "Fracturing"))
  }
  dat <- list()
  for (iuse in unique(data$Use)) {

    # summarize each water use volume and number of wells by state, Subwatershed and YearMonth
    dat[[iuse]] <- data %>%
      filter(Use == iuse) %>%
      select(-Use) %>%
      group_by(Subwatershed, YearMonth) %>%
      summarize(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      arrange(YearMonth)

    # apply year average by Subwatershed of drilling, cementing and indirect water use per well to wells in YearMonths and Subwatersheds with only hydraulic fracturing water use
    if (iuse %in% c("Cementing", "Drilling", "Indirect") & "Fracturing" %in% unique(data$Use)) {
      datUse <- dat[[iuse]]
      datf <- dat[["Fracturing"]]
      IN_datf_AND_datUse <- inner_join(datf, datUse, by = c("Subwatershed", "YearMonth"))
      if (any(!c(nrow(datf), nrow(datUse)) %in% nrow(IN_datf_AND_datUse))) {

        # linearly interpolate values of drilling, cementing and indirect water use for YearMonths and Subwatersheds with missing data
        datUse_IN_datf <- semi_join(datUse, datf, by = c("Subwatershed")) %>%
          mutate(Volume = Volume / Count)
        datSub <- list()
        for (isub in unique(datUse_IN_datf$Subwatershed)) {
          datUse_IN_datfSub <- datUse_IN_datf %>%
            filter(Subwatershed == isub)
          if (all(unique(datUse_IN_datf$YearMonth) %in% unique(datUse_IN_datfSub$YearMonth))) {
            datSub[[isub]] <- datUse_IN_datfSub
          } else {
            temp <- data.frame(Subwatershed = isub,
                               YearMonth = unique(datUse_IN_datf$YearMonth)[!unique(datUse_IN_datf$YearMonth) %in% unique(datUse_IN_datfSub$YearMonth)],
                               Volume = NA,
                               Count = NA)
            datUse_IN_datfSub %<>%
              bind_rows(temp) %>%
              arrange(YearMonth)
            if (length(na.omit(datUse_IN_datfSub$Volume)) == 0) {
              datUse_IN_datfSub$Volume <- 0
            } else if (length(na.omit(datUse_IN_datfSub$Volume)) == 1) {
              datUse_IN_datfSub$Volume <- unique(na.omit(datUse_IN_datfSub$Volume))
            } else {
              ym <- as.numeric(str_sub(datUse_IN_datfSub$YearMonth, 1, 4)) + as.numeric(str_sub(datUse_IN_datfSub$YearMonth, 5, 6)) / 12 - 1 / 12
              datUse_IN_datfSub %<>%
                mutate(x = cumsum(c(0, diff(ym))),
                       Volume = approx(x, Volume, x, rule = 2)$y) %>%
                select(-x)
            }
            datSub[[isub]] <- datUse_IN_datfSub
          }
        }
        datSub %<>%
          bind_rows()
        datUse_IN_datfYM <- datUse_IN_datf %>%
          group_by(YearMonth) %>%
          summarize(Rate = mean(Volume, na.rm = TRUE), .groups = "drop")
        datf_NIN_datUse <- anti_join(datf, datUse, by = c("Subwatershed"))
        if (any(!datf_NIN_datUse$YearMonth %in% datUse_IN_datfYM$YearMonth)) {
          temp <- data.frame(YearMonth = unique(datf_NIN_datUse$YearMonth)[!unique(datf_NIN_datUse$YearMonth) %in% unique(datUse_IN_datfYM$YearMonth)],
                             Rate = NA)
          datUse_IN_datfYM %<>%
            bind_rows(temp) %>%
            arrange(YearMonth)
          if (length(na.omit(datUse_IN_datfYM$Rate)) == 0) {
            datUse_IN_datfYM$Rate <- 0
          } else if (length(na.omit(datUse_IN_datfYM$Rate)) == 1) {
            datUse_IN_datfYM$Rate <- unique(na.omit(datUse_IN_datfYM$Rate))
          } else {
            ym <- as.numeric(str_sub(datUse_IN_datfYM$YearMonth, 1, 4)) + as.numeric(str_sub(datUse_IN_datfYM$YearMonth, 5, 6)) / 12 - 1 / 12
            datUse_IN_datfYM %<>%
              mutate(x = cumsum(c(0, diff(ym))),
                     Rate = approx(x, Rate, x, rule = 2)$y) %>%
              select(-x)
          }
        }
        datf_NIN_datUse %<>%
          mutate(Volume = datUse_IN_datfYM$Rate[match(YearMonth, datUse_IN_datfYM$YearMonth)]) %>%
          full_join(datSub, .) %>%
          inner_join(datf, ., by = c("Subwatershed", "YearMonth")) %>%
          mutate(Volume = Volume.y * Count.x) %>%
          rename(Count = Count.x) %>%
          select(Subwatershed, YearMonth, Volume, Count)
      }
      dat[[iuse]] <- datf_NIN_datUse
    }
  }

  # summarize all direct water use volume and number of wells by Subwatershed and YearMonth
  if (!"Direct" %in% unique(data$Use)) {
    dat[["Direct"]] <- reduce(discard(dat, names(dat) %in% c("Indirect")), full_join) %>%
      group_by(Subwatershed, YearMonth) %>%
      summarize(Volume = sum(Volume, na.rm = TRUE),
                Count = first(Count), .groups = "drop") %>%
      arrange(YearMonth)
  }

  # make water-use input data by use
  dat %<>%
    ldply(data.frame) %>%
    rename(Use = .id) %>%
    mutate(Volume = round(Volume, 6)) %>%
    as_tibble()
  write_rds(dat, "Data/dat.rds")
  return(dat)
}
