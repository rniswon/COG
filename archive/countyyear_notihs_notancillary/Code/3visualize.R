#  ------------------------------------------------------------------------#
# visualize model output----
#  ------------------------------------------------------------------------#



#  ------------------------------------------------------------------------#
# make tables of water use from linear and quantile regression models----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".visualize_*" functions:
#   data = water-use model as data frame;
#   catg = category of analysis as character vector
.table_model = function(data=data,
                        catg=catg) {

  # get parameter estimates and confidence intervals
  dat = list()
  for (iuse in names(data)) {
    dat[[iuse]] = modify_depth(data[[iuse]], .depth=1, .f="Parameters") %>%
      as.data.frame() %>%
      filter(Mean.term == "Count") %>%
      select(-matches(paste(c("term", "std.error", "statistic", "p.value", "tau", "conlev"), collapse="|")),
             -matches(paste(c("[[:digit:]]+.Year", "[[:digit:]]+.conf"), collapse="|")))
  }
  dat %<>%
    ldply(data.frame, .id="Use") %>%
    rename(Year = Mean.Year)
  dat %<>% mutate(across(where(is.numeric), function(x) round(x, 6)))
  dat %<>% mutate(Mean.conf.low = ifelse(Mean.conf.low > Mean.estimate, Mean.estimate, Mean.conf.low),
                  Mean.conf.high = ifelse(Mean.conf.high < Mean.estimate, Mean.estimate, Mean.conf.high),
                  P5.estimate = ifelse(P5.estimate > P50.estimate, P50.estimate, P5.estimate),
                  P95.estimate = ifelse(P95.estimate < P50.estimate, P50.estimate, P95.estimate))

  # get cross-validated goodness-of-fit metrics
  dat2 = list()
  for (iuse in names(data)) {
    dat2[[iuse]] = modify_depth(data[[iuse]], .depth=1, .f="GoodnessofFit") %>%
      as.data.frame()
  }
  dat2 %<>% ldply(data.frame, .id="Use")
  dat2 %<>%
    mutate(across(where(is.numeric), function(x) round(x, 3))) %>%
    mutate(across(contains(c("Rsq", "RSR")), function(x) round(x, 2)))

  # get predicted values and confidence intervals
  dat3 = list()
  for (iuse in names(data)) {
    dat3[[iuse]] = modify_depth(data[[iuse]], .depth=1, .f="Predictions") %>%
      as.data.frame() %>%
      select(-matches(paste(c("Volume", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd", ".std.resid", ".tau"), collapse="|")),
             -matches(paste(c("[[:digit:]]+.Count", "[[:digit:]]+.Year", "[[:digit:]]+.County", "[[:digit:]]+.State"), collapse="|")))
  }
  dat3 %<>%
    ldply(data.frame, .id="Use") %>%
    rename(Count = Mean.Count,
           Year = Mean.Year,
           County = Mean.County,
           State = Mean.State)
  names(dat3) %<>% str_replace(fixed(".."), ".")
  if (catg == "ancillary") {
    dat3 %<>%
      select(-Count) %>%
      arrange(match(Use, c("Domestic", "Public supply", "Ancillary", "Industrial", "Mining", "Thermoelectric power")))
  }
  dat3 %<>%
    select(State, County, Use, Year, everything()) %>%
    mutate(across(where(is.numeric), function(x) round(x, 6)))

  # make tables of parameter estimates and confidence intervals, cross-validated goodness-of-fit metrics, and predicted values and confidence intervals
  dat[1, "Units"] = "in million gallons of water use per well"
  write_excel_csv(dat, paste0("Product/coefficients_", catg, ".csv"), na="")
  dat2[1, "Units"] = "in million gallons of water use per well (except Rsq as ratio scaled from 0 to 1, RSR as ratio scaled from 0 to infinity)"
  write_excel_csv(dat2, paste0("Product/performances_", catg, ".csv"), na="")
  dat3[1, "Units"] = "in million gallons of water use per county and year"
  write_excel_csv(dat3, paste0("Product/predictions_", catg, ".csv"), na="")
}


#  ------------------------------------------------------------------------#
# make figures of water use from linear and quantile regression models----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".visualize_*" functions:
#   data = water-use model as data frame;
#   use = use of data as character vector
.figure_model = function(data=data,
                         use=use) {

  # for each parameter
  dat = list()
  for (iparm in names(data)) {

    # get model
    mod = data[[iparm]]$Model
    conlev = data[[iparm]]$Parameters$conlev %>%
      unique()
    if (iparm == "Mean") {

      # get predicted values and confidence intervals
      dat[[iparm]] = augment(mod$All, se_fit=T) %>%
        mutate(.conf.low = family(mod$All)$linkinv(.fitted + qnorm((1 - conlev) / 2) * .se.fit),
               .conf.high = family(mod$All)$linkinv(.fitted - qnorm((1 - conlev) / 2) * .se.fit))
    } else {
      set.seed(.seed)
      dat[[iparm]] = augment(mod$All, type="percentile", interval="confidence", level=conlev, se="boot", R=100) %>%
        rename(.conf.low = .lower,
               .conf.high = .upper)
    }

    # drop unnecessary variables and change negative values to zero
    dat[[iparm]] %<>%
      select(Volume, Count, .fitted, .conf.low, .conf.high) %>%
      mutate(across(contains(fixed(".")), function(x) ifelse(x < 0, 0, x)),
             Year = data[[iparm]]$CrossValidation$Year,
             County = data[[iparm]]$CrossValidation$County) %>%
      select(Year, County, everything())
  }

  # get range of x variable for axis breaks
  xbrk = range(dat$Mean$Count, na.rm=T) %>%
    pretty()

  # for each parameter
  for (iparm in names(dat)) {

    # get model
    mod = data[[iparm]]$Model

    # make predictions at range of x variable for plotting lines and ribbon
    if (iparm == "Mean") {
      pred = predict(mod$All, newdata=data.frame(Count = range(xbrk)), se.fit=T)
      temp = data.frame(Year = NA,
                        County = NA,
                        Volume = rep(NA, times=2),
                        Count = range(xbrk),
                        .fitted = pred$fit,
                        .conf.low = family(mod$All)$linkinv(pred$fit + qnorm((1 - conlev) / 2) * pred$se.fit),
                        .conf.high = family(mod$All)$linkinv(pred$fit - qnorm((1 - conlev) / 2) * pred$se.fit))
      dat[[iparm]] %<>% bind_rows(temp)
    } else {
      pred = predict(mod$All, newdata=data.frame(Count = range(xbrk)))
      temp = data.frame(Year = NA,
                        County = NA,
                        Volume = rep(NA, times=2),
                        Count = range(xbrk),
                        .fitted = pred,
                        .conf.low = rep(NA, times=2),
                        .conf.high = rep(NA, times=2))
      dat[[iparm]] %<>% bind_rows(temp)
    }
  }

  # get range of y variables for axis breaks
  ybrk = dat$Mean %>%
    select(-Year, -County, -Count) %>%
    max(na.rm=T) %>%
    c(dat %>%
        bind_rows(.id="Parameter") %>%
        filter(Parameter != "Mean") %>%
        select(.fitted) %>%
        max()) %>%
    max() %>%
    c(0, .) %>%
    pretty()

  # make changes to axis labels
  if (any(grepl("\\.", xbrk))) {
    xlab = waiver()
  } else {
    xlab = function(x) comma(x)
  }
  if (any(grepl("\\.", ybrk))) {
    ylab = waiver()
  } else {
    ylab = function(x) comma(x)
  }

  # get parameter, confidence level and use for guide or title
  parm = ifelse(str_detect(names(dat), "[[:digit:]]"), paste0(str_extract(names(dat), "[[:digit:]]+"), "th percentile"), names(dat)) %>%
    str_to_lower() %>%
    paste("Estimated", .)
  conlev = paste0(conlev * 100, "-percent confidence interval")
  if (use == "Mining") {
    use = "Non-COG mining"
  }

  # combine data frames
  names(dat) = parm
  dat %<>% bind_rows(.id="Parameter")

  # make additional data frame for plotting ribbon
  dat2 = dat %>%
    filter(Parameter == "Estimated mean")

  # make scale breaks and values, and palette for guide
  brk = ifelse(str_detect(parm, "[[:digit:]]"), str_extract(parm, "[[:digit:]]+"), parm)
  brk %<>%
    .[str_detect(., "[[:digit:]]")] %>%
    as.numeric() %>%
    sort(decreasing=T) %>%
    paste0("Estimated ", ., "th percentile") %>%
    c("Estimated mean", .)
  pal = brewer_pal(type="seq", palette="Blues")(4)[-c(1, 2)]
  pal2 = brewer_pal(type="seq", palette="Oranges")(min(max(length(brk), 5), 9))[-1]
  pal2 = gradient_n_pal(pal2)(seq(0, 1, length.out=length(brk) - 1))
  brk2 = c(conlev, brk, "Sampling unit")
  val = c(pal, pal2, "black")
  names(val) = brk2

  # make figure of water use against wells
  ggp = ggplot(dat) +
    geom_ribbon(data=dat2, aes(Count, ymin=.conf.low, ymax=.conf.high, fill=conlev), show.legend=F) +
    geom_line(aes(Count, .fitted, color=conlev), size=NA) +
    geom_line(aes(Count, .fitted, color=Parameter), size=0.3) +
    geom_point(aes(Count, Volume, color="Sampling unit"), size=0.3) +
    scale_x_continuous(breaks=xbrk, labels=xlab, position="bottom", sec.axis=sec_axis(~ ., breaks=xbrk, labels=NULL)) +
    scale_y_continuous(breaks=ybrk, labels=ylab, position="left", sec.axis=sec_axis(~ ., breaks=ybrk, labels=NULL)) +
    coord_cartesian(xlim=range(xbrk), ylim=range(ybrk), expand=F) +
    scale_fill_manual(name=NULL, values=pal[1]) +
    scale_color_manual(name="EXPLANATION", values=val, breaks=brk2) +
    guides(color=guide_legend(override.aes=list(linetype=c(1, rep(1, times=length(brk)), 0), shape=c(NA, rep(NA, times=length(brk)), 16), size=c(3, rep(0.3, times=length(brk)), 0.6)))) +
    labs(title=use, x="Number of oil and gas wells developed per sampling unit", y="Water use, in million gallons")
  ggsave(paste0("Product/Model ", use, ".svg"), width=5, height=3)

  # drop unnecessary values
  dat2 %<>% filter(!is.na(Volume))

  # get range of x and y variables for plotting
  xbrk = ybrk = dat2 %>%
    select(Volume, .fitted) %>%
    max(na.rm=T) %>%
    c(0, .) %>%
    pretty()

  # make changes to axis labels
  if (any(grepl("\\.", xbrk))) {
    xlab = waiver()
  } else {
    xlab = function(x) comma(x)
  }
  if (any(grepl("\\.", ybrk))) {
    ylab = waiver()
  } else {
    ylab = function(x) comma(x)
  }

  # make figure of water use predictions against observations
  ggp = ggplot(dat2) +
    geom_abline(intercept=0, slope=1, linetype="dashed", color="black", size=0.3, show.legend=F) +
    geom_line(aes(0, 0, color="1:1 line"), linetype="dashed", size=0.3) +
    geom_point(aes(Volume, .fitted, color="Sampling unit"), size=0.3) +
    scale_x_continuous(breaks=xbrk, labels=xlab, position="bottom", sec.axis=sec_axis(~ ., breaks=xbrk, labels=NULL)) +
    scale_y_continuous(breaks=ybrk, labels=ylab, position="left", sec.axis=sec_axis(~ ., breaks=ybrk, labels=NULL)) +
    coord_cartesian(xlim=range(xbrk), ylim=range(ybrk), expand=F) +
    scale_color_manual(name="EXPLANATION", values=c("black", "black"), breaks=c("Sampling unit", "1:1 line")) +
    guides(color=guide_legend(override.aes=list(linetype=c(0, 2), shape=c(16, NA), size=c(0.6, 0.3)))) +
    labs(title=use, x="Observed water use, in million gallons", y="Estimated water use, in million gallons") +
    theme(aspect.ratio=1)
  ggsave(paste0("Product/Fit ", use, ".svg"), width=2.5, height=3)
}



#  ------------------------------------------------------------------------#
# C.1. visualize direct water-use model output----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct water-use parameters from ".model_direct" function as list;
#   catg = category ("direct") of analysis as character vector
.visualize_direct = function(data=moddir,
                             catg="direct") {

  if (any(sapply(list(data, catg), is.null))) stop("one or more necessary arguments missing")

  # make tables of parameter estimates and confidence intervals, cross-validated goodness-of-fit metrics, and predicted values and confidence intervals
  .table_model(data=data,
               catg=catg)

  # for each use
  for (iuse in names(data)) {
    data2 = data[[iuse]]

    # make figures of predicted values against wells, and predicted against observed values
    .figure_model(data=data2,
                  use=iuse)
  }
}


#  ------------------------------------------------------------------------#
# C.2. visualize indirect water-use model output----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = indirect water-use parameters from ".model_indirect" function as list;
#   catg = category ("indirect") of analysis as character vector
.visualize_indirect = function(data=modind,
                               catg="indirect") {

  if (any(sapply(list(data, catg), is.null))) stop("one or more necessary arguments missing")

  # make tables of parameter estimates and confidence intervals, cross-validated goodness-of-fit metrics, and predicted values and confidence intervals
  .table_model(data=data,
               catg=catg)

  # for each use
  for (iuse in names(data)) {
    data2 = data[[iuse]]

    # make figures of predicted values against wells, and predicted against observed values
    .figure_model(data=data2,
                  use=iuse)
  }
}


# #  ------------------------------------------------------------------------#
# # C.3. visualize ancillary water-use model output----
# #  ------------------------------------------------------------------------#
#
#
# # make function with following arguments:
# #   data = ancillary water-use parameters from ".model_ancillary" function as list;
# #   catg = category ("ancillary") of analysis as character vector
# .visualize_ancillary = function(data=modanc,
#                                 catg="ancillary") {
#
#   if (any(sapply(list(data, catg), is.null))) stop("one or more necessary arguments missing")
#
#   # make tables of parameter estimates and confidence intervals, cross-validated goodness-of-fit metrics, and predicted values and confidence intervals
#   .table_model(data=data,
#                catg=catg)
#
#   # for each use
#   for (iuse in names(data)) {
#     data2 = data[[iuse]]
#
#     # make figures of predicted values against wells, and predicted against observed values
#     .figure_model(data=data2,
#                   use=iuse)
#   }
# }
#
#
# #  ------------------------------------------------------------------------#
# # C.4. visualize population model output----
# #  ------------------------------------------------------------------------#
#
#
# # make function with following arguments:
# #   data = population parameters from ".model_population" function as list
# .visualize_population = function(data=modpop) {
#
#   if (any(sapply(list(data), is.null))) stop("one or more necessary arguments missing")
#
#   # make table of parameter estimates and confidence intervals
#   dat = data$Parameters %>%
#     filter(str_detect(term, "Wells")) %>%
#     select(term, estimate, conf.low, conf.high)
#   temp = str_extract(dat$term, "[[:digit:]]")
#   dat %<>% select(-term)
#   dat[1, "Units"] = paste("in persons per well lagged by", temp, "years")
#   write_excel_csv(dat, "Product/population_coefficients.csv")
#
#   # get predicted values and confidence intervals
#   dat = data$Predictions %>%
#     mutate(across(contains(c("Persons", ".fitted", ".conf")), function(x) {x / 1e6}))
#
#   # get range of x and y variables for axis breaks
#   xbrk = unique(dat$Year)
#   ybrk = dat %>%
#     select(Persons, .fitted, .conf.low, .conf.high) %>%
#     range(na.rm=T) %>%
#     pretty()
#
#   # make changes to axis labels
#   if (any(grepl("\\.", ybrk))) {
#     ylab = waiver()
#   } else {
#     ylab = function(x) comma(x)
#   }
#   if (any(grepl("\\.", xbrk))) {
#     xlab = waiver()
#   } else {
#     xlab = function(x) comma(x)
#   }
#
#   # get confidence level for guide
#   conlev = data$Parameters$conlev
#   conlev = paste0(conlev * 100, "-percent confidence interval")
#
#   # make figure of population against year
#   pal = brewer_pal(type="seq", palette="Blues")(4)[-c(1, 2)]
#   ggp = ggplot(dat) +
#     geom_ribbon(aes(Year, ymin=.conf.low, ymax=.conf.high, fill=conlev), show.legend=F) +
#     geom_line(aes(Year, .fitted, color=conlev), size=NA) +
#     geom_line(aes(Year, .fitted, color="Estimated mean"), size=0.3) +
#     geom_point(aes(Year, Persons, color="Observation"), size=0.3) +
#     scale_x_continuous(breaks=xbrk, position="bottom", sec.axis=sec_axis(~ ., breaks=xbrk, labels=NULL)) +
#     scale_y_continuous(breaks=ybrk, labels=ylab, position="left", sec.axis=sec_axis(~ ., breaks=ybrk, labels=NULL)) +
#     coord_cartesian(xlim=range(xbrk), ylim=range(ybrk), expand=F) +
#     scale_fill_manual(name=NULL, values=pal[1]) +
#     scale_color_manual(name="EXPLANATION", values=c(pal, "black"), breaks=c(conlev, "Estimated mean", "Observation")) +
#     guides(color=guide_legend(override.aes=list(linetype=c(1, 1, 0),  shape=c(NA, NA, 16), size=c(3, 0.3, 0.6)))) +
#     labs(x="Year", y="Number of persons, in millions")
#   ggsave("Product/Population Model.svg", width=5, height=2.5)
#
#   # get range of x and y variables for axis breaks
#   xbrk = ybrk = dat %>%
#     select(Persons, .fitted) %>%
#     range(na.rm=T) %>%
#     pretty()
#
#   # make changes to axis labels
#   if (any(grepl("\\.", ybrk))) {
#     ylab = waiver()
#   } else {
#     ylab = function(x) comma(x)
#   }
#   if (any(grepl("\\.", xbrk))) {
#     xlab = waiver()
#   } else {
#     xlab = function(x) comma(x)
#   }
#
#   # make figure of water use predictions against observations
#   ggp = ggplot(dat) +
#     geom_abline(intercept=0, slope=1, linetype="dashed", color="black", size=0.3, show.legend=F) +
#     geom_line(aes(0, 0, color="1:1 line"), linetype="dashed", size=0.3) +
#     geom_point(aes(Persons, .fitted, color="Sampling unit"), size=0.3) +
#     scale_x_continuous(breaks=xbrk, labels=xlab, position="bottom", sec.axis=sec_axis(~ ., breaks=xbrk, labels=NULL)) +
#     scale_y_continuous(breaks=ybrk, labels=ylab, position="left", sec.axis=sec_axis(~ ., breaks=ybrk, labels=NULL)) +
#     coord_cartesian(xlim=range(xbrk), ylim=range(ybrk), expand=F) +
#     scale_color_manual(name="EXPLANATION", values=c("black", "black"), breaks=c("Sampling unit", "1:1 line")) +
#     guides(color=guide_legend(override.aes=list(linetype=c(0, 2), shape=c(16, NA), size=c(0.6, 0.3)))) +
#     labs(x="Observed number of persons, in millions", y="Estimated number of persons, in millions") +
#     theme(aspect.ratio=1)
#   ggsave("Product/Population Fit.svg", width=2.5, height=3)
# }


#  ------------------------------------------------------------------------#
# C.5. visualize water-use model output for data release----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct water-use predictions from ".model_direct" function as data frame;
#   data2 = indirect water-use predictions from ".model_indirect" function as data frame;
#   data3 = ancillary water-use predictions from ".model_ancillary" function as data frame;
#   data4 = direct water-use coefficients from ".model_direct" function as data frame;
#   data5 = indirect water-use coefficients from ".model_indirect" function as data frame;
#   data6 = ancillary water-use coefficients from ".model_ancillary" function as data frame;
.visualize_data_release = function(data=preddir,
                                   data2=predind,
                                   # data3=predanc,
                                   data4=coefdir,
                                   data5=coefind) {
                                   # data5=coefind,
                                   # data6=coefanc) {

  # if (any(sapply(list(data, data2, data3, data4, data5, data6), is.null))) stop("one or more necessary arguments missing")
  if (any(sapply(list(data, data2, data4, data5), is.null))) stop("one or more necessary arguments missing")

  # make data release file of predicted values and confidence intervals
  temp = data %>%
    filter(Use == "Direct")
  temp2 = data2 %>%
    filter(Use == "Indirect")
  temp$Use %<>% recode("Direct" = "Total")
  temp2$Use %<>% recode("Indirect" = "Total")
  dat = bind_rows(temp, temp2) %>%
    select(-Count, -Units) %>%
    group_by(State, County, Use, Year) %>%
    summarize(across(everything(), sum), .groups="drop")
  # temp3 = data3 %>%
  #   filter(Use %in% c("Domestic", "Public supply", "Industrial", "Mining", "Thermoelectric power"))
  dat %<>%
    # bind_rows(temp3) %>%
    # select(State, County, Year, Use, everything(), -Units) %>%
    select(State, County, Year, Use, everything()) %>%
    mutate(across(contains("."), function(x) {round(x, 6)}))
  names(dat) %<>%
    str_replace(".fitted", "") %>%
    str_replace("conf.low", "Lower") %>%
    str_replace("conf.high", "Upper") %>%
    str_replace("Mean.", "")
  dat$Use %<>% recode("Total" = paste0("COG mining", "\U2014", "Direct and indirect"),
                      "Mining" = "Non-COG mining")
  write_excel_csv(dat, "Product/data_release_predictions.csv")

  # make data release file of parameter estimates and confidence intervals
  temp = data4 %>%
    filter(Use == "Direct")
  temp2 = data5 %>%
    filter(Use == "Indirect")
  temp$Use %<>% recode("Direct" = "Total")
  temp2$Use %<>% recode("Indirect" = "Total")
  dat = bind_rows(temp, temp2) %>%
    select(-Units) %>%
    group_by(Use, Year) %>%
    summarize(across(everything(), sum), .groups="drop") %>%
    arrange(match(Year, "All"))
  temp = data4
  temp2 = data5
  # temp3 = data6 %>%
  #   filter(Use %in% c("Domestic", "Public supply", "Ancillary", "Industrial", "Mining", "Thermoelectric power"))
  dat %<>%
    # bind_rows(temp, temp2, ., temp3) %>%
    bind_rows(temp, temp2, .) %>%
    select(-Units) %>%
    # arrange(match(Use, c("Direct", "Hydraulic fracturing", "Cementing", "Drilling", "Indirect", "Total", "Ancillary", "Domestic", "Public supply", "Industrial", "Mining", "Thermoelectric power"))) %>%
    arrange(match(Use, c("Direct", "Hydraulic fracturing", "Cementing", "Drilling", "Indirect", "Total"))) %>%
    mutate(across(contains("."), function(x) {round(x, 6)}))
  names(dat) %<>%
    str_replace(".estimate", "") %>%
    str_replace("conf.low", "Lower") %>%
    str_replace("conf.high", "Upper") %>%
    str_replace("Mean.", "")
  dat$Use %<>% recode("Direct" = paste0("Direct", "\U2014", "All"),
                      "Hydraulic fracturing" = paste0("Direct", "\U2014", "Hydraulic fracturing"),
                      "Cementing" = paste0("Direct", "\U2014", "Cementing"),
                      "Drilling" = paste0("Direct", "\U2014", "Drilling"),
                      "Total" = paste0("Direct and indirect", "\U2014", "COG mining"),
                      "Ancillary" = paste0("Ancillary (population-based)", "\U2014", "All"),
                      "Mining" = "Non-COG mining")
  write_excel_csv(dat, "Product/data_release_coefficients.csv")
}
