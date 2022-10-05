#  ------------------------------------------------------------------------#
# visualize model output----
#  ------------------------------------------------------------------------#



#  ------------------------------------------------------------------------#
# make tables of water use from linear and quantile regression models----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".visualize" function:
#   data = water-use model as data frame;
.table_model <- function(data = data) {

  # get parameter estimates and confidence intervals
  dat <- list()
  for (iuse in names(data)) {
    dat[[iuse]] <- modify_depth(data[[iuse]], .depth = 1, .f = "Parameters") %>%
      as.data.frame() %>%
      filter(Mean.term == "Count") %>%
      select(-matches(paste(c("term", "std.error", "statistic", "p.value", "tau", "conlev"), collapse = "|")),
             -matches(paste(c("[[:digit:]]+.YearMonth", "[[:digit:]]+.conf"), collapse = "|")))
  }
  dat %<>%
    ldply(data.frame, .id = "Use") %>%
    rename(YearMonth = Mean.YearMonth)
  dat %<>% mutate(across(where(is.numeric), function(x) round(x, 6)))
  dat %<>% mutate(Mean.conf.low = ifelse(Mean.conf.low > Mean.estimate, Mean.estimate, Mean.conf.low),
                  Mean.conf.high = ifelse(Mean.conf.high < Mean.estimate, Mean.estimate, Mean.conf.high),
                  P5.estimate = ifelse(P5.estimate > P50.estimate, P50.estimate, P5.estimate),
                  P95.estimate = ifelse(P95.estimate < P50.estimate, P50.estimate, P95.estimate))

  # get cross-validated goodness-of-fit metrics
  dat2 <- list()
  for (iuse in names(data)) {
    dat2[[iuse]] <- data[[iuse]]$Mean$GoodnessofFit
    # dat2[[iuse]] <- modify_depth(data[[iuse]], .depth = 1, .f = "GoodnessofFit") %>%
    # as.data.frame()
  }
  dat2 %<>% ldply(data.frame, .id = "Use")
  dat2 %<>%
    mutate(across(where(is.numeric), function(x) round(x, 3))) %>%
    mutate(across(contains(c("Rsq", "RSR")), function(x) round(x, 2)))

  # get predicted values and confidence intervals
  dat3 <- list()
  for (iuse in names(data)) {
    dat3[[iuse]] <- modify_depth(data[[iuse]], .depth = 1, .f = "Predictions") %>%
      as.data.frame() %>%
      select(-matches(paste(c("Volume", ".se.fit", ".resid", ".hat", ".sigma", ".cooksd", ".std.resid", ".tau"), collapse = "|")),
             -matches(paste(c("[[:digit:]]+.Count", "[[:digit:]]+.YearMonth", "[[:digit:]]+.Subwatershed", "[[:digit:]]+.State"), collapse = "|")))
  }
  dat3 %<>%
    ldply(data.frame) %>%
    rename(Use = .id,
           Count = Mean.Count,
           YearMonth = Mean.YearMonth,
           Subwatershed = Mean.Subwatershed)
  names(dat3) %<>% str_replace(fixed(".."), ".")
  dat3 %<>%
    select(Subwatershed, Use, YearMonth, everything()) %>%
    mutate(across(where(is.numeric), function(x) round(x, 6)))

  # make tables of parameter estimates and confidence intervals, cross-validated goodness-of-fit metrics, and predicted values and confidence intervals
  dat[1, "Units"] <- "in million gallons of water use per well"
  write_excel_csv(dat, "Product/coefficients.csv", na = "")
  dat2[1, "Units"] <- "in million gallons of water use per well (except Rsq as ratio scaled from 0 to 1, RSR as ratio scaled from 0 to infinity)"
  write_excel_csv(dat2, "Product/performances.csv", na = "")
  dat3[1, "Units"] <- "in million gallons of water use per Subwatershed and YearMonth"
  write_excel_csv(dat3, "Product/predictions.csv", na = "")
}


#  ------------------------------------------------------------------------#
# make figures of water use from linear and quantile regression models----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".visualize" function:
#   data = water-use model as data frame;
#   use = use of data as character vector
.figure_model <- function(data = data,
                          use = use) {

  # for each parameter
  dat <- list()
  for (iparm in names(data)) {

    # get model
    mod <- data[[iparm]]$Model
    conlev <- data[[iparm]]$Parameters$conlev %>%
      unique()
    if (iparm == "Mean") {

      # get predicted values and confidence intervals
      dat[[iparm]] <- augment(mod$All, se_fit = T) %>%
        mutate(.conf.low = family(mod$All)$linkinv(.fitted + qnorm((1 - conlev) / 2) * .se.fit),
               .conf.high = family(mod$All)$linkinv(.fitted - qnorm((1 - conlev) / 2) * .se.fit))
    } else {
      set.seed(.seed)
      dat[[iparm]] <- augment(mod$All, type = "percentile", interval = "confidence", level = conlev, se = "boot", R = 100) %>%
        rename(.conf.low = .lower,
               .conf.high = .upper)
    }

    # drop unnecessary variables and change negative values to zero
    if (iparm == "Mean") {
      dat[[iparm]] %<>%
        select(Volume, Count, .fitted, .conf.low, .conf.high) %>%
        mutate(across(contains(fixed(".")), function(x) ifelse(x < 0, 0, x)),
               YearMonth = data[[iparm]]$Predictions$YearMonth,
               Subwatershed = data[[iparm]]$Predictions$Subwatershed) %>%
        select(YearMonth, Subwatershed, everything())
    }
  }

  # get range of x variable for axis breaks
  xbrk <- range(dat$Mean$Count, na.rm = T) %>%
    pretty()

  # for each parameter
  for (iparm in names(dat)) {

    # get model
    mod <- data[[iparm]]$Model

    # make predictions at range of x variable for plotting lines and ribbon
    if (iparm == "Mean") {
      pred <- predict(mod$All, newdata = data.frame(Count = range(xbrk)), se.fit = T)
      temp <- data.frame(YearMonth = NA,
                         Subwatershed = NA,
                         Volume = rep(NA, times = 2),
                         Count = range(xbrk),
                         .fitted = pred$fit,
                         .conf.low = family(mod$All)$linkinv(pred$fit + qnorm((1 - conlev) / 2) * pred$se.fit),
                         .conf.high = family(mod$All)$linkinv(pred$fit - qnorm((1 - conlev) / 2) * pred$se.fit))
      dat[[iparm]] %<>% bind_rows(temp)
    } else {
      pred <- predict(mod$All, newdata = data.frame(Count = range(xbrk)))
      temp <- data.frame(YearMonth = NA,
                         Subwatershed = NA,
                         Volume = rep(NA, times = 2),
                         Count = range(xbrk),
                         .fitted = pred,
                         .conf.low = rep(NA, times = 2),
                         .conf.high = rep(NA, times = 2))
      dat[[iparm]] %<>% bind_rows(temp)
    }
  }

  # get range of y variables for axis breaks
  ybrk <- dat$Mean %>%
    select(-YearMonth, -Subwatershed, -Count) %>%
    max(na.rm = T) %>%
    c(dat %>%
        bind_rows(.id = "Parameter") %>%
        filter(Parameter != "Mean") %>%
        select(.fitted) %>%
        max()) %>%
    max() %>%
    c(0, .) %>%
    pretty()

  # make changes to axis labels
  if (any(grepl("\\.", xbrk))) {
    xlab <- waiver()
  } else {
    xlab <- function(x) comma(x)
  }
  if (any(grepl("\\.", ybrk))) {
    ylab <- waiver()
  } else {
    ylab <- function(x) comma(x)
  }

  # get parameter, confidence level and use for guide or title
  parm <- ifelse(str_detect(names(dat), "[[:digit:]]"), paste0(str_extract(names(dat), "[[:digit:]]+"), "th percentile"), names(dat)) %>%
    str_to_lower() %>%
    paste("Estimated", .)
  conlev <- paste0(conlev * 100, "-percent confidence interval")

  # combine data frames
  names(dat) <- parm
  dat %<>% bind_rows(.id = "Parameter")

  # make additional data frame for plotting ribbon
  dat2 <- dat %>%
    filter(Parameter == "Estimated mean")

  # make scale breaks and values, and palette for guide
  brk <- ifelse(str_detect(parm, "[[:digit:]]"), str_extract(parm, "[[:digit:]]+"), parm)
  brk %<>%
    .[str_detect(., "[[:digit:]]")] %>%
    as.numeric() %>%
    sort(decreasing = T) %>%
    paste0("Estimated ", ., "th percentile") %>%
    c("Estimated mean", .)
  pal <- brewer_pal(type = "seq", palette = "Blues")(4)[-c(1, 2)]
  pal2 <- brewer_pal(type = "seq", palette = "Oranges")(min(max(length(brk), 5), 9))[-1]
  pal2 <- gradient_n_pal(pal2)(seq(0, 1, length.out = length(brk) - 1))
  brk2 <- c(conlev, brk, "Sampling unit")
  val <- c(pal, pal2, "black")
  names(val) <- brk2

  # make figure of water use against wells
  ggp <- ggplot(dat) +
    geom_ribbon(data = dat2, aes(Count, ymin = .conf.low, ymax = .conf.high, fill = conlev), show.legend = F) +
    geom_line(aes(Count, .fitted, color = conlev), size = NA) +
    geom_line(aes(Count, .fitted, color = Parameter), size = 0.3) +
    geom_point(aes(Count, Volume, color = "Sampling unit"), size = 0.3) +
    scale_x_continuous(breaks = xbrk, labels = xlab, position = "bottom", sec.axis = sec_axis(~ ., breaks = xbrk, labels = NULL)) +
    scale_y_continuous(breaks = ybrk, labels = ylab, position = "left", sec.axis = sec_axis(~ ., breaks = ybrk, labels = NULL)) +
    coord_cartesian(xlim = range(xbrk), ylim = range(ybrk), expand = F) +
    scale_fill_manual(name = NULL, values = pal[1]) +
    scale_color_manual(name = "EXPLANATION", values = val, breaks = brk2) +
    guides(color = guide_legend(override.aes = list(linetype = c(1, rep(1, times = length(brk)), 0), shape = c(NA, rep(NA, times = length(brk)), 16), size = c(3, rep(0.3, times = length(brk)), 0.6)))) +
    labs(title = use, x = "Number of oil and gas wells developed per sampling unit", y = "Water use, in million gallons")
  ggsave(paste0("Product/Model ", use, ".png"), scale = 3, width = 5, height = 3)

  # drop unnecessary values
  dat2 %<>% filter(!is.na(Volume))

  # get range of x and y variables for plotting
  xbrk <- ybrk <- dat2 %>%
    select(Volume, .fitted) %>%
    max(na.rm = T) %>%
    c(0, .) %>%
    pretty()

  # make changes to axis labels
  if (any(grepl("\\.", xbrk))) {
    xlab <- waiver()
  } else {
    xlab <- function(x) comma(x)
  }
  if (any(grepl("\\.", ybrk))) {
    ylab <- waiver()
  } else {
    ylab <- function(x) comma(x)
  }

  # make figure of water use predictions against observations
  ggp <- ggplot(dat2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 0.3, show.legend = F) +
    geom_line(aes(0, 0, color = "1:1 line"), linetype = "dashed", size = 0.3) +
    geom_point(aes(Volume, .fitted, color = "Sampling unit"), size = 0.3) +
    scale_x_continuous(breaks = xbrk, labels = xlab, position = "bottom", sec.axis = sec_axis(~ ., breaks = xbrk, labels = NULL)) +
    scale_y_continuous(breaks = ybrk, labels = ylab, position = "left", sec.axis = sec_axis(~ ., breaks = ybrk, labels = NULL)) +
    coord_cartesian(xlim = range(xbrk), ylim = range(ybrk), expand = F) +
    scale_color_manual(name = "EXPLANATION", values = c("black", "black"), breaks = c("Sampling unit", "1:1 line")) +
    guides(color = guide_legend(override.aes = list(linetype = c(0, 2), shape = c(16, NA), size = c(0.6, 0.3)))) +
    labs(title = use, x = "Observed water use, in million gallons", y = "Estimated water use, in million gallons") +
    theme(aspect.ratio = 1)
  ggsave(paste0("Product/Fit ", use, ".png"), scale = 3, width = 2.5, height = 3)
}



#  ------------------------------------------------------------------------#
# visualize water-use model output----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = water-use parameters from ".model" function as list;
.visualize <- function(data = mod) {

  if (any(sapply(list(data), is.null))) stop("one or more necessary arguments missing")

  # make tables of parameter estimates and confidence intervals, cross-validated goodness-of-fit metrics, and predicted values and confidence intervals
  .table_model(data = data)

  # for each use
  for (iuse in names(data)) {

    # make figures of predicted values against wells, and predicted against observed values
    .figure_model(data = data[[iuse]],
                  use = iuse)
  }
}


#  ------------------------------------------------------------------------#
# visualize water-use model output for data release----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = water-use predictions from ".model" function as data frame;
#   data2 = water-use coefficients from ".model" function as data frame;
.visualize_data_release <- function(data = pred,
                                    data2 = coef) {

  if (any(sapply(list(data, data2), is.null))) stop("one or more necessary arguments missing")

  # make data release file of predicted values and confidence intervals
  dat <- data %>%
    filter(Use %in% c("Direct", "Indirect"))
  dat$Use %<>% recode("Direct" = "Total",
                      "Indirect" = "Total")
  dat %<>%
    select(-Count, -Units) %>%
    group_by(Subwatershed, Use, YearMonth) %>%
    summarize(across(everything(), sum), .groups = "drop")
  dat %<>%
    select(Subwatershed, YearMonth, Use, everything()) %>%
    mutate(across(contains("."), function(x) {round(x, 6)}))
  names(dat) %<>%
    str_replace(".fitted", "") %>%
    str_replace("conf.low", "Lower") %>%
    str_replace("conf.high", "Upper") %>%
    str_replace("Mean.", "")
  dat$Use %<>% recode("Total" = paste0("COG mining", "\U2014", "Direct and indirect"))
  write_excel_csv(dat, "Product/data_release_predictions.csv")

  # make data release file of parameter estimates and confidence intervals
  dat <- data2 %>%
    filter(Use == "Direct")
  dat$Use %<>% recode("Direct" = "Total")
  dat %<>%
    select(-Units) %>%
    group_by(Use, YearMonth) %>%
    summarize(across(everything(), sum), .groups = "drop") %>%
    arrange(match(YearMonth, "All"))
  temp <- data2
  dat %<>%
    bind_rows(temp, .) %>%
    select(-Units) %>%
    arrange(match(Use, c("Direct", "Fracturing", "Cementing", "Drilling"))) %>%
    mutate(across(contains("."), function(x) {round(x, 6)}))
  names(dat) %<>%
    str_replace(".estimate", "") %>%
    str_replace("conf.low", "Lower") %>%
    str_replace("conf.high", "Upper") %>%
    str_replace("Mean.", "")
  dat$Use %<>% recode("Direct" = paste0("Direct", "\U2014", "All"),
                      "Fracturing" = paste0("Direct", "\U2014", "Fracturing"),
                      "Cementing" = paste0("Direct", "\U2014", "Cementing"),
                      "Drilling" = paste0("Direct", "\U2014", "Drilling"),
                      "Total" = paste0("Direct and indirect", "\U2014", "COG mining"))
  write_excel_csv(dat, "Product/data_release_coefficients.csv")
}
