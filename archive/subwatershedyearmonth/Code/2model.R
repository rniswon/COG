#  ------------------------------------------------------------------------#
# model data----
#  ------------------------------------------------------------------------#



#  ------------------------------------------------------------------------#
# train models of water use using linear or quantile regression----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".model" function:
#   data = water-use input data as data frame;
#   parm = parameters to model as character vector;
#   conlev = level for confidence interval as numeric vector
.train_model <- function(data = data,
                         parm = parm,
                         conlev = conlev) {

  data$Subwatershed %<>% as.factor()
  data$YearMonth %<>% as.factor()
  if (parm == "Mean") {

    # fit linear regression of volume against number of wells
    mod <- list()
    mod$All <- lm(Volume ~ Count, data = data)

    # get parameter estimates and confidence intervals
    modparm <- list()
    modparm$All <- tidy(mod$All, conf.int = TRUE, conf.level = conlev) %>%
      filter(!term == "(Intercept)")

    # get model summary
    modsumm <- list()
    modsumm$All <- glance(mod$All)

    # do model for each YearMonth
    for (iym in levels(data$YearMonth)) {
      datYM <- data %>%
        filter(YearMonth %in% iym)

      # fit linear regression of volume against number of wells
      mod[[iym]] <- lm(Volume ~ Count, data = datYM)

      # get parameter estimates and confidence intervals
      modparm[[iym]] <- tidy(mod[[iym]], conf.int = TRUE, conf.level = conlev) %>%
        filter(!term == "(Intercept)")

      # get model summary
      modsumm[[iym]] <- glance(mod[[iym]])
    }
    modparm %<>%
      bind_rows(.id = "YearMonth") %>%
      mutate(conlev = conlev)
    modsumm %<>% bind_rows(.id = "YearMonth")

    # get predicted values, residuals and confidence intervals by YearMonth and Subwatershed
    if (length(unique(data$Subwatershed)) == 1 | length(unique(data$YearMonth)) == 1) {
      mod0 <- try(glm(Volume ~ Count, family = Gamma, data = data), silent = TRUE)
      if ("try-error" %in% class(mod0)) {
        mod0 <- glm(Volume ~ Count, family = Gamma, data = data, start = c(0, 1))
      }
    } else {
      mod0 <- try(glm(Volume ~ Count + YearMonth + Subwatershed, family = Gamma, data = data), silent = TRUE)
      if ("try-error" %in% class(mod0)) {
        mod0 <- glm(Volume ~ Count + YearMonth + Subwatershed, family = Gamma, data = data, start = c(0, 1, rep(1, times = length(unique(data$Subwatershed)) - 1 + length(unique(data$YearMonth)) - 1)))
      }
    }
    shp <- summary(mod0)$dispersion
    modpred <- augment(mod0, se_fit = TRUE) %>%
      mutate(.conf.low = family(mod0)$linkinv(.fitted + qgamma((1 - conlev) / 2, ifelse(shp < 1, shp ^ -1, shp)) * .se.fit),
             .conf.high = family(mod0)$linkinv(.fitted - qgamma((1 - conlev) / 2, ifelse(shp < 1, shp ^ -1, shp)) * .se.fit),
             .fitted = family(mod0)$linkinv(.fitted),
             .conf.low = ifelse(.conf.low < 0, 0, .conf.low),
             .conf.high = ifelse(.conf.high < 0, 0, .conf.high))
    if (length(unique(data$Subwatershed)) == 1 | length(unique(data$YearMonth)) == 1) {
      modpred %<>% add_column(YearMonth = data$YearMonth,
                              Subwatershed = data$Subwatershed, .after = "Count")
    }
  } else {

    # fit quantile regression of volume against number of wells
    qile <- str_extract(parm, "[[:digit:]]+") %>%
      as.numeric() %>%
      divide_by(100)
    mod <- list()
    mod$All <- rq(Volume ~ Count, data = data, tau = qile)

    # get parameter estimates and confidence intervals
    modparm <- list()
    set.seed(.seed)
    modparm$All <- tidy(mod$All, se.type = "boot", conf.level = conlev, R = 100) %>%
      filter(!term == "(Intercept)")

    # get model summary
    modsumm <- list()
    modsumm$All <- glance(mod$All) %>%
      type.convert(as.is = TRUE)

    # do model for each YearMonth
    for (iym in levels(data$YearMonth)) {
      datYM <- data %>%
        filter(YearMonth == iym)

      # add zero volume and count observations for singular design matrix errors
      if (nrow(datYM) == 1) {
        datYM %<>% bind_rows(datYM) %>%
          bind_rows(datYM)
        datYM[2:3, c("Volume", "Count")] <- 0
      }
      if (all(datYM$Count == 1)) {
        temp = nrow(datYM)
        datYM %<>% bind_rows(datYM)
        datYM[(temp + 1):(temp * 2), c("Volume", "Count")] <- 0
      }

      # fit quantile regression of volume against number of wells
      qile <- str_extract(parm, "[[:digit:]]+") %>%
        as.numeric() %>%
        divide_by(100)
      mod[[iym]] <- rq(Volume ~ Count, data = datYM, tau = qile)

      # get parameter estimates and confidence intervals
      modparm[[iym]] <- tidy(mod[[iym]], se.type = "rank", conf.level = conlev) %>%
        filter(!term == "(Intercept)")

      # get model summary
      modsumm[[iym]] <- glance(mod[[iym]]) %>%
        type.convert(as.is = TRUE)
    }
    modparm %<>%
      bind_rows(.id = "YearMonth") %>%
      mutate(conlev = conlev)
    modsumm %<>% bind_rows(.id = "YearMonth")

    # get predicted values and residuals by YearMonth and Subwatershed
    if (length(unique(data$Subwatershed)) == 1 | length(unique(data$YearMonth)) == 1) {
      mod0 <- rq(Volume ~ Count, data = data, tau = qile)
    } else {
      mod0 <- rq(Volume ~ Count + YearMonth + Subwatershed, data = data, tau = qile)
    }
    modpred <- augment(mod0, se_fit = TRUE) %>%
      mutate(.fitted = ifelse(.fitted < 0, 0, .fitted))
    if (length(unique(data$Subwatershed)) == 1 | length(unique(data$YearMonth)) == 1) {
      modpred %<>% add_column(YearMonth = data$YearMonth,
                              Subwatershed = data$Subwatershed, .after = "Count")
    }
  }

  # make fitted model, predicted values, model summary and parameter estimates
  dat <- list()
  dat$Parameters <- modparm
  dat$Summary <- modsumm
  dat$Predictions <- modpred
  dat$Model <- mod
  return(dat)
}


#  ------------------------------------------------------------------------#
# test models for predictions of water use with all possible combinations of observations using leave-one-out cross-validation----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".model" function:
#   data = water-use input data as data frame;
#   parm = parameters to model as character vector;
#   conlev = level for confidence interval as numeric vector
.test_model <- function(data = data,
                        parm = parm,
                        conlev = conlev) {

  # for each cross-validated value
  # datcros <- list()
  data %<>% mutate(Subbasin = as.factor(str_sub(Subwatershed, 1, 8)),
                   Year = as.factor(str_sub(YearMonth, 1, 4)))
  data$Subwatershed %<>% as.factor()
  data$YearMonth %<>% as.factor()
  cl <- makeCluster(detectCores() - 4)
  registerDoParallel(cl)
  k <- 5
  r <- 3
  iter <- seq_len(k * r)
  set.seed(.seed)
  idxi <- createMultiFolds(data$Volume, k = k, times = r)
  idxo <- lapply(idxi, function(x) seq_along(data$Volume)[-x])
  writeLines("iteration", con = "log.txt")
  datcros <- foreach(i = iter, .packages = c("magrittr", "dplyr", "stringr", "quantreg")) %dopar% {

    # train model by fitting all but one left-out combination except for singular combinations
    if (nrow(filter(data, Subwatershed == data$Subwatershed[i])) == 1 | nrow(filter(data, YearMonth == data$YearMonth[i])) == 1) {
      traindat <- data
    } else {
      traindat <- data %>%
        slice(idxi[[i]])
    }
    testdat <- data %>%
      slice(idxo[[i]])

    # drop singular combinations for factor new level errors
    if (any(!testdat$Subwatershed %in% traindat$Subwatershed | !testdat$YearMonth %in% traindat$YearMonth)) {
      idx <- which(!testdat$Subwatershed %in% traindat$Subwatershed | !testdat$YearMonth %in% traindat$YearMonth)
      testdat %<>% slice(-idx)
    }

    if (parm == "Mean") {

      # fit linear regression of volume against number of wells by YearMonth and Subwatershed
      if (length(unique(traindat$Subwatershed)) == 1 | length(unique(traindat$YearMonth)) == 1) {
        trainmod <- try(glm(Volume ~ Count, family = Gamma, data = traindat), silent = TRUE)
        if ("try-error" %in% class(trainmod)) {
          trainmod <- glm(Volume ~ Count, family = Gamma, data = traindat, start = c(0, 1))
        }
      } else {
        trainmod <- try(glm(Volume ~ Count + YearMonth + Subwatershed, family = Gamma, data = traindat), silent = TRUE)
        if ("try-error" %in% class(trainmod)) {
          trainmod <- glm(Volume ~ Count + YearMonth + Subwatershed, family = Gamma, data = traindat, start = c(0, 1, rep(1, times = length(unique(traindat$Subwatershed)) - 1 + length(unique(traindat$YearMonth)) - 1)))
        }
      }
    } else {

      # fit quantile regression of volume against number of wells by YearMonth and Subwatershed
      qile <- str_extract(parm, "[[:digit:]]+") %>%
        as.numeric() %>%
        divide_by(100)
      if (length(unique(traindat$Subwatershed)) == 1 | length(unique(traindat$YearMonth)) == 1) {
        trainmod <- rq(Volume ~ Count, data = traindat, tau = qile)
      } else {
        trainmod <- rq(Volume ~ Count + YearMonth + Subwatershed, data = traindat, tau = qile)
      }
    }

    # test model by predicting one left-out combination
    testmod <- predict(trainmod, newdata = testdat, type = "response") %>%
      ifelse(. < 0, 0, .)

    cat(paste(i, "/", tail(iter, 1), "\n"), file = "log.txt", append = TRUE)
    return(data.frame("Observed" = testdat$Volume, "Predicted" = testmod, "Count" = testdat$Count))
  }
  stopCluster(cl)
  datcros <- bind_rows(datcros)

  # get goodness-of-fit metrics for predicted values against observed values except for extreme outliers
  datcros$Predicted[which(abs(datcros$Observed - datcros$Predicted) > mean(datcros$Predicted) + 1.5 * IQR(datcros$Predicted))] <- NA
  rsq <- cor(datcros$Observed, datcros$Predicted, use = "na.or.complete") ^ 2
  rmse <- sqrt(mean((datcros$Predicted - datcros$Observed) ^ 2, na.rm = TRUE)) / mean(datcros$Count, na.rm = TRUE)
  rsr <- rmse / sd(datcros$Observed, na.rm = TRUE) * mean(datcros$Count, na.rm = TRUE)

  # make cross-validated values and goodness-of-fit metrics
  dat <- list()
  dat$GoodnessofFit <- data.frame(Rsq = rsq, RMSE = rmse, RSR = rsr)
  dat$CrossValidation <- as_tibble(datcros)
  return(dat)
}



#  ------------------------------------------------------------------------#
# model water-use input data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct water-use input data as data frame;
#   parm = parameters ("Mean", "P5", "P50", "P95", etc.) to model as character vector;
#   conlev = level for confidence interval as numeric vector
.model <- function(data = dat,
                   parm = c("Mean", "P5", "P50", "P95"),
                   conlev = .conlev) {

  if (any(sapply(list(data, parm, conlev), is.null))) stop("one or more necessary arguments missing")

  # for each use
  moduse <- list()
  for (iuse in unique(data$Use)) {

    # get data for use
    dat <- data %>%
      filter(Use == iuse)

    # for each parameter
    modparm <- list()
    for (iparm in parm) {

      # train model using linear or quantile regression, and test model using leave-one-out cross-validation
      modtrn <- .train_model(data = dat, parm = iparm, conlev = conlev)
      if (iparm == "Mean") {
        modtst <- .test_model(data = dat, parm = iparm, conlev = conlev)
        modparm[[iparm]] <- append(modtrn, modtst)
      } else {
        modparm[[iparm]] <- modtrn
      }
    }
    moduse[[iuse]] <- modparm
  }

  # make direct water use model by use and parameter
  mod <- moduse
  write_rds(mod, "Product/mod.rds")
  return(mod)
}
