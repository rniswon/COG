#  ------------------------------------------------------------------------#
# model data----
#  ------------------------------------------------------------------------#



#  ------------------------------------------------------------------------#
# train models of water use using linear or quantile regression----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".model_*" functions:
#   data = water-use input data as data frame;
#   data2 = optional additional water-use input data (or NULL) as data frame;
#   parm = parameters to model as character vector;
#   catg = category of analysis as character vector;
#   conlev = level for confidence interval as numeric vector
.train_model = function(data=data,
                        data2=data2,
                        parm=parm,
                        catg=catg,
                        conlev=conlev) {

  data$Year %<>% as.factor()
  data$County %<>% as.factor()
  if (!is.null(data2)) {
    data2$Year %<>% as.factor()
    data2$County %<>% as.factor()
  }
  if (parm == "Mean") {

    # fit linear regression of volume against number of wells
    mod = list()
    mod$All = lm(Volume ~ Count, data=data)

    # get parameter estimates and confidence intervals
    modparm = list()
    modparm$All = tidy(mod$All, conf.int=T, conf.level=conlev) %>%
      filter(!term == "(Intercept)")

    # get model summary
    modsumm = list()
    modsumm$All = glance(mod$All)

    # do model for each year
    for (iyr in levels(data$Year)) {
      temp = data %>%
        filter(Year %in% iyr)

      # fit linear regression of volume against number of wells
      mod[[iyr]] = lm(Volume ~ Count, data=temp)

      # get parameter estimates and confidence intervals
      modparm[[iyr]] = tidy(mod[[iyr]], conf.int=T, conf.level=conlev) %>%
        filter(!term == "(Intercept)")

      # get model summary
      modsumm[[iyr]] = glance(mod[[iyr]])
    }
    modparm %<>%
      bind_rows(.id="Year") %>%
      mutate(conlev = conlev)
    modsumm %<>% bind_rows(.id="Year")

    # get predicted values, residuals and confidence intervals by year and county
    if (catg == "Ancillary") {
      temp = data2
    } else {
      temp = data
    }
    if (length(unique(temp$County)) == 1 | length(unique(temp$Year)) == 1) {
      mod0 = try(glm(Volume ~ Count, family=Gamma, data=temp), silent=T)
      if ("try-error" %in% class(mod0)) {
        mod0 = glm(Volume ~ Count, family=Gamma, data=temp, start=c(0, 1))
      }
    } else {
      mod0 = try(glm(Volume ~ Count + Year + County, family=Gamma, data=temp), silent=T)
      if ("try-error" %in% class(mod0)) {
        mod0 = glm(Volume ~ Count + Year + County, family=Gamma, data=temp, start=c(0, 1, rep(1, times=length(unique(temp$County)) - 1 + length(unique(temp$Year)) - 1)))
      }
    }
    if (catg == "Ancillary") {
      modpred = augment(mod0, type.predict="response", se_fit=T) %>%
        mutate(.conf.low = .fitted + qnorm((1 - conlev) / 2) * .se.fit,
               .conf.high = .fitted - qnorm((1 - conlev) / 2) * .se.fit,
               .conf.low = ifelse(.conf.low < 0, 0, .conf.low),
               .conf.high = ifelse(.conf.high < 0, 0, .conf.high)) %>%
        add_column(State = temp$State, .before=".fitted")
    } else {
      shp = summary(mod0)$dispersion
      modpred = augment(mod0, se_fit=T) %>%
        mutate(.conf.low = family(mod0)$linkinv(.fitted + qgamma((1 - conlev) / 2, ifelse(shp < 1, shp ^ -1, shp)) * .se.fit),
               .conf.high = family(mod0)$linkinv(.fitted - qgamma((1 - conlev) / 2, ifelse(shp < 1, shp ^ -1, shp)) * .se.fit),
               .fitted = family(mod0)$linkinv(.fitted),
               .conf.low = ifelse(.conf.low < 0, 0, .conf.low),
               .conf.high = ifelse(.conf.high < 0, 0, .conf.high)) %>%
        add_column(State = temp$State, .before=".fitted")
    }
    if (length(unique(temp$County)) == 1 | length(unique(temp$Year)) == 1) {
      modpred %<>% add_column(Year = temp$Year,
                              County = temp$County, .before="State")
    }
  } else {

    # fit quantile regression of volume against number of wells
    qile = str_extract(parm, "[[:digit:]]+") %>%
      as.numeric() %>%
      divide_by(100)
    mod = list()
    mod$All = rq(Volume ~ Count, data=data, tau=qile)

    # get parameter estimates and confidence intervals
    modparm = list()
    set.seed(.seed)
    modparm$All = tidy(mod$All, se.type="boot", conf.level=conlev, R=100) %>%
      filter(!term == "(Intercept)")

    # get model summary
    modsumm = list()
    modsumm$All = glance(mod$All) %>%
      type.convert()

    # do model for each year
    for (iyr in levels(data$Year)) {
      temp = data %>%
        filter(Year == iyr)

      # fit quantile regression of volume against number of wells
      qile = str_extract(parm, "[[:digit:]]+") %>%
        as.numeric() %>%
        divide_by(100)
      mod[[iyr]] = rq(Volume ~ Count, data=temp, tau=qile)

      # get parameter estimates and confidence intervals
      modparm[[iyr]] = tidy(mod[[iyr]], se.type="rank", conf.level=conlev) %>%
        filter(!term == "(Intercept)")

      # get model summary
      modsumm[[iyr]] = glance(mod[[iyr]]) %>%
        type.convert()
    }
    modparm %<>%
      bind_rows(.id="Year") %>%
      mutate(conlev = conlev)
    modsumm %<>% bind_rows(.id="Year")

    # get predicted values and residuals by year and county
    if (catg == "Ancillary") {
      temp = data2
    } else {
      temp = data
    }
    if (length(unique(temp$County)) == 1 | length(unique(temp$Year)) == 1) {
      mod0 = rq(Volume ~ Count, data=temp, tau=qile)
    } else {
      mod0 = rq(Volume ~ Count + Year + County, data=temp, tau=qile)
    }
    modpred = augment(mod0, se_fit=T) %>%
      mutate(.fitted = ifelse(.fitted < 0, 0, .fitted)) %>%
      add_column(State = temp$State, .before=".resid")
    if (length(unique(temp$County)) == 1 | length(unique(temp$Year)) == 1) {
      modpred %<>% add_column(Year = temp$Year,
                              County = temp$County, .before="State")
    }
  }

  # make fitted model, predicted values, model summary and parameter estimates
  dat = list()
  dat$Parameters = modparm
  dat$Summary = modsumm
  dat$Predictions = modpred
  dat$Model = mod
  return(dat)
}


#  ------------------------------------------------------------------------#
# test models for predictions of water use with all possible combinations of observations using leave-one-out cross-validation----
#  ------------------------------------------------------------------------#


# make function with following arguments from ".model_*" functions:
#   data = water-use input data as data frame;
#   parm = parameters to model as character vector;
#   conlev = level for confidence interval as numeric vector
.test_model = function(data=data,
                       parm=parm,
                       conlev=conlev) {

  # for each cross-validated value
  datcros = data.frame("Observed" = NA, "Predicted" = NA)
  data$Year %<>% as.factor()
  data$County %<>% as.factor()
  for (i in seq_len(nrow(data))) {

    # train model by fitting all but one left-out observation except for singular combinations
    if (nrow(filter(data, County == data$County[i])) == 1 | nrow(filter(data, Year == data$Year[i])) == 1) {
      traindat = data
    } else {
      traindat = data[-i, ]
    }
    if (parm == "Mean") {

      # fit linear regression of volume against number of wells by year and county
      if (length(unique(traindat$County)) == 1 | length(unique(traindat$Year)) == 1) {
        trainmod = try(glm(Volume ~ Count, family=Gamma, data=traindat), silent=T)
        if ("try-error" %in% class(trainmod)) {
          trainmod = glm(Volume ~ Count, family=Gamma, data=traindat, start=c(0, 1))
        }
      } else {
        trainmod = try(glm(Volume ~ Count + Year + County, family=Gamma, data=traindat), silent=T)
        if ("try-error" %in% class(trainmod)) {
          trainmod = glm(Volume ~ Count + Year + County, family=Gamma, data=traindat, start=c(0, 1, rep(1, times=length(unique(traindat$County)) - 1 + length(unique(traindat$Year)) - 1)))
        }
      }
    } else {

      # fit quantile regression of volume against number of wells by year and county
      qile = str_extract(parm, "[[:digit:]]+") %>%
        as.numeric() %>%
        divide_by(100)
      if (length(unique(traindat$County)) == 1 | length(unique(traindat$Year)) == 1) {
        trainmod = rq(Volume ~ Count, data=traindat, tau=qile)
      } else {
        trainmod = rq(Volume ~ Count + Year + County, data=traindat, tau=qile)
      }
    }

    # test model by predicting one left-out observation
    testdat = data[i, ]
    testmod = predict(trainmod, newdata=testdat, type="response") %>%
      ifelse(. < 0, 0, .)
    datcros[i, "Observed"] = testdat$Volume
    datcros[i, "Predicted"] = testmod
    datcros[i, "Count"] = testdat$Count
    datcros[i, "Year"] = testdat$Year
    datcros[i, "County"] = testdat$County
    datcros[i, "State"] = testdat$State
  }

  # get goodness-of-fit metrics for predicted values against observed values except for extreme outliers
  datcros$Predicted[which(abs(datcros$Observed - datcros$Predicted) > mean(datcros$Predicted) + 1.5 * IQR(datcros$Predicted))] = NA
  rsq = cor(datcros$Observed, datcros$Predicted, use="na.or.complete") ^ 2  # R squared
  rmse = sqrt(mean((datcros$Predicted - datcros$Observed) ^ 2, na.rm=T)) / mean(datcros$Count)  # root mean square error
  rsr = rmse / sd(datcros$Observed) * mean(datcros$Count)  # ratio of root mean square error to standard deviation of observations

  # make cross-validated values and goodness-of-fit metrics
  dat = list()
  dat$GoodnessofFit = data.frame(Rsq = rsq, RMSE = rmse, RSR = rsr)
  dat$CrossValidation = as_tibble(datcros)
  return(dat)
}



#  ------------------------------------------------------------------------#
# B.1. model direct water-use input data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = direct water-use input data as data frame;
#   parm = parameters ("Mean", "P5", "P50", "P95", etc.) to model as character vector;
#   cat = category ("Direct") of analysis as character vector;
#   conlev = level for confidence interval as numeric vector
.model_direct = function(data=datdir,
                         parm=c("Mean", "P5", "P50", "P95"),
                         catg="Direct",
                         conlev=.conlev) {

  if (any(sapply(list(data, parm, catg, conlev), is.null))) stop("one or more necessary arguments missing")

  # for each use
  moduse = list()
  for (iuse in unique(data$Use)) {

    # get data for use
    dat = data %>%
      filter(Use == iuse)

    # for each parameter
    modparm = list()
    for (iparm in parm) {

      # train model using linear or quantile regression, and test model using leave-one-out cross-validation
      modtrn = .train_model(data=dat, data2=NULL, parm=iparm, catg=catg, conlev=conlev)
      modtst = .test_model(data=dat, parm=iparm, conlev=conlev)
      modparm[[iparm]] = append(modtrn, modtst)
    }
    moduse[[iuse]] = modparm
  }

  # make direct water use model by use and parameter
  moddir = moduse
  write_rds(moddir, "Product/moddir.rds")
  return(moddir)
}


#  ------------------------------------------------------------------------#
# B.2. model indirect water-use input data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = indirect water-use input data as data frame;
#   parm = parameters ("Mean", "P5", "P50", "P95", etc.) to model as character vector;
#   catg = category ("Indirect") of analysis as character vector;
#   conlev = level for confidence interval as numeric vector
.model_indirect = function(data=datind,
                           parm=c("Mean", "P5", "P50", "P95"),
                           catg="Indirect",
                           conlev=.conlev) {

  if (any(sapply(list(data, parm, catg, conlev), is.null))) stop("one or more necessary arguments missing")

  # for each use
  moduse = list()
  for (iuse in unique(data$Use)) {

    # get data for use
    dat = data %>%
      filter(Use == iuse)

    # for each parameter
    modparm = list()
    for (iparm in parm) {

      # train model using linear or quantile regression, and test model using leave-one-out cross-validation
      modtrn = .train_model(data=dat, data2=NULL, parm=iparm, catg=catg, conlev=conlev)
      modtst = .test_model(data=dat, parm=iparm, conlev=conlev)
      modparm[[iparm]] = append(modtrn, modtst)
    }
    moduse[[iuse]] = modparm
  }

  # make indirect water use model by use and parameter
  modind = moduse
  write_rds(modind, "Product/modind.rds")
  return(modind)
}


#  ------------------------------------------------------------------------#
# B.3. model ancillary water-use input data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = ancillary water-use input data as data frame;
#   parm = parameters ("Mean", "P5", "P50", "P95", etc.) to model as character vector;
#   catg = category ("Ancillary") of analysis as character vector;
#   conlev = level for confidence interval as numeric vector
.model_ancillary = function(data=datanc,
                            parm=c("Mean", "P5", "P50", "P95"),
                            catg="Ancillary",
                            conlev=.conlev) {

  if (any(sapply(list(data, parm, catg, conlev), is.null))) stop("one or more necessary arguments missing")

  # for each use
  moduse = list()
  for (iuse in unique(data$Use)) {

    # get data for use distributed to wells or withdrawn from sources
    dat = data %>%
      filter(Use == iuse,
             Case == "DistributedtoWells")
    dat2 = data %>%
      filter(Use == iuse,
             Case == "WithdrawnfromSources")

    # for each parameter
    modparm = list()
    for (iparm in parm) {

      # train model using linear or quantile regression, and test model using leave-one-out cross-validation
      modtrn = .train_model(data=dat, data2=dat2, parm=iparm, catg=catg, conlev=conlev)
      modtst = .test_model(data=dat, parm=iparm, conlev=conlev)
      modparm[[iparm]] = append(modtrn, modtst)
    }
    moduse[[iuse]] = modparm
  }

  # make ancillary water use model by use and parameter
  modanc = moduse
  write_rds(modanc, "Product/modanc.rds")
  return(modanc)
}


#  ------------------------------------------------------------------------#
# B.4. model population data----
#  ------------------------------------------------------------------------#


# make function with following arguments:
#   data = population data as data frame;
#   conlev = level for confidence interval as numeric vector
.model_population = function(data=datpop,
                             conlev=.conlev) {

  if (any(sapply(list(data, conlev), is.null))) stop("one or more necessary arguments missing")

  # summarize population by year
  data %<>%
    group_by(Year) %>%
    select(-State) %>%
    summarize(across(everything(), sum), .groups="drop")

  # fit linear regression of population against lagged number of wells
  form = lapply(paste0("Wells", 0:5), function(x) paste("Persons ~", x))
  mod = lapply(form, function(x) lm(as.formula(x), data=data))
  idx = lapply(mod, function(x) summary(x)$r.squared) %>%
    which.max()
  mod %<>% .[[idx]]

  # get parameter estimates and confidence intervals
  modparm = tidy(mod, conf.int=T, conf.level=conlev) %>%
    filter(!term == "(Intercept)") %>%
    mutate(conlev = conlev)

  # get model summary
  modsumm = glance(mod)

  # get predicted values and confidence intervals
  mod0 = lapply(form, function(x) glm(as.formula(x), data=data))
  mod0 %<>% .[[idx]]
  modpred = augment(mod0, se_fit=T) %>%
    mutate(.conf.low = family(mod0)$linkinv(.fitted + qnorm((1 - conlev) / 2) * .se.fit),
           .conf.high = family(mod0)$linkinv(.fitted - qnorm((1 - conlev) / 2) * .se.fit))

  # combine fitted data with year variable from original data
  modpred = data %>%
    select(Year, Persons) %>%
    full_join(modpred, by="Persons")

  # make fitted model, predicted values, model summary and parameter estimates
  dat = list()
  dat$Parameters = modparm
  dat$Summary = modsumm
  dat$Predictions = modpred
  dat$Model = mod0

  # make population model
  modpop = dat
  write_rds(modpop, "Product/modpop.rds")
  return(modpop)
}
