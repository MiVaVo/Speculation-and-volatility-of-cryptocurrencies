library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(lmtest)
install_and_load_packages(c('caret', 'pROC', 'RColorBrewer', 'ggplot2', "devtools",
                            'quantmod', 'timeSeries', 'xts', 'lmtest', "xtable",
                            "rugarch", "forecast", 'lmtest', 'sandwich',
                            "tidyr", "tbl2xts", "lubridate", "readr", 'texreg',
                            "PerformanceAnalytics",
                            "ggplot2", "dplyr", 'rmsfuns', 'plotROC',
                            "ggthemes",
                            'stargazer'))
library(rugarch)
source('src/modeling/functional.R')

# 1. Prepare overall data
df = read.csv('datasets_created_python/merged_all.csv')
df$date = as.POSIXct(as.Date(df$date))
df = df[seq(51, dim(df)[1], 1),]
summary(df)
crypto_abr = c('BTC', 'ETH', 'XRP')
fits_of_garch = list()
fits_of_garch_better = list()
cor(df[, -1])
models_all = list()
lm_models_list = list()
lm_models_list_ripple = list()
log_reg_models_list = list()
# 2. Loop over all currencies and calculate volatility, that was associated with speculative processes
# tsdisplay(y_here)
for (cryptos in crypto_abr) {
  # cryptos='BTC'
  print(cryptos)
  steping = dim(df)[1] - 1

  for (i in seq(1, dim(df)[1] - steping, steping)) {
    if (cryptos == 'XRP') {
      garch_mdel = list(model = "csGARCH", # external.regressors = as.matrix(ext_regressor_here),
                        garchOrder = c(1, 1))
    }
    else {
      garch_mdel = list(model = "sGARCH", #external.regressors = as.matrix(ext_regressor_here),
                        garchOrder = c(1, 1))
    }
    df_new = df[seq(i, i + steping, 1),]
    dates = df_new[, grepl('date', colnames(df_new))]
    # 2.1 Prepare dep.variable y, that will be used in ARMAX-GARCH model
    y_here = df_new[, grepl(paste('R_', cryptos, sep = ''), colnames(df_new)) | grepl('date', colnames(df_new))]
    y_here <- xts(y_here[, -1], order.by = as.POSIXct(y_here$date))
    # 2.2 Prepare exogenious variable, that will be used in ARMAX part of ARMAX-GARCH model
    ext_regressor_here = df_new[, grepl(paste('RV_', cryptos, sep = ''), colnames(df_new))]
    # ext_regressor_here=abs(ext_regressor_here)
    # 2.3 Describe ARMAX(1,1)-GARCH(1,1) model
    g1 = ugarchspec(variance.model = garch_mdel,
                    mean.model = list(armaOrder = c(1, 0), external.regressors = as.matrix(ext_regressor_here),
                                      include.mean = TRUE),
                    # mean.model  = list(external.regressors = as.matrix(df_new[,c(2)])),
                    distribution.model = "std")
    # 2.4 Fit model with appropriate solvers
    g1fit = ugarchfit(g1, data = y_here, solver = 'hybrid')

    models_all[[cryptos]] <- list(g1fit)
    # 2.5 Prepare dataset for GARCH regression

    df_to_reg = cbind(g1fit@fit$sigma, ext_regressor_here)
    colnames(df_to_reg) = c(paste('sigma_', cryptos, sep = ''), paste('RV_', cryptos, sep = ''))
    df_to_reg = as.data.frame(df_to_reg)

    # 2.6 Fit regression model GARCH(1,1)~b0+b1*Speculation , where Speculation is the measure of speculation
    # as described in 'Blau M. Price dynamics and speculative trading in bitcoinBenjamin,2017'
    # and is based on 'Guillermo L. Dynamic Volume-Return Relation of Individual Stocks,2000'
    df_regression = df_new[, grepl('RV_', colnames(df_new))]
    # df_regression=df_regression*df_regression
    df_regression = abs(df_regression)
    ######################## SIMPLE REGRESSION ##############
    regression_df = data.frame(df_to_reg[, 1],
                               c(0, df_regression[-dim(df_regression)[1], 1]),
                               c(0, df_regression[-dim(df_regression)[1], 2]),
                               c(0, df_regression[-dim(df_regression)[1], 3]))


    # dep_variable>quantile(dep_variable,probs=c(.05,.95))[2]
    colnames(regression_df) <- c(names(df_to_reg)[1], 'Speculations_BTC',
                                 'Speculations_ETH',
                                 'Speculations_XRP')
    lm_model <- lm(regression_df[, 1] ~ Speculations_BTC +
      Speculations_ETH +
      Speculations_XRP, data = regression_df)
    lm_models_list <- append(lm_models_list, list(lm_model))
    if (cryptos == 'XRP') {

      lm_model_1 <- lm(regression_df[, 1] ~ Speculations_BTC, data = regression_df)
      lm_model_2 <- lm(regression_df[, 1] ~ Speculations_ETH, data = regression_df)
      lm_model_3 <- lm(regression_df[, 1] ~ Speculations_XRP, data = regression_df)
      lm_models_list_ripple <- append(lm_models_list_ripple, list(lm_model))
      lm_models_list_ripple <- append(lm_models_list_ripple, list(lm_model_1))
      lm_models_list_ripple <- append(lm_models_list_ripple, list(lm_model_2))
      lm_models_list_ripple <- append(lm_models_list_ripple, list(lm_model_3))
    }
    # lm_models_list_ripple<-append(lm_models_list_ripple,list(lm_model))
    ######################## LOGISTIC REGRESSION ##############
    dep_variable = df_to_reg[, 1]
    quantile_95 = quantile(dep_variable, probs = c(.10, .90))[2]
    dep_variable_logreg = sapply(dep_variable, function(i) if (i > quantile_95) 1  else 0)
    log_regression_df = data.frame(dep_variable_logreg,
                                   c(0, df_regression[-dim(df_regression)[1], 1]),
                                   c(0, df_regression[-dim(df_regression)[1], 2]),
                                   c(0, df_regression[-dim(df_regression)[1], 3]))
    colnames(log_regression_df) <- c(names(log_regression_df)[1], 'Speculations_BTC',
                                     'Speculations_ETH',
                                     'Speculations_XRP')
    volatil_logreg <- glm(log_regression_df[, 1] ~ Speculations_BTC +
      Speculations_ETH +
      Speculations_XRP, data = log_regression_df,
                          family = "binomial")
    # fc_log_reg<-predict(volatil_logreg, log_regression_df[,-1], type="response")
    log_reg_models_list[[cryptos]] <- volatil_logreg


    #


    # ),data = df_to_reg)
    # cor(c(0,df_regression[-dim(df_to_reg)[1],1]),c(0,df_regression[-dim(df_to_reg)[1],2]))
    # windows()
    # plot(df_to_reg[,1])
    print(summary(lm_model))

    # 2.7 Save volatility of a given cryptocyrrency, that is associated (caused by) with speculation
    fits_of_garch = append(fits_of_garch, list(m1$fitted.values))
    # fits_of_garch=append(fits_of_garch,list(g1fit@fit$sigma))
    fits_of_garch_better[[cryptos]] <- list(m1$fitted.values)
  }
}
make_latex_table_with_robust_coeffs(lm_models_list, model_names_in_seq = c('BTC Volatility Reg.',
                                                                           'ETH Volatility Reg.',
                                                                           'XRP Volatility Reg.'))
make_latex_table_with_robust_coeffs(lm_models_list_ripple, model_names_in_seq = c('XRP Model 1',
                                                                                  'XRP Model 2',
                                                                                  'XRP Model 3',
                                                                                  'XRP Model 4'))
make_latex_table_with_robust_coeffs(log_reg_models_list, model_names_in_seq = c('BTC extreme volatility ',
                                                                                'ETH extreme volatility',
                                                                                'XRP extreme volatility'))
# log_reg_models_list[[1]]$fitted.values
# log_reg_models_list[[1]].predict()

roc_curves_list = list()
for (i in crypto_abr) {
  myRoc <- roc(response = log_reg_models_list[[i]]$y, predictor = log_reg_models_list[[i]]$fitted.values,
               positive = 'versicolor')
  roc_curves_list[[i]] <- list(TPR = myRoc$sensitivities,
                               FPR = 1 - myRoc$specificities,
                               Cryptocurrency = as.factor(rep(i, length(myRoc$sensitivities))))
}

results = do.call(rbind.data.frame, roc_curves_list)
ggplot(results, aes(FPR, TPR, color = Cryptocurrency)) +
  geom_line(size = 2, alpha = 0.7) +
  labs(
  # title= "ROC curve",
  x = "False Positive Rate (1-Specificity)",
  y = "True Positive Rate (Sensitivity)") +
  style_roc()

lists_for_roc_curves[]
sen = myRoc$sensitivities
one_minus_spec = 1 - myRoc$specificities

# save(g1fit, file = paste('saved_models/',paste(cryptos,'GARCH_model.rds',sep='_'),sep=''))
save(models_all, file = paste('saved_models/', 'GARCH_model.rds', sep = ''))
save(fits_of_garch_better, file = paste('saved_models/', 'fits_of_garch_better.rds', sep = ''))

# 3 . Conduct Granger casuality test to test the H0, which is as follows:
# Volatility, associated  with speculative processes on cryptocurrency X cause ( based on granger test)
# speculative volatility on cryptocurrency Y, where X and Y are currencies from c('BTC','ETH','XRP')
# 3.1. BTC -> ETH
grangertest(unlist(fits_of_garch[2]) ~ unlist(fits_of_garch[1]), order = 3) #0.194 H0 rejected #0.16
# 3.2. ETH -> BTC
grangertest(unlist(fits_of_garch[1]) ~ unlist(fits_of_garch[2]), order = 3) #0.001692 ** H0 not rejected  0.001936 **
grangertest(unlist(fits_of_garch[1]) ~ unlist(fits_of_garch[2]), order = 1) #0.001692 ** H0 not rejected  0.001936 **

# 3.3. BTC -> XRP
grangertest(unlist(fits_of_garch[3]) ~ unlist(fits_of_garch[1]), order = 2) #0.8227 H0 rejected
# 3.4. XRP -> BTC
grangertest(unlist(fits_of_garch[1]) ~ unlist(fits_of_garch[3]), order = 3) #0.8551 H0 rejected

# 3.3. ETH -> XRP
grangertest(unlist(fits_of_garch[3]) ~ unlist(fits_of_garch[2]), order = 2) #0.03617 * H0 not rejected
# 3.4. XRP -> ETH
grangertest(unlist(fits_of_garch[2]) ~ unlist(fits_of_garch[3]), order = 2) # 0.6793 H0 rejected

