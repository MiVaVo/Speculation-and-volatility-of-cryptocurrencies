library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(lmtest)
library(rugarch)
source('functional.R')
# install.packages('xtable')
# install.packages('Hmisc')
library(xtable)
library(Hmisc)
# 1. Prepare overall data
df = read.csv('../../datasets_created_python/merged_all.csv')
df$date = as.POSIXct(as.Date(df$date))
df = df[seq(51, dim(df)[1], 1),]
summary(df)
crypto_abr <- c('BTC', 'ETH', 'XRP')
models_lists = list(list(model = "sGARCH", garchOrder = c(1, 1)),
                    list(model = "csGARCH", garchOrder = c(1, 1)),
                    list(model = "fGARCH", garchOrder = c(1, 1), submodel = "NAGARCH"))
fits_of_garch = list()
cor(df[, -1])
models_all = list()
# 2. Loop over all currencies and calculate volatility, that was associated with speculative processes
tsdisplay(y_here)

mdels_ic = list()
for (cryptos in crypto_abr) {
  # cryptos='BTC'
  print(cryptos)
  steping = dim(df)[1] - 1
  mdels_ic_crypto = list()
  counts_crypto = 1
  for (current_model in models_lists) {
    # current_model=models_lists[1]


    for (i in seq(1, dim(df)[1] - steping, steping)) {

      dates = df_new[, grepl('date', colnames(df_new))]
      df_new = df[seq(i, i + steping, 1),]
      # 2.1 Prepare dep.variable y, that will be used in ARMAX-GARCH model
      y_here = df_new[, grepl(paste('R_', cryptos, sep = ''), colnames(df_new)) | grepl('date', colnames(df_new))]
      y_here <- xts(y_here[, -1], order.by = as.POSIXct(y_here$date))
      # 2.2 Prepare exogenious variable, that will be used in ARMAX part of ARMAX-GARCH model
      ext_regressor_here = df_new[, grepl(paste('RV_', cryptos, sep = ''), colnames(df_new))]

      # 2.3 Describe ARMAX(1,1)-GARCH(1,1) model
      g1 = ugarchspec(variance.model = current_model,
                      mean.model = list(armaOrder = c(1, 0), external.regressors = as.matrix(ext_regressor_here),
                                        include.mean = TRUE),
                      # mean.model  = list(external.regressors = as.matrix(df_new[,c(2)])),
                      distribution.model = "std")
      # 2.4 Fit model with appropriate solvers
      g1fit = ugarchfit(g1, data = y_here, solver = 'hybrid')
      mdels_ic_crypto[[current_model$model]] <- list(infocriteria(g1fit))
      counts_crypto = counts_crypto + 1
      models_all <- append(models_all, list(g1fit))

      # 2.5 Prepare dataset for GARCH regression

      df_to_reg = cbind(g1fit@fit$sigma, ext_regressor_here)
      colnames(df_to_reg) = c(paste('sigma_', cryptos, sep = ''), paste('RV_', cryptos, sep = ''))
      df_to_reg = as.data.frame(df_to_reg)

      # 2.6 Fit regression model GARCH(1,1)~b0+b1*Speculation , where Speculation is the measure of speculation
      # as described in 'Blau M. Price dynamics and speculative trading in bitcoinBenjamin,2017'
      # and is based on 'Guillermo L. Dynamic Volume-Return Relation of Individual Stocks,2000'

      m1 <- lm(df_to_reg[, 1] ~ df_to_reg[, 2], data = df_to_reg)
      print(summary(m1))

      # 2.7 Save volatility of a given cryptocyrrency, that is associated (caused by) with speculation
      fits_of_garch = append(fits_of_garch, list(m1$fitted.values))

    }
  }
  mdels_ic[[cryptos]] <- mdels_ic_crypto
}
data.frame(mdels_ic)
for (name in names(mdels_ic)) {
  df_to_report = data.frame(mdels_ic[[name]])
  names(df_to_report) = sapply(c('Simple GARCH', 'Component GARCH', 'Asymmetric GARCH'), function(i)paste(i, name, sep = ', '))
  print(xtable(df_to_report))
}



