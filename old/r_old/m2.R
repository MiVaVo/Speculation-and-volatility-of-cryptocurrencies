library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(lmtest)
library(rugarch)
source('../../src/modeling/functional.R')
df = read.csv('merged_all.csv')

df$date = as.POSIXct(as.Date(df$date))
df = df[seq(51, dim(df)[1], 1),]
summary(df)
crypto_abr = c('BTC', 'ETH', 'XRP')
fits_of_garch = list()
for (cryptos in crypto_abr) {
  print(cryptos)


  # df_here=df[grepl('BTC', colnames(df))]
  steping = dim(df)[1] - 1
  # int_values=c()
  # int_std=c()
  # f=list()

  for (i in seq(1, dim(df)[1] - steping, steping)) {
    dates = df_new[, grepl('date', colnames(df_new))]
    df_new = df[seq(i, i + steping, 1),]
    # df_new[grepl(cryptos, colnames(df_new))]
    qxts <- xts(df_new[, !grepl('date', colnames(df_new))], order.by = as.POSIXct(dates))
    y_here = df_new[, grepl(paste('R_', cryptos, sep = ''), colnames(df_new))]
    ext_regressor_here = df_new[, grepl(paste('RV_', cryptos, sep = ''), colnames(df_new))]
    # print(arima(df_new$R_Bitcoin,order=c(1,0,0),xreg=df_new$RV_Bitcoin,include.mean = TRUE))
    g1 = ugarchspec(variance.model = list(model = "sGARCH",
                                          garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 1), external.regressors = as.matrix(ext_regressor_here),
                                      include.mean = TRUE),
                    # mean.model  = list(external.regressors = as.matrix(df_new[,c(2)])),
                    distribution.model = "norm")
    g1fit = ugarchfit(g1, data = y_here, solver = 'hybrid')
    # int_values<-c(int_values,coef(g1fit)[3])
    # int_std<-c(int_std,      g1fit@fit$se.coef[3])

    df_garch_lm = ext_regressor_here
    df_garch_lm$garch11 = g1fit@fit$sigma
еее=g1fit@fit$sigma
df_to_reg=cbind(g1fit@fit$sigma, ext_regressor_here)
colnames(df_to_reg)=c(paste('sigma_', cryptos, sep=''), paste('RV_', cryptos, sep=''))
df_to_reg=as.data.frame(df_to_reg)

m1<-lm(df_to_reg[, 1]~df_to_reg[, 2], data = df_to_reg)
print(summary(m1))
m1$fitted.values
fits_of_garch=append(fits_of_garch, list(m1$fitted.values))
# summary(m1)
# print(summary(m1))
}
}
# grangertest(fits_of_garch[1],
grangertest(unlist(fits_of_garch[1]) ~ unlist(fits_of_garch[2]), order = 3) #0.01952 *

plot(int_values-2*int_std, col='green', type='l', lwd=2, ylim=c(0, 1))
points(int_values+2*int_std, col='green', type='l', lwd=2)
points(int_values, col='blue', type='l', lwd=2)
# ARIMA_GARCH_optimizer(p_range=c(1), q_range=c(1),
#                       P_range=c(0,1,2,3),
#                       Q_range=c(0,1,2,3),
#                       ext_regress=as.matrix(df_new[,c(2,3,4)]),
#                       y=qxts$R_Bitcoin)





cor(df_garch_lm$RV_Bitcoin, df_garch_lm$V_Bitcoin)
# fitted(g1fit)
# windows()
plot(g1fit, which=2)
# cor(qxts$R_Ethereum,qxts$R_Bitcoin)
summary(g1fit)

plot(g1fit, which='all')

windows()
plot(g1fit, which='all')

tsdisplay(qxts$R_Bitcoin)

windows()
tsdisplay(qxts$mixed_variable)

# print(c(infocriteria(g1fit)[2],p,q))
# 
# tsdisplay(qxts$R)
# plot(g1fit,which=3)
# plot(ni$zx, ni$zy, ylab=ni$yexpr, xlab=ni$xexpr, type="l", main = "News Impact Curve")
# plot(g1fit)
# # df=data.frame(mixed_variable,R)
# # colnames(df)=c('R','mv')
# 
# c(infocriteria(g1fit))[1]
# length(qxts$R)
# ########################### REGRESS SIGMA ON OTHER FACTORS ############
# g1fit@fit$sigma
# 
# 
# fit <- Arima(R, order = c(1,0,0), xreg = mixed_variable)
# # if (fit['coef'][[1]][[3]]<0){
# #   print(crypto)
# # }
# summary(model)
# par(mar=c(0.1,0.1,0.1,0.1))
# resids=unlist(model['residuals'])
# 
# tsdisplay(resids)
# dev.off()
# min(model['residuals'])
# length(unlist(model['residuals']))
# par(mar = rep(1, 4))
# 
# par(mar = rep(1, 4))
# graphics.off()
