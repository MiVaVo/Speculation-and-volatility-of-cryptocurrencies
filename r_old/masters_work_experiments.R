library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(lmtest)
library(rugarch)
source('funcs.R')

# 1. Prepare overall data
df=read.csv('merged_all.csv')
df$date=as.POSIXct(as.Date(df$date))
df=df[seq(51,dim(df)[1],1),]
summary(df)
crypto_abr=c('BTC','ETH','XRP')
fits_of_garch=list()
cor(df[,-1])
# 2. Loop over all currencies and calculate volatility, that was associated with speculative processes
for (cryptos in crypto_abr){
  
  print(cryptos)
  steping=dim(df)[1]-1
  
  for (i in seq(1,dim(df)[1]-steping,steping)){
    dates=df_new[,grepl('date', colnames(df_new))]
    df_new=df[seq(i,i+steping,1),]
    # 2.1 Prepare dep.variable y, that will be used in ARMAX-GARCH model
    y_here=df_new[,grepl(paste('R_',cryptos,sep=''), colnames(df_new)) | grepl('date', colnames(df_new)) ]
    y_here <- xts(y_here[,-1], order.by=as.POSIXct(y_here$date))
    # 2.2 Prepare exogenious variable, that will be used in ARMAX part of ARMAX-GARCH model
    ext_regressor_here=df_new[,grepl(paste('RV_',cryptos,sep=''), colnames(df_new))]
    # tdf=as.matrix(cbind(ext_regressor_here_1,
    #               ext_regressor_here_2,ext_regressor_here_3))
    # 2.3 Describe ARMAX(1,1)-GARCH(1,1) model
    g1=ugarchspec(variance.model = list(model = "apARCH", #external.regressors = as.matrix(ext_regressor_here),
                                        garchOrder = c(1,1)),
                  mean.model  = list(armaOrder = c(1,0), external.regressors = as.matrix(ext_regressor_here),
                                     include.mean = TRUE),
                  # mean.model  = list(external.regressors = as.matrix(df_new[,c(2)])), 
                  distribution.model = "std")
    # 2.4 Fit model with appropriate solvers
    g1fit=ugarchfit(g1,data=y_here,solver='hybrid')
    windows()
    plot(g1fit,which='all')
    # 2.5 Prepare dataset for GARCH regression
    # df_garch_lm=ext_regressor_here
    # df_garch_lm$garch11=g1fit@fit$sigma
    df_to_reg=cbind(g1fit@fit$sigma,ext_regressor_here_1)
    colnames(df_to_reg)=c(paste('sigma_',cryptos,sep=''),paste('RV_',cryptos,sep=''))
    df_to_reg=as.data.frame(df_to_reg)
    
    # 2.6 Fit regression model GARCH(1,1)~b0+b1*Speculation , where Speculation is the measure of speculation
    # as described in 'Blau M. Price dynamics and speculative trading in bitcoinBenjamin,2017'
    # and is based on 'Guillermo L. Dynamic Volume-Return Relation of Individual Stocks,2000'
    
    m1<-lm(df_to_reg[,1]~df_to_reg[,2],data = df_to_reg)
    print(summary(m1))
    
    # 2.7 Save volatility of a given cryptocyrrency, that is associated (caused by) with speculation
    fits_of_garch=append(fits_of_garch,list(m1$fitted.values))
    
  }
}
# 3 . Conduct Granger casuality test to test the H0, which is as follows:
# Volatility, associated  with speculative processes on cryptocurrency X cause ( based on granger test)
# speculative volatility on cryptocurrency Y, where X and Y are currencies from c('BTC','ETH','XRP')
# 3.1. BTC -> ETH
tsdisplay(unlist(fits_of_garch[2]))
grangertest(unlist(fits_of_garch[2]) ~ unlist(fits_of_garch[1]), order = 3) #0.194 H0 rejected
# 3.2. ETH -> BTC
grangertest(unlist(fits_of_garch[1]) ~ unlist(fits_of_garch[2]), order = 3) #0.001692 ** H0 not rejected

# 3.3. BTC -> XRP
grangertest(unlist(fits_of_garch[3]) ~ unlist(fits_of_garch[1]), order = 3) #0.8227 H0 rejected
# 3.4. XRP -> BTC
grangertest(unlist(fits_of_garch[1]) ~ unlist(fits_of_garch[3]), order = 3) #0.8551 H0 rejected

# 3.3. ETH -> XRP
grangertest(unlist(fits_of_garch[3]) ~ unlist(fits_of_garch[2]), order = 3) #0.03617 * H0 not rejected
# 3.4. XRP -> ETH
grangertest(unlist(fits_of_garch[2]) ~ unlist(fits_of_garch[3]), order = 3) # 0.6793 H0 rejected

