library(forecast)
library(quantmod)
# install.packages('forecast')
# install.packages('rugarch')
# install.packages('timeSeries')
# install.packages('tseries')
library(timeSeries)
library(tseries)
library(xts)
library(rugarch)
df=read.csv('all-crypto-currencies/crypto-markets.csv')
unique(df[,'name'])
df_new=df[c('name','date','open','high','low','close','volume','market')]
df_new=df_new[df_new[,'name']=='Bitcoin',]
logturnover=log(df_new['volume']+0.00000255)
counts=0
trend_lasts=50
V=c()
for (i in seq(1,dim(logturnover)[1],1)){
  if (i<trend_lasts){
    V<-c(V,0)
    next
  }
  V<-c(V,logturnover[i,'volume']-mean(logturnover[seq(i-trend_lasts,i-1,1),'volume']))
}
counts=trend_lasts
logturnover[seq(counts-trend_lasts,counts-1,1),'volume']
# par(mar=c(1,1,1,1))
# tsdisplay(V[seq(0,length(V),1)])

###################### CREATE RETURN SERIES ############
series_to_analyze=unlist(df_new['close'])
# periodReturn()
diff(series_to_analyze)
R=diff(series_to_analyze)/series_to_analyze[-length(series_to_analyze)]
df_new$V=V
df_new$R=c(0,R)
df_new=df_new[seq(300,dim(df_new)[[1]]-300,1),c('R','V','date')]
df_new$date=as.Date(df_new$date,format='%Y-%m-%d')
df_new$mixed_variable=df_new$R*df_new$V
# V=V[seq(300,length(V),1)]
# tsdisplay(df_new$mixed_variable)
# mixed_variable=R*V
# length(V)
# length(mixed_variable)
df_new$date

df_new$date=as.POSIXct(df_new$date)
# df_new=as.matrix(df_new)
qxts <- xts(df_new[,-3], order.by=as.POSIXct(df_new$date))

windows()
tsdisplay(df_new$R)
model = auto.arima(df_new$R,xreg = df_new$mixed_variable)
arima(df_new$R,order=c(1,0,0),xreg=df_new$mixed_variable,include.mean = TRUE)
# model$coef
# model = auto.arima(R,xreg = data.frame(mixed_variable))
g1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
              mean.model  = list(armaOrder = c(0, 0),
                                 arfima =FALSE,include.mean = TRUE,
                             external.regressors = qxts$mixed_variable,
                             archex = FALSE),distribution.model ='std' )
# df=data.frame(mixed_variable,R)
# colnames(df)=c('R','mv')
g1fit=ugarchfit(g1,data=qxts$R)
########################### REGRESS SIGMA ON OTHER FACTORS ############
g1fit@fit$sigma


fit <- Arima(R, order = c(1,0,0), xreg = mixed_variable)
# if (fit['coef'][[1]][[3]]<0){
#   print(crypto)
# }
summary(model)
par(mar=c(0.1,0.1,0.1,0.1))
resids=unlist(model['residuals'])

tsdisplay(resids)
dev.off()
min(model['residuals'])
length(unlist(model['residuals']))
par(mar = rep(1, 4))

par(mar = rep(1, 4))
graphics.off()
