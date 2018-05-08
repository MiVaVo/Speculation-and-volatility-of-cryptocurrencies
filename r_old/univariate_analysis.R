library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(rugarch)
df=read.csv('all-crypto-currencies/crypto-markets.csv')
unique(df[,'name'])
df_new=df[c('name','date','open','high','low','close','volume','market')]
df_new=df_new[df_new[,'name'] %in% 'Bitcoin',]
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
model = auto.arima(df_new$R,xreg = df_new$mixed_variable,
                   max.p = 7,
                   max.q = 7,stepwise = FALSE)
arima(df_new$R,order=c(5,0,5),xreg=df_new$mixed_variable,include.mean = TRUE)
tsdisplay(df_new$V)
# model$coef
# model = auto.arima(R,xreg = data.frame(mixed_variable))
seq_of_res=c()
for (p in c(1)){
  for (q in c(1)){
    for (P in c(0,1,2,3,4,5)){
      for (Q in c(0,1,2,3,4,5)){
        for (include.mean in c(TRUE,FALSE)){
          for (arfima in c(TRUE,FALSE)){
            if ( p==q && p==0){
              next
            }
            g1=ugarchspec(variance.model = list(model = "sGARCH",
                                                garchOrder = c(p, q),
                                                submodel = NULL,
                                                external.regressors = NULL,
                                                variance.targeting = FALSE),
                          mean.model  = list(armaOrder = c(P,Q),
                                             arfima =arfima,include.mean = include.mean,
                                             external.regressors = matrix(df_new$mixed_variable)),
                          distribution.model = "std")
            g1fit=ugarchfit(g1,data=qxts$R)
            print(c(infocriteria(g1fit)[2],P,Q,include.mean,arfima))
            seq_of_res<-c(seq_of_res,c(infocriteria(g1fit)[2],P,Q,include.mean,arfima))
          }
        }
      }
    }
  }
}

g1=ugarchspec(variance.model = list(model = "sGARCH", 
                                    garchOrder = c(1,1), 
                                    submodel = NULL, 
                                    external.regressors = NULL,
                                    variance.targeting = FALSE),
              mean.model  = list(armaOrder = c(1,1),
                                 arfima =TRUE,include.mean = FALSE,
                                 external.regressors = matrix(df_new$mixed_variable)), 
              distribution.model = "std")
g1fit=ugarchfit(g1,data=qxts$R)
summary(g1fit)
windows()
plot(g1fit,which='all')

windows()
tsdisplay(qxts$R)

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
