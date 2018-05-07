library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(rugarch)
source('funcs.R')
df=read.csv('df_to_regress.csv')

df$date=as.POSIXct(as.Date(df$date))
df=df[seq(51,dim(df)[1],1),]
summary(df)
# steping=500
steping=dim(df)[1]-1
int_values=c()
int_std=c()
for (i in seq(1,dim(df)[1]-steping,steping)){
  df_new=df[seq(i,i+steping,1),]
  qxts <- xts(df_new[,-1], order.by=as.POSIXct(df_new$date))
  # print(arima(df_new$R_Bitcoin,order=c(1,0,0),xreg=df_new$RV_Bitcoin,include.mean = TRUE))
  g1=ugarchspec(variance.model = list(model = "sGARCH", 
                                      garchOrder = c(1,1)),
                mean.model  = list(armaOrder = c(1,1),
                                   external.regressors = as.matrix(df_new[,c(2)])), 
                distribution.model = "norm")
  g1fit=ugarchfit(g1,data=qxts$R_Bitcoin,solver='hybrid')
  int_values<-c(int_values,coef(g1fit)[3])
  int_std<-c(int_std,      g1fit@fit$se.coef[3])
  
  df_garch_lm=df_new[,-1]
  df_garch_lm$garch11=g1fit@fit$sigma
  
  df_garch_lm$RV_Bitcoin=df_new$RV_Bitcoin
  df_garch_lm$V_Bitcoin=df_new$V_Bitcoin
  df_garch_lm$R5_Bitcoin=df_new$R5_Bitcoin
  df_garch_lm$log_btc_tot=df_new$log_btc_tot
  df_garch_lm$turnover_pct_change=df_new$turnover_pct_change
  # cor(df_garch_lm)
  
  # df_new$RV_Ripple
  m1<-lm(garch11~RV_Bitcoin+R5_Bitcoin+turnover_pct_change+log_btc_tot,data = df_garch_lm)
  summary(m1)
  print(summary(m1))
  
}
plot(int_values-2*int_std, col='green', type='l', lwd=2, ylim=c(0,1))
points(int_values+2*int_std, col='green', type='l', lwd=2)
points(int_values, col='blue', type='l', lwd=2)  
# ARIMA_GARCH_optimizer(p_range=c(1), q_range=c(1),
#                       P_range=c(0,1,2,3),
#                       Q_range=c(0,1,2,3),
#                       ext_regress=as.matrix(df_new[,c(2,3,4)]),
#                       y=qxts$R_Bitcoin)





cor(df_garch_lm$RV_Bitcoin,df_garch_lm$V_Bitcoin)
# fitted(g1fit)
# windows()
plot(g1fit,which=2)
# cor(qxts$R_Ethereum,qxts$R_Bitcoin)
summary(g1fit)

plot(g1fit,which='all')

windows()
plot(g1fit,which='all')

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
