library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(rugarch)
source('funcs.R')
df=read.csv('df_prepared_for_multiple_analysis.csv')

df$date=as.POSIXct(as.Date(df$date))
summary(df)
df=df[seq(51,dim(df)[1],1),]
as.integer(dim(df_new)[1]/9)
# steping=300
steping=dim(df)[1]-1
int_values=c()
int_std=c()
for (i in seq(1,dim(df)[1]-steping,steping)){
  # i=801
  df_new=df[seq(i,i+steping,1),]
# df_new=as.matrix(df_new)
  qxts <- xts(df_new[,-1], order.by=as.POSIXct(df_new$date))
  
  # colnames(qxts)
  # tsdisplay(df_new$RV_Ripple)
  # model = auto.arima(df_new$R_Bitcoin,xreg = as.matrix(df_new[,c(2,3,4)]),
  #                    max.p = 7,
  #                    max.q = 7,stepwise = FALSE)
  # arima(df_new$R,order=c(5,0,5),xreg=df_new$mixed_variable,include.mean = TRUE)
  
  g1=ugarchspec(variance.model = list(model = "sGARCH", 
                                      garchOrder = c(1,1), 
                                      submodel = NULL, 
                                      external.regressors = NULL,
                                      variance.targeting = FALSE),
                mean.model  = list(armaOrder = c(4,2),
                                   arfima =FALSE,include.mean = TRUE,
                                   external.regressors = as.matrix(df_new[,c(2,3,4)])), 
                distribution.model = "std")
  g1fit=ugarchfit(g1,data=qxts$R_Bitcoin)
  
  int_values<-c(int_values,coef(g1fit)[4])
  int_std<-c(int_std,      g1fit@fit$se.coef[4])
  
  # cor(df_new[,2],df_new[,3])
  # se.coef(g1fit)
  # g1fit
}


plot(int_values-2*int_std, col='green', type='l', lwd=2, ylim=c(-0.3,0.1))
points(int_values+2*int_std, col='green', type='l', lwd=2)
points(int_values, col='blue', type='l', lwd=2)
points(rep.int(0, length(int_values)), col='black', type='l', lwd=2)
plot(int_std)


df_garch_lm=df[,-1]
df_garch_lm$garch11=g1fit@fit$sigma
df_garch_lm$RV_Bitcoin=df$RV_Bitcoin
df_garch_lm$RV_Ethereum=df$RV_Ethereum
df_garch_lm$RV_Ripple=df$RV_Ripple
df_garch_lm$V_Bitcoin=df$V_Bitcoin
df_garch_lm$V_Ethereum=df$V_Ethereum
df_garch_lm$V_Ripple=df$V_Ripple

# df_new$RV_Ripple
m1<-lm(garch11~RV_Bitcoin+RV_Ethereum+RV_Ripple+V_Bitcoin+V_Ethereum+V_Ripple,data = df_garch_lm)
summary(m1)
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
