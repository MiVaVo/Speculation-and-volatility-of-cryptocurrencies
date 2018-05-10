library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(lmtest)
library(rugarch)
source('funcs.R')

# 1. Prepare overall data
df=read.csv('datasets_created_python/merged_all.csv')
df$date=as.POSIXct(as.Date(df$date))

######################### 1. Visualize returns

redtrans = rgb(255, 0, 0, 225, maxColorValue=255) 
greentrans=rgb(0, 255, 0, 225, maxColorValue=255) 
bluetrans=rgb(0, 0, 255, 225, maxColorValue=255) 

par(mar = rep(2, 4))
dt
par(mfrow=c(1,1))
polygon(density(((log_usdrub-mean(log_usdrub))/sd(log_usdrub))), col="grey", border="black")
plot(density(((df$R_BTC-mean(df$R_BTC))/sd(df$R_BTC))), type = "l", main = " ", ylab = "Плотность распределения",
     xlab = "Доходность актива", lty = 1, col = redtrans, lwd = 2, xlim = c(-5,5), ylim = c(0,1.5))
# polygon(density(((df$R_BTC-mean(df$R_BTC))/sd(df$R_BTC))), lty = 3, lwd = 2,col=redtrans)

lines(density(((df$R_ETH-mean(df$R_ETH))/sd(df$R_ETH))), lty = 1, lwd = 2,col=greentrans) 
# polygon(density(((df$R_ETH-mean(df$R_ETH))/sd(df$R_ETH))), lty = 3, lwd = 2,col=greentrans) 

lines(density(((df$R_XRP-mean(df$R_XRP))/sd(df$R_XRP))), lty = 1, lwd = 2,col=bluetrans) 


# polygon(density(((df$R_XRP-mean(df$R_XRP))/sd(df$R_XRP))), lty = 3, lwd = 2,col=bluetrans) 
legend("topleft", cex=0.8,text.font=0.9, legend =c("Доходность USD/BTC", "Доходность USD/ETH", "Доходность USD/XRP"),
                                                  col = c( "red", "blue", "green"),lty = 1:3,lwd = 2, bty = "n")
grid(12)
############################# 2.Visualize models
x=models_all[[1]]
plot(x,whic='all')
vmodel  = x@model$modeldesc$vmodel
T = x@model$modeldata$T
insample = 1:T
xdates  = x@model$modeldata$index[insample]
xseries = x@model$modeldata$data[insample]
xsigma  = x@fit$sigma
ci = 2
plot(xdates, xseries, type = "l",  col = "steelblue", ylab = "Returns", xlab="Time",
     main = "Series with 2 Conditional SD Superimposed", cex.main = 0.8)
lines(xdates, +ci* xsigma , col = "tomato1")
lines(xdates, -ci* xsigma, col = "tomato1")
mtext(paste("GARCH model : ", vmodel), side = 4, adj = 0, padj=0, col = "gray", cex = 0.5)
if(vmodel == "fGARCH"){
  mtext(paste("fGARCH submodel: ", x@model$modeldesc$vsubmodel, sep = ""), side = 4, adj = 0, padj=1.5, col = "gray", cex = 0.5)
}
abline(h = 0, col = "grey", lty = 3)
grid()
