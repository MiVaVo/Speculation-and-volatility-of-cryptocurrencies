library(forecast)
library(quantmod)
library(timeSeries)
library(tseries)
library(xts)
library(lmtest)
library(ggplot2)

library(rugarch)
source('funcs.R')
library(psych)

library(reshape2)
#install.packages('tibble')
# install.packages("devtools")
library(devtools)
# install_github("easyGgplot2", "kassambara")
# install.packages("PerformanceAnalytics")

load(paste('saved_models/','GARCH_model.rds',sep=''))
# plot(models_all[['BTC']][[1]],which='all')
crypto_abr=c('BTC','ETH','XRP')
# 1. Prepare overall data
df=read.csv('datasets_created_python/merged_all.csv')
df$date=as.POSIXct(as.Date(df$date))
df=df[seq(51,dim(df)[1],1),]
#########################  2. plot distributions of returns
library(easyGgplot2)
sas=melt(df, id.vars=c("date"))
df_R=df[sapply( colnames(df) ,function(i)(grepl('R_' , i) || grepl('date' , i) ))]
# for (i in seq(2,3,4)){
#   df_R[,i]=(df_R[,i]-mean(df_R[,i]))/sd(df_R[,i])
#   
# }
plot_obj=ggplot2.density(data=melt(df_R, id.vars=c("date")), xName='value', groupName='variable',
                # legendPosition="top",
                alpha=0.5, fillGroupDensity=TRUE)+
  # stat_function(fun = custom)+
                xlab("Значение доходности") +theme(plot.title = element_text(hjust = 0.75))+ 
                  ylab("Плотность")

plot_obj=plot_obj+xlim(-0.25,0.25)
plot_obj=plot_obj+ guides(fill = guide_legend(title = "Криптовалюты", title.position = "left"))+
  scale_fill_discrete(labels=c("Bitcoin", "Ethereum", "Ripple"))+ 
  # ggtitle("Плотности распределения доходности криптовалют")+
  theme(plot.title = element_text(hjust = 0.5))
plot_obj
#########################  3. Plot R,RV, sigma
list_of_plots=list()
for (crypto in crypto_abr){
  # crypto=crypto_abr[1]
  list_of_dfs=list()
  df_in_loop=df
  current_model=models_all[[crypto]][[1]]
  length(current_model@fit$sigma)
  # df_in_loop=melt(df, id.vars=c("date"))
  df_in_loop=df_in_loop[sapply( colnames(df_in_loop) ,function(i)(all(grepl(crypto , i) ,(grepl('R_' , i)) || grepl('RV_' , i)) || grepl('date' , i)))]
  # df_in_loop=melt(df_in_loop, id.vars=c("date"))
  df_in_loop$sigma=rep(current_model@fit$sigma,1)
  df_in_loop=df_in_loop[ , order(names(df_in_loop))]
  names(df_in_loop)<-c('Дата','Доходность','Спекуляция','Волатильность')
  p<-ggplot(data = df_in_loop, aes(x = Дата))+
    geom_line(aes( y = Доходность, colour = "Доходность"), size = 0.8, alpha=0.5) +
    geom_line(aes( y = Спекуляция, colour = "Спекуляция"), size = 0.8, alpha=0.5)+
    geom_line(aes( y = Волатильность, colour = "Волатильность"), size = 0.8, alpha=0.5)+
    scale_colour_manual("", 
                        breaks = c("Доходность", "Спекуляция", "Волатильность"),
                        values = c("red", "green", "blue")) +
    xlab(" ") +
    labs(title=paste("Волатильность и спекулятивная торговля",crypto,sep=' ') ) +
    theme(plot.title = element_text(hjust = 0.5))
  # if (crypto!='ETH'){
  #   p=p + theme(legend.position="none")
  # }
  
  list_of_plots[[crypto]]=p
  # scale_color_discrete(name = "Y series", labels = c("RV_BTC"))
  #   
  # p+scale_colour_manual(name="Sprachregionen", values = c("#238b45"),
  #                              labels=c("deutschsprachig"),
  #                              guide = guide_legend(override.aes = list(linetype = c(1))))
  # p+ guides(fill = guide_legend(override.aes = list(color = NA)))
  #   # scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  # 
  # y_bew=rep(current_model@fit$sigma,1)
  # p<-p+geom_line(data = df_in_loop[c('R_BTC','date')],aes(x=date,y=R_BTC),color='pink',
  #                size=3, alpha=0.4,color="TS1")
  # p <- p + geom_line(aes(y = y_bew),size=1, alpha=0.4)
  # # p <- p + scale_y_continuous(sec.axis = sec_axis(~.*, name = "Relative humidity [%]"))
  # # p<-p+scale_colour_manual("", 
  # #                       breaks = c("TempMax", "TempMedia", "TempMin"))++ guides(fill = guide_legend(title = "Криптовалюты", title.position = "left"))+
  # p+scale_fill_discrete(labels=c("Bitcoin", "Ethereum", "Ripple"))+ ggtitle("Плотности распределения доходности криптовалют")+
  #   theme(plot.title = element_text(hjust = 0.5))
  # # p+scale_colour_manual(name= "Levels",
  # #                       values = c("TS1"= "black","TS3"= "red",
  # #                                  "TS2" ="blue"))+
  #   labs(title="Two time series") +
  #   xlab("Time") +
  #   ylab("Levels")+theme(legend.justification = c(1, 0), legend.position = c(1, 0)) 
  # p
}
multiplot(list_of_plots[[1]],list_of_plots[[2]],list_of_plots[[3]] ,cols=1)
#########################  4.Plot cross corelation of volatility
library("PerformanceAnalytics")

load(paste('saved_models/','fits_of_garch_better.rds',sep=''))
df_fitted_sigma=sapply(fits_of_garch_better ,function(i)(unlist(i)))
chart.Correlation(df_fitted_sigma, histogram=TRUE)
#########################  5.Plot quality of fitted AR-GARCH models
load(paste('saved_models/','GARCH_model.rds',sep=''))
windows()
plot(x,which='all')
dev.off()
graphics.off()
windows()
par(mfrow=c(3,3))
for (crypto in crypto_abr){
  plot_9_chart(models_all[[crypto]][[1]],name=crypto)
  plot_8_chart(models_all[[crypto]][[1]],name=crypto)
  plot_10_chart(models_all[[crypto]][[1]],name=crypto)
}
for (crypto in crypto_abr){
  print(models_all[[crypto]][[1]])
  print('--------------------------------------------------------------------------------------------------')
}




#########################  6.make table with p-values 
# TODO: make table with p-values 

# chart.Correlation(df_R[,-1], histogram=TRUE, pch=19)
# vmodel  = x@model$modeldesc$vmodel
# T = x@model$modeldata$T
# insample = 1:T
# xdates  = x@model$modeldata$index[insample]
# xseries = x@model$modeldata$data[insample]
# xsigma  = x@fit$sigma
# ci = 2
# plot(xdates, xseries, type = "l",  col = "steelblue", ylab = "Returns", xlab="Time",
#      main = "Series with 2 Conditional SD Superimposed", cex.main = 0.8)
# lines(xdates, +ci* xsigma , col = "tomato1")
# lines(xdates, -ci* xsigma, col = "tomato1")
# mtext(paste("GARCH model : ", vmodel), side = 4, adj = 0, padj=0, col = "gray", cex = 0.5)
# if(vmodel == "fGARCH"){
#   mtext(paste("fGARCH submodel: ", x@model$modeldesc$vsubmodel, sep = ""), side = 4, adj = 0, padj=1.5, col = "gray", cex = 0.5)
# }
# abline(h = 0, col = "grey", lty = 3)
# grid()
