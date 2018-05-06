library(forecast)
library(quantmod)
library(dplyr)
remove.packages('Rcpp')
# install.packages('dplyr')
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
df_new=df_new[df_new[,'name']=='Bitcoin' | df_new[,'name']=='Ethereum',]
logturnover=log(df_new['volume']+0.00000255)

group_by(df_new, name) %>%
  summarise(mean=mean(age), sd=sd(age))