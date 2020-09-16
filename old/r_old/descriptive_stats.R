Sys.setenv(PATH = "%PATH%;C:/Rtools/gcc-4.6.3/bin;c:/Rtools/bin")
library(forecast)
library(quantmod)
library(dplyr)
library(psych)
library(timeSeries)
library(tseries)
library(xts)
library(rugarch)
df = read.csv('../../all-crypto-currencies/crypto-markets.csv')
unique(df[, 'name'])
df_new = df[c('name', 'date', 'open', 'high', 'low', 'close', 'volume', 'market')]
grouped = group_by(df_new, name) %>%
  summarise(sum = sum(market), sd = sd(market))
grouped = grouped[order(-grouped[, 2]),]
percentages = grouped['sum'] / sum(grouped[, 2])
sum(percentages[1:3,]) #### FIRST 3 currencies account for 72% of total market cap 
named_of_cryptos = lapply(c(grouped[1:5, 1]), as.character)
# df_to_work_with=df_new[df_new[,'name'] %in% named_of_cryptos$name,] 
df_to_work_with_2 = df_new[df_new[, 'name'] %in% c("Bitcoin",
                                                   "Ethereum",
                                                   "Ripple"),]

write.csv(df_to_work_with_2, file = "df_to_prepare_for_multiple_analysis.csv")
     