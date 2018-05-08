import os
import numpy as np
import pandas as pd
from datetime import datetime as dt
import quandl
from python_funcs import *
import matplotlib.pyplot as plt

#### 1. Specify cryprocurrencyes ( excluding bicoin), that will be used in analysis
altcoins = ['ETH','XRP']

#### 2. Load this cryptocurrencies from Polonies exchange
altcoin_data = {}
for altcoin in altcoins:
    coinpair = 'BTC_{}'.format(altcoin)
    crypto_price_df = get_crypto_data(coinpair)
    altcoin_data[altcoin] = crypto_price_df

#### 3. Read previously created Bitcoin dataset to convert prices from (for example) BTC/ETH to USD/ETH
btc_usd_datasets=pd.read_csv(datasets_created_python+'/'+'df_to_regress.csv',index_col=0)
list(btc_usd_datasets)

#### 4. Iteratively conver prices of altcoins to USDs
for altcoin in altcoin_data.keys():
    altcoin_data[altcoin]['price_usd'] =  altcoin_data[altcoin]['weightedAverage'] * btc_usd_datasets['price']

#### 5. Iteratively conver prices of altcoins to USDs
merged=pd.concat(altcoin_data).reset_index()[['price_usd','volume','level_0','date']]
merged.columns=['close','volume','name','date']

#### 6. Drop missing values (there is not much of them, namely 6. To prove you can use : merged.isnull().sum() )
merged.isnull().sum()
merged=merged.dropna()
altcoins=prepare_df_v2(merged).reset_index()

#### 7. Merge Bitcoin dataset, aquired from Quandl, with Poloniex altcoins
bitcoin=pd.read_csv(datasets_created_python+'/'+'btc_quandl.csv',index_col=0).reset_index()
bitcoin.date=pd.to_datetime(bitcoin.date)
bitcoin.columns=['date']+[i.split('_')[0]+'_BTC' for i in list(bitcoin)[1:]]
altcoins.date=pd.to_datetime(altcoins.date)
merged_all=pd.merge(bitcoin,altcoins,on='date')
merged_all=merged_all.dropna()

#### 8. Samve all dataset, that will be used in analysis in R
merged_all.to_csv(datasets_created_python+'/'+'merged_all.csv',index=False,header=True)
