import os
import numpy as np
import pandas as pd
import pickle
from datetime import datetime as dt
import quandl
from python_funcs import *
from datetime import datetime
import matplotlib.pyplot as plt
quandl.ApiConfig.api_key = 'FsG5WQxG6ubGPBiPBzx5'
data_dir = 'crypto_from_quandl/'
def get_quandl_data(quandl_id):
    '''Download and cache Quandl dataseries'''
    cache_path = '{}{}.pkl'.format(data_dir, quandl_id.replace('/', '-'))
    try:
        f = open(cache_path, 'rb')
        df = pickle.load(f)
        print('Loaded {} from cache'.format(quandl_id))
    except (OSError, IOError) as e:
        print('Downloading {} from Quandl'.format(quandl_id))
        df = quandl.get(quandl_id, returns="pandas")
        df.to_pickle(cache_path)
        print('Cached {} at {}'.format(quandl_id, cache_path))
    return df
exchanges = ['KRAKEN','COINBASE', 'BITSTAMP', 'ITBIT'] #MTGOX
exchange_data = {}
for exchange in exchanges:
    exchange_code = 'BCHARTS/{}USD'.format(exchange)
    btc_exchange_df = get_quandl_data(exchange_code)
    exchange_data[exchange] = btc_exchange_df
def merge_dfs_on_column(dataframes, labels, col):
    '''Merge a single column of each dataframe into a new combined dataframe'''
    series_dict = {}
    for index in range(len(dataframes)):
        series_dict[labels[index]] = dataframes[index][col]

    return pd.DataFrame(series_dict)
btc_usd_datasets_price = merge_dfs_on_column(list(exchange_data.values()), list(exchange_data.keys()), 'Weighted Price')
btc_usd_datasets_volume = merge_dfs_on_column(list(exchange_data.values()), list(exchange_data.keys()), 'Volume (BTC)')
btc_usd_datasets= merge_dfs_on_column(list(exchange_data.values()), list(exchange_data.keys()), 'Volume (BTC)')
btc_usd_datasets.plot()
# btc_usd_datasets.tail()

[i.replace(0, np.nan, inplace=True) for i in [btc_usd_datasets_price,btc_usd_datasets_volume]]
# df_scatter(btc_usd_datasets, 'Bitcoin Price (USD) By Exchange')
btc_usd_datasets['price'] = btc_usd_datasets_price.mean(axis=1,skipna=True)
btc_usd_datasets['volume'] = btc_usd_datasets_volume.sum(axis=1,skipna=True)
btc_usd_datasets.plot()
# # # btc_usd_datasets_volume.plot()
btc_usd_datasets=btc_usd_datasets[['price','volume']]
# # btc_usd_datasets.iloc[:100,:].plot()
btc_usd_datasets['name']='Bitcoin'
btc_usd_datasets=btc_usd_datasets.reset_index()
btc_usd_datasets.columns=['date', 'price', 'volume', 'name']
btc_usd_datasets=btc_usd_datasets.iloc[100:,:]
# # # pd.DataFrame(btc_usd_datasets['volume']).plot()
# plt.show()
# # # btc_usd_datasets['volume'].plot()
################################ PREPARE OUTSTANDING BTC ###############
btc_outst=pd.read_csv('total-bitcoins.csv',header=None)
btc_outst.tail()
btc_outst.columns=['date','btc_tot']
btc_outst.date=pd.to_datetime(btc_outst.date)
# pd.timedelta_range(btc_outst.iloc[0,0],btc_outst.iloc[-1,0])
# int(btc_outst.iloc[-1,0]-btc_outst.iloc[0,0])
datelist = pd.date_range(btc_outst.iloc[0,0], periods=(btc_outst.iloc[-1,0]-btc_outst.iloc[0,0]).days+1).tolist()
dateframe=pd.DataFrame(datelist)
dateframe.columns=['date']
### NO DEVISION BY 2 BECAUSE IT IS SUPPLY !!!
bts_outst_int=(pd.merge(dateframe,btc_outst,how='outer',on='date').set_index('date')).interpolate().reset_index()

#################################### CALCULATE WRITE VOLUME ###########################
btc_usd_datasets_new=pd.merge(btc_usd_datasets,bts_outst_int,how='left',on='date')
btc_usd_datasets_new.head()
btc_usd_datasets_new['turnover']=btc_usd_datasets_new['volume']/btc_usd_datasets_new['btc_tot']
btc_usd_datasets_new[np.any(btc_usd_datasets_new.isnull(),axis=1)]
# btc_usd_datasets_new['turnover'].plot()

btc_quandl=prepare_df_v2(btc_usd_datasets_new,
                         price_name='price',
                         crypto_name='name',
                         date_name='date',
                         turnover_name='turnover')

btc_quandl.to_csv('btc_quandl.csv',header=True,index=True)

############## ALL FOR REGRESSION ##########
btc_quandl.head()
btc_usd_datasets_new.head()
df_to_regress=pd.merge(btc_quandl.reset_index(),btc_usd_datasets_new[['btc_tot','turnover','date']],how='left',on='date')
df_to_regress.head()
df_to_regress[['turnover_pct_change']]=df_to_regress[['turnover']].pct_change(periods=5)
df_to_regress[['log_btc_tot']]=np.log(df_to_regress[['btc_tot']])
df_to_regress=df_to_regress.iloc[1:,:]
df_to_regress.to_csv('df_to_regress.csv',header=True,index=False)
###
list(btc_usd_datasets_new)
btc_usd_datasets_new.columns=['date', 'close', '2', 'name', '3', 'volume']
btc_usd_datasets_new=btc_usd_datasets_new[['date', 'close', 'name', 'volume']]
btc_usd_datasets_new.to_csv('btc_usd_datasets_new.csv',header=True,index=False)

