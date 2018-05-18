import os
import numpy as np
import pandas as pd
import pickle
from datetime import datetime as dt
import quandl
from python_funcs import *
from datetime import datetime
import matplotlib.pyplot as plt

exchanges = ['KRAKEN','COINBASE', 'BITSTAMP', 'ITBIT'] #MTGOX
exchanges=['BITSTAMP']
# exchanges = [ 'BITSTAMP']
exchange_data = {}
for exchange in exchanges:
    exchange_code = 'BCHARTS/{}USD'.format(exchange)
    btc_exchange_df = get_quandl_data(exchange_code)
    exchange_data[exchange] = btc_exchange_df
btc_exchange_df.T.columns
# btc_exchange_df.to_csv('btc_play.csv')
##### 1.2. Merge data from different exchanges
btc_usd_datasets_price = merge_dfs_on_column(list(exchange_data.values()), list(exchange_data.keys()), 'Weighted Price')
btc_usd_datasets_volume = merge_dfs_on_column(list(exchange_data.values()), list(exchange_data.keys()), 'Volume (BTC)')
btc_usd_datasets= merge_dfs_on_column(list(exchange_data.values()), list(exchange_data.keys()), 'Volume (BTC)')
btc_usd_datasets.plot()
# btc_usd_datasets.tail()

[i.replace(0, np.nan, inplace=True) for i in [btc_usd_datasets_price,btc_usd_datasets_volume]]
# df_scatter(btc_usd_datasets, 'Bitcoin Price (USD) By Exchange')
##### 1.3. Deal with NAs
btc_usd_datasets['price'] = btc_usd_datasets_price.mean(axis=1,skipna=True)
btc_usd_datasets['volume'] = btc_usd_datasets_volume.sum(axis=1,skipna=True)
btc_usd_datasets.plot()
# # # btc_usd_datasets_volume.plot()
##### 1.4. Select needed columns
btc_usd_datasets=btc_usd_datasets[['price','volume']]
# # btc_usd_datasets.iloc[:100,:].plot()
btc_usd_datasets['name']='Bitcoin'
btc_usd_datasets=btc_usd_datasets.reset_index()
btc_usd_datasets.columns=['date', 'price', 'volume', 'name']
##### 1.5. Skip firs 100 bad obs
btc_usd_datasets=btc_usd_datasets.iloc[100:,:]
# # # pd.DataFrame(btc_usd_datasets['volume']).plot()
# plt.show()
# # # btc_usd_datasets['volume'].plot()
############################################ PART 2. Prepare dataset with OUTSTANDING BITCOINS (issued) ##################################

##### 2.1. Prepare bitcoin issued dataset
btc_outst=pd.read_csv('total-bitcoins.csv',header=None)
btc_outst.columns=['date','btc_tot']
btc_outst.date=pd.to_datetime(btc_outst.date)
# pd.timedelta_range(btc_outst.iloc[0,0],btc_outst.iloc[-1,0])
# int(btc_outst.iloc[-1,0]-btc_outst.iloc[0,0])
datelist = pd.date_range(btc_outst.iloc[0,0], periods=(btc_outst.iloc[-1,0]-btc_outst.iloc[0,0]).days+1).tolist()
dateframe=pd.DataFrame(datelist)
dateframe.columns=['date']
### NO DEVISION BY 2 BECAUSE IT IS SUPPLY !!!
bts_outst_int=(pd.merge(dateframe,btc_outst,how='outer',on='date').set_index('date')).interpolate().reset_index()

##### 2.2. Merge outstanding Bitcoins dataset with Bitcoin price-volume dataset
btc_usd_datasets_new=pd.merge(btc_usd_datasets,bts_outst_int,how='left',on='date')
btc_usd_datasets_new.head()

##### 2.3. Add turnover rate to total dataset
btc_usd_datasets_new['turnover']=btc_usd_datasets_new['volume']
btc_usd_datasets_new[np.any(btc_usd_datasets_new.isnull(),axis=1)]
btc_usd_datasets_new=btc_usd_datasets_new.dropna() ############################### !!!!!!!!!!!!
# btc_usd_datasets_new['turnover'].plot()

##### 2.4. Prepare dataset as in paper ( 'RV_Bitcoin', 'V_Bitcoin', 'R_Bitcoin', 'R5_Bitcoin') , where R - is return, V- is trading activity,
##### RV - is multiple of R and V

btc_quandl=prepare_df_v2(btc_usd_datasets_new,
                         price_name='price',
                         crypto_name='name',
                         date_name='date',
                         turnover_name='turnover',trend_lasts=120)

data_for_spec=pd.merge(btc_quandl.reset_index(),btc_usd_datasets.reset_index(),on='date')
df_price_and_spec=data_for_spec[['date','RV_Bitcoin','price','R_Bitcoin']]
df_price_and_spec=df_price_and_spec.iloc[2000:,:]


df_price_and_spec.columns=['Дата','Спекулятивная переменная для Bitcoin','Курс Bitcoin','Дневная доходность Bitcoin']
df_price_and_spec = df_price_and_spec.set_index('Дата')
fig, ax1 = plt.subplots(1,1)
ax1.set_ylabel('Процентное изменение')
df_price_and_spec[['Дневная доходность Bitcoin']].plot(ax=ax1, color='red', label='Дневная доходность Bitcoin')
df_price_and_spec[['Спекулятивная переменная для Bitcoin']].plot(ax=ax1, color='blue', label='Спекулятивная переменная для Bitcoin')

ax2 = ax1.twinx()
df_price_and_spec[['Курс Bitcoin']].plot(ax=ax2, color='green', label='Курс Bitcoin')
np.correlate(df_price_and_spec[['Курс Bitcoin']],df_price_and_spec[['Дневная доходность Bitcoin']])
ax2.set_ylabel('USD')
# ax2.set
ax1.legend(loc=2)
ax2.legend(loc=1)
plt.show()