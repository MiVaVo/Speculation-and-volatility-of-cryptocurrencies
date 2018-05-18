import os
import numpy as np
import pandas as pd
import pickle
from datetime import datetime as dt
import quandl
from python_funcs import *
from datetime import datetime
import matplotlib.pyplot as plt

############################################ PART 1. Prepare bitcoin pirce and bitcoin volume dataset ##################################
##### 1.1. Get data from different exchanges
exchanges = ['KRAKEN','COINBASE', 'BITSTAMP', 'ITBIT'] #MTGOX
exchanges=['BITSTAMP']
# exchanges = [ 'BITSTAMP']
exchange_data = {}
for exchange in exchanges:
    exchange_code = 'BCHARTS/{}USD'.format(exchange)
    btc_exchange_df = get_quandl_data(exchange_code)
    exchange_data[exchange] = btc_exchange_df
btc_exchange_df.T.columns
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
                         turnover_name='turnover',trend_lasts=20)
list(btc_quandl)

##### 2.5. Write this ready dataframe to csv
btc_quandl.to_csv(datasets_created_python+'/'+'btc_quandl.csv',header=True,index=True)
############################################ PART 3. Add features to the created dataset to make analysis more flexible #####################

btc_quandl.head()
btc_usd_datasets_new.head()

##### 3.1. Merge create above dataset btc_quandl with columns, that previously were not included in the analysis
df_to_regress=pd.merge(btc_quandl.reset_index(),btc_usd_datasets_new[['btc_tot','turnover','date','price']],how='left',on='date')
df_to_regress.head()

##### 3.2. Calculate turnover as percent change (note, that we diverge here from original paper)
df_to_regress[['turnover_pct_change']]=df_to_regress[['turnover']].pct_change(periods=5)

##### 3.3. Add logs of outstanding bitcoins. Might be helpfull in future
df_to_regress[['log_btc_tot']]=np.log(df_to_regress[['btc_tot']])

##### 3.4. Exclude bad first row
df_to_regress=df_to_regress.iloc[1:,:]

##### 3.5. Flexible dataset for analysis is ready
df_to_regress.to_csv(datasets_created_python+'/'+'df_to_regress.csv',header=True,index=False)
###
# list(btc_usd_datasets_new)
# btc_usd_datasets_new.columns=['date', 'close', '2', 'name', '3', 'volume']
# btc_usd_datasets_new=btc_usd_datasets_new[['date', 'close', 'name', 'volume']]
# btc_usd_datasets_new.to_csv('btc_usd_datasets_new.csv',header=True,index=False)
#
######################################### tests
# btc_quandl1=prepare_df_v2(btc_usd_datasets_new,
#                          price_name='price',
#                          crypto_name='name',
#                          date_name='date',
#                          turnover_name='turnover',trend_lasts=20)
# btc_quandl1[['RV_Bitcoin']].plot()
# list(btc_quandl1)
# btc_quandl2=prepare_df_v2(btc_usd_datasets_new,
#                          price_name='price',
#                          crypto_name='name',
#                          date_name='date',
#                          turnover_name='turnover',trend_lasts=50)
# btc_quandl2[['V_Bitcoin']].plot()
#
# btc_usd_datasets_new[['turnover']].plot()
# btc_quandl2[['R_Bitcoin']].plot()