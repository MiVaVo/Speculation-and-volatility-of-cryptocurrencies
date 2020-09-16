import os
import pickle

import numpy as np
import pandas as pd
import quandl

from .configs.data_load_configs import Configs as configs


def create_folder(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)


def prepare_df(df):
    df = df[['date', 'name', 'volume', 'close']]
    df_last = pd.pivot_table(df, values=['volume', 'close'], index=['date'], columns=['name'])
    df_last.index = pd.to_datetime(df_last.index)
    sorted(list(np.unique(df_last[np.any(df_last.isnull(), axis=1)].index)))
    df_last = df_last.dropna()
    list(df_last)

    # 2. Prepare Rt: returns (shor will be Rt) for each cryprocyrrency
    list(df_last)
    Rt = df_last[[i for i in list(df_last) if i[0] == 'close']]
    Rt = Rt.pct_change()
    Rt[pd.isnull(Rt)] = 0
    Rt.columns = ['R_' + i[1] for i in list(Rt)]

    # 3. Prepare vt : cryptocyrrency 50-day de-trended measure of trading activity as described in
    # "Dynamic volume-Return relation of individual stocks. Rev. Financial Stud. 15, 1005–1047."
    Vt_temp = df_last[[i for i in list(df_last) if i[0] == 'volume']]
    Vt_temp = np.log(Vt_temp + 0.00000255)
    trend_lasts = 50
    Vt = []
    for i in range(0, Vt_temp.shape[0]):
        if i < trend_lasts:
            Vt.append(np.array([0] * Vt_temp.shape[1]))
            continue
        Vt.append(Vt_temp.iloc[i, :].as_matrix() - np.mean(Vt_temp.iloc[i - trend_lasts:i - 1, :]).as_matrix())
    Vt = pd.DataFrame(np.asarray(Vt)).set_index(Rt.index)
    Vt.columns = list(Vt_temp)
    Vt.columns = ['V_' + i[1] for i in list(Vt)]

    # 4. Prepare Rt x vt
    RVt = pd.DataFrame(np.array(Vt) * np.array(Rt)).set_index(Rt.index)
    RVt.columns = ['RV_' + i[1] for i in list(Vt_temp)]

    # 5. Save tables
    res_table = pd.concat([RVt, Vt, Rt], axis=1)
    return res_table


def prepare_df_v2(df, turnover_name='volume', price_name='close', crypto_name='name', date_name='date',
                  include_5_change=True, how_to_deal_with_na='drop', trend_lasts=2):
    # df=btc_usd_datasets_new.copy()
    # price_name = 'price'
    # crypto_name = 'name'
    # date_name = 'date'
    # turnover_name = 'turnover'
    df = df[[date_name, crypto_name, turnover_name, price_name]]
    df_last = pd.pivot_table(df, values=[turnover_name, price_name], index=[date_name], columns=[crypto_name])
    df_last.index = pd.to_datetime(df_last.index)

    if np.any(df_last.isnull(), axis=1).sum() != 0:
        if how_to_deal_with_na == 'drop':
            df_last = df_last.dropna()
        else:
            raise ValueError('DATAFRAME CONTAINS NANs')

    # 2. Prepare Rt: returns (shor will be Rt) for each cryprocyrrency
    list(df_last)
    Rt = df_last[[i for i in list(df_last) if i[0] == price_name]]
    Rt = Rt.pct_change()
    Rt[np.array(pd.isnull(Rt))[:, 0]] = 0
    Rt.columns = ['R_' + i[1] for i in list(Rt)]

    # 3. Prepare vt : cryptocyrrency 50-day de-trended measure of trading activity as described in
    # "Dynamic volume-Return relation of individual stocks. Rev. Financial Stud. 15, 1005–1047."
    Vt_temp = df_last[[i for i in list(df_last) if i[0] == turnover_name]]
    Vt_temp = np.log(Vt_temp + 0.00000255)
    # Vt_temp.plot()
    # plt.show()

    Vt = []
    for i in range(0, Vt_temp.shape[0]):
        if i < trend_lasts:
            Vt.append(np.array([0] * Vt_temp.shape[1]))
            continue
        Vt.append(Vt_temp.iloc[i, :].as_matrix() - np.mean(Vt_temp.iloc[i - trend_lasts:i - 1, :]).as_matrix())
    Vt = pd.DataFrame(np.asarray(Vt)).set_index(Rt.index)
    Vt.columns = list(Vt_temp)
    Vt.columns = ['V_' + i[1] for i in list(Vt)]

    # 4. Prepare Rt x vt
    RVt = pd.DataFrame(np.array(Vt) * np.array(Rt)).set_index(Rt.index)
    RVt.columns = ['RV_' + i[1] for i in list(Vt_temp)]

    # 5. Save tables
    if include_5_change:
        Rt5 = df_last[[i for i in list(df_last) if i[0] == price_name]]
        Rt5 = Rt5.pct_change(periods=5)
        Rt5[np.array(pd.isnull(Rt5))[:, 0]] = 0
        Rt5.columns = ['R5_' + i[1] for i in list(Rt5)]
        res_table = pd.concat([RVt, Vt, Rt, Rt5], axis=1)
    else:
        res_table = pd.concat([RVt, Vt, Rt], axis=1)
    return res_table


def merge_dfs_on_column(dataframes, labels, col):
    '''Merge a single column of each dataframe into a new combined dataframe'''
    series_dict = {}
    for index in range(len(dataframes)):
        series_dict[labels[index]] = dataframes[index][col]

    return pd.DataFrame(series_dict)


def get_json_data(json_url, cache_path):
    '''Download and cache JSON data, return as a dataframe.'''
    try:
        f = open(cache_path, 'rb')
        df = pickle.load(f)
        print('Loaded {} from cache'.format(json_url))
    except (OSError, IOError) as e:
        print('Downloading {}'.format(json_url))
        df = pd.read_json(json_url)
        df.to_pickle(cache_path)
        print('Cached {} at {}'.format(json_url, cache_path))
    return df


def get_crypto_data(poloniex_pair):
    '''Retrieve cryptocurrency data from poloniex'''
    json_url = configs.base_polo_url.format(poloniex_pair, configs.start_date.timestamp(), configs.end_date.timestamp(),
                                            configs.pediod)
    data_df = get_json_data(json_url, 'crypto_from_poloniex/' + poloniex_pair)
    data_df = data_df.set_index('date')
    return data_df


def get_quandl_data(quandl_id):
    '''Download and cache Quandl dataseries'''
    cache_path = '{}{}.pkl'.format(configs.data_dir, quandl_id.replace('/', '-'))
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


def find(pattern, path):
    result = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if pattern in name:
                # if fnmatch.fnmatch(name, pattern):
                result.append(os.path.join(root, name))
                print(os.path.join(root, name))

    return result
