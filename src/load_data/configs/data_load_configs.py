from datetime import datetime

import quandl


class Configs:
    quandl.ApiConfig.api_key = '****'
    base_polo_url = 'https://poloniex.com/public?command=returnChartData&currencyPair={}&start={}&end={}&period={}'
    start_date = datetime.strptime('2015-01-01', '%Y-%m-%d')  # get data from the start of 2015
    end_date = datetime.now()  # up until today
    pediod = 86400  # pull daily data (86,400 seconds per day)
    data_dir = 'crypto_from_quandl/'
    datasets_created_python = 'configs.datasets_created_python/'
