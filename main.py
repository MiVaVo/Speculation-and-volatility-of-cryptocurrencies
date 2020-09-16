from src.load_data.poloniex_data import load_data_from_poloniex
from src.load_data.quandl_data import load_data_from_quandl

if __name__ == "__main__":
    load_data_from_poloniex()
    load_data_from_quandl()
