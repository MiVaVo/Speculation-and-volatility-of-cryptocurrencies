# Code for my master's degree paper "Modeling of speculative processes in the cryptocurrency market"
This repo include code and some data wrappers for my master's degree program. Most analysis was done in R but Python was also used for data gatharing and processing.

It also contains scripts for loading cryptocurrency data from Poloniex (Poloniex_data.py)  and quandl (quandl_data.py) , that might be helpful for you.
## Usage
1. You first have to download and save data from poloniex stock exchange and then from quandl using python
```python
python main.py
```
2. Then conduct main analysis using 
```r
Rscript main.R
```
## Approach

In this paper I study the effect of speculative processes of three cryptocurrency markets :USD/BTC, USD/XRP,USD/ETH on volatility on this markets. Key findings are as follows: 1) The price of theese three exchange rates are predominantly formulated bybased on speculative trade, 2) The speculative processes on USD/BTC market cause volatility (instability) on other two markets , but reverse casuality was not revieled 3) USD/XRP is the mostly speculative pair among these three pairs. Ethereuam is the less speculative  ,perhaps due to the fact,that it has much higher fundamental value.

Main approach: 
1) Estimate volatility of each pair and choose hyperparameters for models based on grid search.
2) Clear it from noise 
3) Define the speculative regressors for each pair as the product of trade volume multiplied by difference in prices
3) Treat fitted values as the measure of speculative process
4) Fit regression and classification model to predict volatility and top 10% of volatility based on regressors, that represent speculative trade( speculative regressors)

Some concepts steps are derived from paper "Blau B. Price dynamics and speculative trading in Bitcoin // Research in International Business and Finance, Vol. 43, January 2018. pp. 15-21.",but instead I use multivariate analysis for estimation of speculative processes. 

## Estimation results
### Best volatily models for each crypto based on AIC 
![1](https://user-images.githubusercontent.com/21066491/45515164-0cb33b80-b7b0-11e8-8497-297cce29f0ca.png)

Fitted (cleaned from  noise)  volatilies, that are further used for inference.
![6](https://user-images.githubusercontent.com/21066491/45515882-20f83800-b7b2-11e8-9ebe-f8d7b7101bbe.png)

### Estimated coefficients of regression of speculative trade on volatility of each pair
![3](https://user-images.githubusercontent.com/21066491/45515180-16d53a00-b7b0-11e8-931d-2c1420d0b40e.png)

As it can be seen coefficients behind XRP are all significant, which indicates, that it's volatility have very high correlation with speculative processes on other two pairs, but reverse is not true.

### Estimated coefficients of logistic regression of speculative trade on volatility  of each pair.
Prediction of the probability of volatility bieng in max 10%.
![4](https://user-images.githubusercontent.com/21066491/45515184-19d02a80-b7b0-11e8-9513-a04ef85d96b6.png)

ROC-AUC plot, volatility on XRP is highly influence by speculations on other two markets.
![5](https://user-images.githubusercontent.com/21066491/45515195-1f2d7500-b7b0-11e8-9096-65830b8c1fed.png)


## TODO:
- Refactoring
- Test if project is still working