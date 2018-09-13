# Code for my master's degree paper "Modeling of speculative processes in the cryptocurrency market"
This repo include code and some data wrappers for my master's degree program. Most analysis was done in R but Python was also used for data gatharing and processing.

## Approach

In this paper I study the effect of speculative processes of three cryptocurrency markets :USD/BTC, USD/XRP,USD/ETH on volatility on this markets. Key findings are as follows: 1) The price of theese three exchange rates are predominantly formulated bybased on speculative trade, 2) The speculative processes on USD/BTC market cause volatility (instability) on other two markets , but reverse casuality was not revieled 3) USD/XRP is the mostly speculative pair among these three pairs. Ethereuam is the less speculative  ,perhaps due to the fact,that it has much higher fundamental value.

Main approach: 
1) Estimate volatility of each pair and choose hyperparameters for models based on grid search.
2) Clear it from noise 
3) Define the speculative regressors for each pair as the product of trade volume multiplied by difference in prices
3) Treat fitted values as the measure of speculative process
4) Fit regression and classification model to predict volatility based on regressors, that represent speculative trade( speculative regressors)

## Estimation results
### Best volatily models for each crypto based on AIC 
![1](https://user-images.githubusercontent.com/21066491/45515164-0cb33b80-b7b0-11e8-8497-297cce29f0ca.png)


