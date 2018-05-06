# TimeSeries

The idea of the strategy is relatively simple but if you want to experiment with it I highly suggest reading the previous posts on time series analysis in order to understand what you would be modifying!

The strategy is carried out on a "rolling" basis:

For each day, n, the previous k days of the differenced logarithmic returns of a stock market index are used as a window for fitting an optimal ARIMA and GARCH model.
The combined model is used to make a prediction for the next day returns.
If the prediction is negative the stock is shorted at the previous close, while if it is positive it is longed.
If the prediction is the same direction as the previous day then nothing is changed.
For this strategy I have used the maximum available data from Yahoo Finance for the S&P500. I have taken k=500 but this is a parameter that can be optimised in order to improve performance or reduce drawdown.
