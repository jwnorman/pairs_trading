Pairs Trading
=============

Implementation and simulation of Pairs Trading, a stock market trading strategy.

I implemented the Pairs Trading algorithm in R. Pairs Trading is a stock market technique developed by Morgan Stanley in the 1980's. The idea is to collect past data for two stocks, acting as training data, that are highly correlated with each other. Since they are highly correlated, the ratio of the prices of the stocks remains around a constant value. Using this property, I buy and sell either stock depending on its position from the constant ratio value. For example, if the ratio of Stock A to Stock B is relatively high compared to the average ratio, then, since the stocks are highly correlated, we expect Stock B to begin to do better, relative to Stock A. This position at which I buy or sell a stock - the distance from the mean ratio - can be determined by optimizing the total profit yielded through simulation.
