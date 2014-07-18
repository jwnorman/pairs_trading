## stocksim

library(RColorBrewer)
library(lattice)
library(parallel)

########
####
##
#
# stocksim()
#
# num_days: number of days to simulate
# rho: controls how correlated the sequences (stocks) are in time
# psi: controls how correlated the sequences (stocks) are with each other
# sd1, sd2: standard deviation of stock 1,2
# beta1_0, beta2_0: y-intercept of stock 1,2, respectively
# beta1_1, beta2_1: slope of stock 1,2, respectively
#
# function returns a dataframe that contains num_days observations of:
#	stock1, stock2,
# where stock1 represents the closing price of stock1 on day t, and stock2 represents the closing price of stock2 on day t
# 
stocksim <- function(num_days = 4000, rho = .99, psi = 0, sd1 = 1, sd2 = 1, beta1_0 = 100, beta2_0 = 100, beta1_1 = 0, beta2_1 = 0) {
	# Calculate error terms
	error1_t <- rnorm(num_days, 0, sd1)
	error2_t <- rnorm(num_days, 0, sd2)
	
	# store the previous days' values here
	stock1 <- stock2 <- list() 
	
	# generate initial point and immediately prepare it to be the previous point
	stock1[1] <- X1_t <- X1_1 <- rnorm(1, 0, sd1) 
	stock2[1] <- X2_t <- X2_1 <- rnorm(1, 0, sd2)
	
	# Generate data for the rest of the days
	for (i in 2:num_days) { # We've already calculate the first day's price
		
		# note that X1_t and X2_t on the RHS represent the previous day 
		stock1[i] <- X1_t <- rho*X1_t + psi*(1 - rho)*X2_t + error1_t[i]
		stock2[i] <- X2_t <- rho*X2_t + psi*(1 - rho)*X1_t + error2_t[i]
		
	} # end for
	
	stocks <- as.data.frame(cbind(t = 1:num_days, stock1 = unlist(stock1), stock2 = unlist(stock2)))
	Y1_t <- beta1_0 + beta1_1*stocks$t + stocks$stock1
	Y2_t <- beta2_0 + beta2_1*stocks$t + stocks$stock2
	return(as.data.frame(cbind(Close.Stock1 = Y1_t, Close.Stock2 = Y2_t)))
}
#
# end stocksim()
#
##
####
########

########
####
##
#
# simstudy()
#
# B: the number of datasets we'd like to sim
# corr: controls how correlated the sequences (stocks) are with each other
# slope1: slope of stock1
# slope2: slope of stock2
# ... : any parameters to pass on to find.best.k()
#
# function returns the profit for the given parameters (using k which maximised profit in training data)
#
simstudy <- function(B.corr.slope1.slope2, ...) {
	browser()
	B = B.corr.slope1.slope2[1]
	corr = B.corr.slope1.slope2[2]
	slope1 = B.corr.slope1.slope2[3]
	slope2 = B.corr.slope1.slope2[4]
	
	starttime <- Sys.time()
	situation <- list()
	
	for (i in 1:B) {
		
		datasetCurr <- stocksim(psi = corr, beta1_1 = slope1, beta2_1 = slope2)
		datasetCurr <- cbind(Date = 1:4000, datasetCurr)
		datasetCurr$ratio <- datasetCurr$Close.Stock1 / datasetCurr$Close.Stock2
		
		# separate dataset in to training and testing
		training <- datasetCurr[1:2000, ]
		testing  <- datasetCurr[2001:4000, ]
		
		# find the best k to 2 decimal places
		best.k <- find.best.k(dataset = training, origin = .5,  ending = 2.5, interval = .5, k.best = NULL, precision = 2)
		
		# calculate profit using the best.k 
		maxprofit <- unlist(best.k[2]) + findGrandProfit(dataset = testing, ks = unlist(best.k[1]), plot.bool = FALSE, p = .001, stock1.name = NULL, stock2.name = NULL, training = FALSE, m = mean(datasetCurr$ratio), sd = sd(datasetCurr$ratio))
		
		# store this iteration of the simulation (so we can find average, sd, etc. later)
		situation[i] <- maxprofit
		
	} # end for
	
	# After all simulations {
		
		endtime <- Sys.time()
		elapsedtime <- endtime - starttime
		# return B, corr, slope1, slope2, mean, sd, min, max, starttime, endtime, elapsedtime
		d <- unlist(situation)  
		situation.data <- as.data.frame(cbind(B = B, corr = corr, slope1 = slope1, slope2 = slope2, mean = mean(d), sd = sd(d), min = min(d), max = max(d), elapsedtime = elapsedtime))
		
	# }
}
#
# end simstudy()
#
##
####
########

########
####
##
#
# find.best.k()
#
# dataset: dataset on which to find best k (will probably be the training dataset)
# origin: the smallest value of k to test
# ending: the largest value of k to test
# interval: the interval size between origin and ending to test (note: the number of k's being tested for iteration of find.best.k will be (ending - origin) / interval)
# k.best: initially NULL; the current list of the (k which maximises the profit) and the (max profit).
# precision: number of decimal places you want k to be; if there are multiple k's that yield the best profit, find.best.k() will return the median of the k's; precision doesn't exactly work they way i'd like it to since it depends on your original interval. 
# does.stock1.split: boolean indicating whether stock1 splits (if TRUE, that means the stock1 splits at some point)
# does.stock2.split: boolean indicating whether stock2 splits (if TRUE, that means the stock2 splits at some point)
#
# function returns the k that maximises the profit of the dataset to the number of decimal places indicated by precision and the profit. Note as precision increases, time to run increases
#
find.best.k <- function(dataset, origin, ending, interval, k.best, precision, does.stock1.split = FALSE, does.stock2.split = FALSE, ...) {
	#browser()
	
	if (precision != 0) { # base case; i.e., we want our k to be more precise than it currently is
		
		# initialize k to the values we want to explore
		k <- seq(origin, ending, interval)
		
		# create list of k values and their corresponding profits 
		k.profit <- sapply(k, FUN = function(i) findGrandProfit(dataset, ks = i, plot.bool = FALSE, p = .001, stock1.name = NULL, stock2.name = NULL, does.stock1.split = does.stock1.split, does.stock2.split = does.stock2.split))
		
		# isolate the max profit and it's corresponding k
		k.best <- list(k[k.profit == max(k.profit)], max(k.profit))
		
		# recursive call to find.best.k using a narrower search for k
		find.best.k(dataset = dataset, unlist(k.best)[1] - interval, unlist(k.best)[1] + interval, interval / 5, k.best, precision - 1, does.stock1.split = does.stock1.split, does.stock2.split = does.stock2.split) # call recursively
		
	} else { # i.e., we don't want a k any more precise than it already is; exit recursion and return k.best
		
		if (length(k.best) > 1) { # if there are multiple best k's (that yield the best profit), choose the middle (roughly)
			k.best <- list(unlist(k.best[1])[ceiling(length(unlist(k.best[1]))/2)], unlist(k.best[2]))
		
		} # end if
		
		return(k.best)
	} # end if else
}
#
# end find.best.k()
#
##
####
########


########
####
##
#
# find.all.ks()
#
# dataset: dataset on which to find best k (will probably be the training dataset)
# origin: the smallest value of k to test
# ending: the largest value of k to test
# interval: the interval size between origin and ending to test (note: the number of k's being tested for iteration of find.best.k will be (ending - origin) / interval)
# does.stock1.split: boolean indicating whether stock1 splits (if TRUE, that means the stock1 splits at some point)
# does.stock2.split: boolean indicating whether stock2 splits (if TRUE, that means the stock2 splits at some point)
#
# function returns all ks and their profit values for the purpose of plotting
#
find.all.ks <- function(dataset, origin, ending, interval, does.stock1.split = FALSE, does.stock2.split = FALSE, ...) {
	#browser()
	
	# initialize k to the values we want to explore
	k <- seq(origin, ending, interval)
	
	# create list of k values and their corresponding profits 
	k.profit <- sapply(k, FUN = function(i) findGrandProfit(dataset, ks = i, plot.bool = FALSE, p = .001, stock1.name = NULL, stock2.name = NULL, does.stock1.split = does.stock1.split, does.stock2.split = does.stock2.split))
	
	# return all ks and their profit values
	return(as.data.frame(cbind(unlist(k), unlist(k.profit))))
}
#
# end find.all.ks()
#
##
####
########

# No cross-correlation, no trends:
case1 <- stocksim() # what the parameters default to
# Cross-correlation, no trends:
case2 <- stocksim(psi = .9)
# Cross-correlation, both increasing:
case3 <- stocksim(psi = .9, beta1_1 = 0.01, beta2_1 = 0.01)
# Cross-correlation, opposite trends:
case4 <- stocksim(psi = .9, beta1_1 = 0.01, beta2_1 = -0.01)

# Plot the four cases
par(mfrow = c(2,2))
plot(case1$Close.Stock1, type = 'l', ylim = range(case1$Close.Stock1, case1$Close.Stock2), col = 'navy', ylab = 'Simulated Closing Price', xlab = 'Day', main = 'No cross-correlation, no trends')
lines(case1$Close.Stock2, type = 'l',  col = 'lightblue')
plot(case2$Close.Stock1, type = 'l', ylim = range(case2$Close.Stock1, case2$Close.Stock2), col = 'navy', ylab = 'Simulated Closing Price', xlab = 'Day', main = 'Cross-correlation, no trends')
lines(case2$Close.Stock2, type = 'l', col = 'lightblue')
plot(case3$Close.Stock1, type = 'l', ylim = range(case3$Close.Stock1, case3$Close.Stock2), col = 'navy', ylab = 'Simulated Closing Price', xlab = 'Day', main = 'Cross-correlation, both increasing')
lines(case3$Close.Stock2, type = 'l', col = 'lightblue')
plot(case4$Close.Stock1, type = 'l', ylim = range(case4$Close.Stock1, case4$Close.Stock2), col = 'navy', ylab = 'Simulated Closing Price', xlab = 'Day', main = 'Cross-correlation, opposite trends')
lines(case4$Close.Stock2, type = 'l', col = 'lightblue')

# Combinations
cls <- makeCluster(rep('localhost', 4))
beg <- Sys.time()
B		= rep(1, 30)
corr	= rep(c(0, .2, .4, .6, .8, .95), each = 5)
slope1	= rep(c(0, .01, -.01, .01, -.01), 6)
slope2	= rep(c(0, .01, -.01, -.01, .01), 6)
boo <- as.data.frame(cbind(B, corr, slope1, slope2))
boolist <- list()
for (i in 1:nrow(boo)) {
	hold <- c(B[i], corr[i], slope1[i], slope2[i])
	boolist[[i]] <- hold
}
ans <- clusterApply(cls, boolist, simstudy)
end <- Sys.time()
total = end - beg




