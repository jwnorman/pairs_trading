
# libraries to load:
library(lattice)
library(RColorBrewer)

# Load data
setwd("~/Desktop/Fall_2013/STA_141/Homework_3_TradingSim/")
Stock1 <- read.table("gaps2.csv", sep = ",", header = TRUE) # gaps2 is from Yahoo
Stock2 <- read.table("urban2.csv", sep = ",", header = TRUE) # urban2 is from Yahoo
Stock1 <- Stock1[!is.na(Stock1$Close), ] # make sure there are no NA's for the close column;
Stock2 <- Stock2[!is.na(Stock2$Close), ]
Stock1$Date <- as.Date(Stock1$Date, format = "%m/%d/%y") # format dates to class Date
Stock2$Date <- as.Date(Stock2$Date, format = "%m/%d/%y") 
Stock1.Stock2 <- merge(Stock1, Stock2, by = "Date", suffixes = c(".Stock1", ".Stock2"), all = TRUE) # merge stocks
Stock1.Stock2 <- Stock1.Stock2[!is.na(Stock1.Stock2$Close.Stock1) & !is.na(Stock1.Stock2$Close.Stock2), ] # remove missing day's data
Stock1.Stock2$ratio <- Stock1.Stock2$Adj.Close.Stock1 / Stock1.Stock2$Adj.Close.Stock2 # calculate ratio
nums_training_year <- 5 # specify number of training years; using 5 in this hw assignment
training <- Stock1.Stock2[Stock1.Stock2$Date <= (min(Stock1.Stock2$Date) + 365*nums_training_year), c("Date", "Close.Stock1", "Close.Stock2", "Adj.Close.Stock1", "Adj.Close.Stock2", "ratio")]
testing <- Stock1.Stock2[Stock1.Stock2$Date > (min(Stock1.Stock2$Date) + 365*nums_training_year), c("Date", "Close.Stock1", "Close.Stock2", "Adj.Close.Stock1", "Adj.Close.Stock2", "ratio")]

par(mfrow = c(2,1))
plot(Stock1$Date, Stock1$Close, type = 'l', main = 'Closing Price of Gap', xlab = 'Date', ylab= 'Closing Price')
plot(Stock2$Date, Stock2$Close, type = 'l', main = 'Closing Price of Urban', xlab = 'Date', ylab= 'Closing Price')



########
####
##
#
# Open() 
#
# DateCurr: the date we want to Open()
# Dollar.Sell: the amount (in dollars) we want to sell of the stock we want to sell
# Dollar.Buy: the amount (in dollars) we want to buy of the stock we want to buy
# p: the handling fee to multiply by the abs value of money transacted
# status: the location (will be 'a' (if less than m - ks) or 'd' (if greater than m + ks))
#
# Function returns list: [<units sold>, <units bought>, <amount given to broker>]
#
Open <- function(dataset, DateCurr, Dollar.Sell, Dollar.Buy, p, status) {
	if (status == 'a') { # sell stock 2, buy stock 1
		units.sold <- Dollar.Sell/dataset[dataset$Date == DateCurr, c("Close.Stock2")]
		units.bought <- Dollar.Buy/dataset[dataset$Date == DateCurr, c("Close.Stock1")]
	}
	if (status == 'd') { # sell stock 1, buy stock 2
		units.sold <- Dollar.Sell/dataset[dataset$Date == DateCurr, c("Close.Stock1")]
		units.bought <- Dollar.Buy/dataset[dataset$Date == DateCurr, c("Close.Stock2")]	
	}
	fee <- p * (abs(Dollar.Sell) + abs(Dollar.Buy))
	list(units.sold, units.bought, fee)
}
#
# end Open()
#
##
####
########

########
####
##
#
# Close() 
#
# DateCurr: the date we want to Close()
# sellback.shares: the amount (in stock units) that we want to sell (back) of the stock we want to sell (back)
# buyback.shares: the amount (in stock units) that we want to buy (back) of the stock we want to buy (back)
# status: the location (will be 'b' (if m - ks < ratio < m) or 'c' (if m < ratio < m + ks))
# p: the handling fee to multiply by the abs value of money transacted
#
# Function returns list: [<dollars from sellback>, <dollars from buyback>]
#
Close <- function(dataset, DateCurr, sellback.shares, buyback.shares, p, status) {
	if (status == 'b' | status == 'a') { # buyback stock 1, sell back stock2
		dollars.sellback <- sellback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock2")]
		dollars.buyback <- buyback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock1")]
	}
	if (status == 'c' | status == 'd') {
		dollars.sellback <- sellback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock1")]
		dollars.buyback <- buyback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock2")]		
	}
	fee <- p * (abs(dollars.sellback) + abs(dollars.buyback))
	list(dollars.sellback, dollars.buyback, fee)
}
#
# end Close()
#
##
####
########

########
####
##
#
# End() 
#
# DateCurr: the date we want to Close()
# sellback.shares: the amount (in stock units) that we want to sell (back) of the stock we want to sell (back)
# buyback.shares: the amount (in stock units) that we want to buy (back) of the stock we want to buy (back)
# status: the location (will be 'b' (if m - ks < ratio < m) or 'c' (if m < ratio < m + ks))
# p: the handling fee to multiply by the abs value of money transacted
#
# Function returns list: [<dollars from sellback>, <dollars from buyback>]
#
End <- function(dataset, DateCurr, sellback.shares, buyback.shares, p, status) {
	if (status == 'b' | status == 'd') { # buyback stock 1, sell back stock2
		dollars.sellback <- sellback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock2")]
		dollars.buyback <- buyback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock1")]
	}
	if (status == 'c' | status == 'a') {
		dollars.sellback <- sellback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock1")]
		dollars.buyback <- buyback.shares*dataset[dataset$Date == DateCurr, c("Close.Stock2")]		
	}
	fee <- p * (abs(dollars.sellback) + abs(dollars.buyback))
	list(dollars.sellback, dollars.buyback, fee)
}
#
# end End()
#
##
####
########

########
####
##
#
# findNextOpen()
#
# DateCurr: the date we want to "look forward" from
#
# Function returns a Date: the date of which has our next place to open
#
findNextOpen <- function(dataset, DateCurr) {
	# find all times the ratio is status a or d AND the date is AFTER or EQUAL the day we just opened on
	training.aord <- dataset[(dataset$status == 'a' | dataset$status == 'd') & (dataset$Date >= DateCurr), ]
	# find next time the ratio is status a or d AFTER or EQUAL the day we just opened on
	next.aord <- training.aord[dataset$Date == min(dataset$Date), ]
	next.aord$Date
}
#
# end findNextOpen()
#
##
####
########


########
####
##
#
# findNextClose()
#
# DateCurr: the date we want to "look forward" from
# StatusCurr: the current location (will be either 'a' or 'd')
#
# Function returns a Date: the date of which has our next place to close
#
findNextClose <- function(dataset, DateCurr, StatusCurr) {
	if (StatusCurr == 'a') {
		training.borc <- dataset[(dataset$status == 'c' | dataset$status == 'd') & (dataset$Date >= DateCurr), ]
		next.borc <- training.borc[dataset$Date == min(dataset$Date), ]
	}
	if (StatusCurr == 'd') {
		training.borc <- dataset[(dataset$status == 'a' | dataset$status == 'b') & (dataset$Date >= DateCurr), ]
		next.borc <- training.borc[dataset$Date == min(dataset$Date), ]
	}	
	next.borc$Date
}
#
# end findNextClose()
#
##
####
########

########
####
##
#
# ratio.plot()
#
ratio.plot <- function(dataset, nextday, which.plot, avg, k, stock1, stock2) {
	if (which.plot == 1) { # if we haven't plotted yet, we just want to print the ratio, mean, and k's
		plot.new()
		mycolors <- brewer.pal(4, "Reds")
		plot(dataset$Date, dataset$ratio, type = 'l', xlab = 'Date', ylab = 'Ratio', main = paste(stock1, '/', stock2, 'Closure Ratio\nfrom', format(min(dataset$Date), "%B %d, %Y"), 'to', format(max(dataset$Date), "%B %d, %Y")))
		abline(avg, 0)
		abline(avg - k, 0, lty = 2) # +k
		abline(avg + k, 0, lty = 2) # -k
	}
	if (which.plot == 2) { # if we have already plotted and now we want to add circles
		symbols(dataset$Date[dataset$Date == nextday], dataset$ratio[dataset$Date == nextday], circles = c(55), inches = FALSE, add = TRUE, fg = 'red')
	}
}
#
# end ratio.plot()
#
##
####
########

########
####
##
#
# findGrandProfit()
#
# dataset: the training data.frame which should include "Date", "Close.Stock1", "Close.Stock2", "ratio"
# ks: number of standard deviations; used for determining where to Open() and where to Close()
# plot.bool: default to FALSE; if set to true, a time series plot will be plotted of the ratio over time with a line for the mean, mean - ks, mean + ks, and circles at the first date, Close()ing and Open()ing dates, and last date
#
# Function returns a number representing the grand profit in dollars
#
findGrandProfit <- function(dataset, ks, plot.bool = FALSE, p, stock1.name, stock2.name, does.stock1.split = FALSE, does.stock2.split = FALSE, training = TRUE, m = NULL, sd = NULL, ...) {
	
	# Deal with splits by using adj.closing instead of closing
	if(does.stock1.split == TRUE) { dataset$Close.Stock1 <- dataset$Adj.Close.Stock1 }
	if(does.stock2.split == TRUE) { dataset$Close.Stock2 <- dataset$Adj.Close.Stock2 }
	
	# Calculate ratio
	dataset$ratio <- dataset$Close.Stock1 / dataset$Close.Stock2 
	
	# Cheat way of dealing with ks = 0 (for bad user input)
	if (ks <= 0) { ks <- 0.0000001 }
	
	# Prepare data (is it training or testing?)
	if (training == FALSE) { 		# if we are dealing with testing data
		# m <- m  					# we want to use the training mean (passed in)
		ks <- ks*sd					# we want to use the training sd (passed in)
	} else {						# if we are dealing with training data
		m <- mean(dataset$ratio)	# we want to use the mean of training dataset
		ks <- ks*sd(dataset$ratio)	# we want to use the sd of trianing dataset
	} # end if else
	
	# find status of each point
		# a = (-Inf, m - ks]
		# b = (m - ks, m]
		# c = (m, m + ks]
		# d = (m + ks, Inf]
	dataset$status <- cut(dataset$ratio, breaks = c(-Inf, m - ks, m, m + ks, Inf), labels = c("a", "b", "c", "d"))
	
	# Plot ratio with mean and k
	if (plot.bool == TRUE) { ratio.plot(dataset, NULL, 1, m, ks, stock1.name, stock2.name) }
		
	# Find first day of dataset and add a circle to that point
	nextdate <- min(dataset$Date)
	if (plot.bool == TRUE) { ratio.plot(dataset, nextdate, 2, m, ks, NULL, NULL) }
	
	# initialize grand profit to zero
	grand.profit <- 0
	
	# Run the following while loop until the function returns 
	# (which will occur after we get to the last date)
	while (TRUE) { 
		nextdate <- findNextOpen(dataset, nextdate)
		
		# If nextdate is NA, i.e. we've reached the last date of our dataset
		if (is.na(nextdate)) {
			nextdate <- max(dataset$Date)
			
			# Add a circle to the last point
			if (plot.bool == TRUE) { ratio.plot(dataset, nextdate, 2, m, ks, NULL, NULL) }
				
			# Isolate the observation of the last point	
			ObsCurr <- dataset[dataset$Date == nextdate, ]
			
			# If the last date has status 'a' or 'd' (specified above), 
			# then we need to close before we return; If the last point is 'b' or 'c',
			# then we have already previously closed
			if (as.character(ObsCurr$status) == 'a' | as.character(ObsCurr$status) == 'd') {
				
				# End() will return a list <dollars.sellback, dollars.buyback, dollars.fee>
				dollars.list <- End(dataset, max(dataset$Date), units.bought, units.sold, p, as.character(ObsCurr$status))
				dollars.sellback <- unlist(dollars.list[1]) # dollars from sellback
				dollars.buyback <- unlist(dollars.list[2]) # dollars from buyback
				dollars.fee <- unlist(dollars.list[3])
				
				# Calculate profit for the current open / closing pair
				local.profit <- (1 - dollars.buyback) + (dollars.sellback - 1) - dollars.fee
				
				# Calculate profit for the overall profit thus far
				grand.profit <- grand.profit + local.profit # should be end of function
			} # end if
			
			# We have dealt with the last point so return grand.profit
			return(grand.profit)
		} # end if -- i.e. if nextdate isn't NA
		
		# Add circle to the plot (circling the spot where we're opening)
		if (plot.bool == TRUE) { ratio.plot(dataset, nextdate, 2, m, ks, NULL, NULL) }
			
		# OPEN process {
			
			# Isolate the observation of the current point	
			ObsCurr <- dataset[dataset$Date == nextdate, ]
			
			# Open() will return a list <units.sold, units.bought, units.fee>
			units.list <- Open(dataset, nextdate, 1, 1, p, as.character(ObsCurr$status))
			units.sold <- unlist(units.list[1])
			units.bought <- unlist(units.list[2])
			units.fee <- unlist(units.list[3]) # dollars spent on fee
			
			# No change is made to the grand profit other than the transaction fee from opening
			grand.profit <- grand.profit - units.fee
			
		# } end OPEN process
		
		nextdate <- findNextClose(dataset, nextdate, as.character(ObsCurr$status))
		
		# If nextdate is NA, i.e. we've reached the last date of our dataset
		if (is.na(nextdate)) {
			nextdate <- max(dataset$Date)
			
			# Add circle to plot at last point
			if (plot.bool == TRUE) { ratio.plot(dataset, nextdate, 2, m, ks, NULL, NULL) }
			
			# Isolate the observation of the current point
			ObsCurr <- dataset[dataset$Date == nextdate, ]
			
			# If the last date has status 'a' or 'd' (specified above), 
			# then we need to close before we return; If the last point is 'b' or 'c',
			# then we have already previously closed
			if (as.character(ObsCurr$status) == 'a' | as.character(ObsCurr$status) == 'd') {
				
				# End() will return a list <dollars.sellback, dollars.buyback, dollars.fee>
				dollars.list <- End(dataset, max(dataset$Date), units.bought, units.sold, p, as.character(ObsCurr$status))
				dollars.sellback <- unlist(dollars.list[1]) # dollars from sellback
				dollars.buyback <- unlist(dollars.list[2]) # dollars from buyback
				dollars.fee <- unlist(dollars.list[3])
				
				# Calculate profit for the current open / closing pair
				local.profit <- (1 - dollars.buyback) + (dollars.sellback - 1) - dollars.fee
				
				# Calculate profit for the overall profit thus far
				grand.profit <- grand.profit + local.profit # should be end of function
			} # end if
			
			# We have dealt with the last point so return grand.profit
			return(grand.profit)
		} # end if -- i.e. if nextdate isn't NA
		
		# Add circle to the plot (circling the spot where we're opening)
		if (plot.bool == TRUE) { ratio.plot(dataset, nextdate, 2, m, ks, NULL, NULL) }
		
		# CLOSE process {
			
			# Isolate the observation of the current point
			ObsCurr <- dataset[dataset$Date == nextdate, ]
			
			# Close() will return a list <dollars.sellback, dollars.buyback, dollars.fee>
			dollars.list <- Close(dataset, nextdate, units.bought, units.sold, p, as.character(ObsCurr$status))
			dollars.sellback <- unlist(dollars.list[1])
			dollars.buyback <- unlist(dollars.list[2])
			dollars.fee <- unlist(dollars.list[3])
			
			# Calculate profit for the current open / closing pair
			local.profit <- (1 - dollars.buyback) + (dollars.sellback - 1) - dollars.fee
		# }
		
		# Calculate profit for the overall profit thus far
		grand.profit <- grand.profit + local.profit # should be end of function
		
		# Don't need these lines because my code already deals with these situations
		#if (nextdate == max(dataset$Date))
		#	return(grand.profit)
	}
}
#
# end findGrandProfit()
#
##
####
########

allks <- find.all.ks(training, 0, 3, .01, does.stock1.split = TRUE, does.stock2.split = TRUE) # all k's
plot(allks$V1, allks$V2, type = 'l') # plot of ks
optimal.k <- find.best.k(dataset = training, origin = 0, ending = 3, interval = .1, k.best = NULL, precision = 4, does.stock1.split = TRUE, does.stock2.split = TRUE) # best k
findGrandProfit(dataset = training, ks = unlist(optimal.k[1]), plot.bool = TRUE, p = .001, stock1.name = "Gap", stock2.name = "Urban Outfitters", does.stock1.split = TRUE, does.stock2.split = TRUE, training = TRUE)

# Using optimal.k, run algorithm on rest of data (testing)
#	In order to do so, I must include 3 additional parameters:
#		1. training = FALSE
#		2. m = mean of training data
#		3. sd = sd of training data
findGrandProfit(dataset = testing, ks = unlist(optimal.k[1]), plot.bool = TRUE, p = .001, stock1.name = "Gap", stock2.name = "Urban Outfitters", training = FALSE, m = mean(training$ratio), sd = sd(training$ratio))

