library(dygraphs)
library(forecast)
library(tseries)
library(lmtest)


monthly <- read.csv("monthly.csv")
quarterly <- read.csv('quarterly.csv')
extradata <- read.csv("extradata.csv")

new.mortgages <- monthly$Total.amount.of.new.indiv.mortg..in.Afl.mln...Comm.Banks.
new.mortgages <- ts(new.mortgages, start=c(2005, 10), end=c(2017, 4), frequency=12) 
hous.number.permits <- ts(extradata$Number.of.construction.permits.granted.Houses, start = c(1997,1), end = c(2017,1), frequency = 4)
hous.value.permits <- ts(extradata$Total.value.of.construction.permits..x.Afl..million..Houses, start = c(1997,1), end = c(2017,1), frequency = 4)

#convert monthly new mortgages to quarterly (via summation)
q_new.mortgages <- aggregate(new.mortgages, nfrequency=4)
q_new.mortgages

# Graphing
plot(hous.value.permits)
plot(q_new.mortgages, xlab = "Time", ylab = "Total amount of new individual mortgages (in Afl. million)")

# test for unit root and number of differences required
ndiffs(hous.number.permits, alpha=0.05, test=c("kpss","adf", "pp"), max.d=2)
ndiffs(hous.value.permits, alpha=0.05, test=c("kpss","adf", "pp"), max.d=2)
ndiffs(q_new.mortgages, alpha=0.05, test=c("kpss","adf", "pp"), max.d=2)
# So hous.number.permits and new mortgages has unit root, need to remove it.

# First difference
d_hous.number.permits <- diff(hous.number.permits)
d_new.mortgages <- diff(q_new.mortgages)

# ADF test
adf.test(d_hous.number.permits)
adf.test(hous.value.permits)
adf.test(d_new.mortgages)


# According to test results can reject te null hypothesis that 
# there is a unit root for d_hous.number.permits and d_new.mortgages



# Identified 3 variables of interest: d_hous.number.permits, hous.value.permits, d_new.mortgages
plot(d_hous.number.permits)
plot(hous.value.permits)
plot(d_new.mortgages)

# granger causality
d.new.mortgages <- window(d_new.mortgages, start=c(2006,1), end=c(2017,1))
d.hous.number.permits <- window(d_hous.number.permits, start=c(2006,1), end=c(2017,1))

grangertest(d_new.mortgages ~ d_hous.number.permits,
            order=4)






