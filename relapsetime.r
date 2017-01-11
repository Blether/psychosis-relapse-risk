# time to first relapse
# uses data 2012-2013 discharges
# 
# 2016-06 to 2017-01

library(survival)

survplot = function(survmodel, ...) {
	plot(survmodel, axes=0, xlab='time from starting Early Intervention (years)', ylab='proportion without major relapse', ...)
	axis(2)
	axis(1, at=seq(0,max(relapse1time), 365), labels = seq(0, 7))
}


addline = function(x_years, y, ...) {
	lines(c(x_years*365, x_years*365) , c(0, y), ...)
	lines(c(0, x_years*365) , c(y, y), ...)
}

addsetoflines = function(x, y) {
	for (i in 1:length(x)) {
		addline(x[i], y[i], col='blue', lty='solid')
	}
}

addverts = function(x, y, atext='1/4') {
	for (i in 1:length(x)) {
		lines(c(x[i], x[i]), c(0, y[i]), col='blue', lty='longdash')
	}
	nx=c(0, x)
	for (i in 2:length(nx)) {
		arrows(nx[i-1], .05, nx[i], length=.05, code=3, col='blue')
		text(sum(nx[i-1], nx[i])/2, .07, atext, col='blue', cex=.8)
	}
} 

addmodelfit = function(model, minprop=.8, ...) {
	y = seq(.00001, minprop, .05)
	p = predict(model, type = 'quantile', p= y)[1,]
	lines(p, 1-y, lty='solid', ...)
}

data = read.csv('2013_relapse_data.csv', stringsAsFactors = FALSE)
data = subset(data, team < 4 )


startEIdate = as.Date(data$assess1, '%m/%d/%Y')
endEIdate = as.Date(data$discheidate, '%m/%d/%Y')
relapse1date = as.Date(data$relap1date, '%m/%d/%Y')

dayswithEI = as.integer(endEIdate - startEIdate)
daystorelapse1 = as.integer(relapse1date - startEIdate)


# status convention 0=alive, 1=dead i.e. 0=censored, 1=relapsed
relapse1status = numeric(length(dayswithEI)) # all zeroes
relapse1time = dayswithEI

relapse1time[!is.na(daystorelapse1)] = daystorelapse1[!is.na(daystorelapse1)]
relapse1status[!is.na(daystorelapse1)] = 1

ei.surv = survfit(Surv(relapse1time, relapse1status) ~ 1, data = data)


ei.loglog.model = survreg(Surv(relapse1time, relapse1status) ~ 1, data = data, subset=(relapse1time > 0), dist='loglogistic')


survplot(ei.surv, xlim=c(0,4*365), conf.int='none')
addmodelfit(ei.loglog.model, col='green')
# these values extracted from survival table manually - maybe better get from model?
addline(1, .71, col='grey', lty='longdash')
addline(2, .554, col='grey', lty='longdash')
addline(4, .41, col='grey', lty='longdash')
arrows(.05*365, 1, y1=.71, length=.05, code=3, col='grey')
text(.06, .8, '29%', col='grey', cex=.7, pos=4)
arrows(.05*365, .71, y1=.554, length=.05, code=3, col='grey')
text(.06, .62, '16%', col='grey', cex=.7, pos=4)
arrows(.05*365, .554, y1=.41, length=.05, code=3, col='grey')
text(.06, .48, '15%', col='grey', cex=.7, pos=4)

# plot with model and quartiles of relapses shown

y = seq(.146, .60, .146)
p = predict(ei.loglog.model, type = 'quantile', p= y)[1,]

survplot(ei.surv, xlim=c(0,4*365), conf.int='none')
addmodelfit(ei.loglog.model, col='green')
addverts(p, 1-y)


