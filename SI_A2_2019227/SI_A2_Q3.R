n<-25
dataSmokers <- c(124, 134, 136, 125, 133, 127, 135, 131, 133, 125, 118)
numberOfSmokers <- 11 
dataNonSmokers <- c(130, 122, 128, 129, 118, 122, 116, 127, 135, 120, 122, 120, 115, 123)
numberOfNonSmokers <- 14 

SmokersMean <- mean(dataSmokers)
SmokersMean

NonSmokersMean <- mean(dataNonSmokers)
NonSmokersMean

SmokersStandardDeviation <- sd(dataSmokers)
SmokersStandardDeviation

NonSmokersStandardDeviation <- sd(dataNonSmokers)
NonSmokersStandardDeviation

muNull <- 0 
testStatistic<-((SmokersMean-NonSmokersMean)-muNull)/sqrt(((SmokersStandardDeviation^2)/numberOfSmokers)+((NonSmokersStandardDeviation^2)/numberOfNonSmokers))
c('test statistic', testStatistic)

testTest <- t.test(x=dataSmokers, y=dataNonSmokers, paired=FALSE, var.equal=FALSE,
                   alternative="two.sided")

test_Statistic <- testTest
c('t_stat_inbuilt', test_Statistic)

degreesOfFreedom <- testTest$parameter
c('degreesOfFreedom',degreesOfFreedom)

tCriticalValue <-(qt(p=0.025, df=21.66, lower.tail=FALSE))
c('tCriticalValue',tCriticalValue)

pValue <- 2*pt(q=testStatistic, df=21.66, lower.tail=FALSE)
c('pValue', pValue)

pValueFunc <- testTest$p.value
c('p_value', pValueFunc)

alpha <- 0.05 

if (pValue < alpha) print("We Reject the Null Hypothesis")else print("We Accept the Null Hypothesis")

