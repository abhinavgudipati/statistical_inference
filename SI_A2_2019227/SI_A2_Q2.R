
data <- c(5.728, 5.731, 5.722, 5.719, 5.727, 5.724, 5.718, 5.726, 5.723, 5.722)
n <- 10
sigmaNOT <- 0.4

sampleMean <- mean(data)
sampleMean

sampleStandardDeviation <- sd(data)
sampleStandardDeviation

chiSquareTestStatistic <- ((n-1)*sampleStandardDeviation**2)/(sigmaNOT**2)
c('chiSquareTestStatistic', chiSquareTestStatistic)

chiSquareCriticalValue <- (qchisq(0.05, df=n-1, lower.tail=TRUE))
c('chiSquareCriticalValue', chiSquareCriticalValue)

p_Value<-pchisq(q=chiSquareTestStatistic, df=n-1, lower.tail=TRUE)
c('pValue', p_Value)

alpha <- 0.05 

if (p_Value < alpha) print("We Reject the Null Hypothesis")else print("We Accept the Null Hypothesis")
