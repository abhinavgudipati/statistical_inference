


sampleStandardDeviation <- 0.3   
sampleMean <- 1.0   
n <- 28       

U_0 <- 0.8    

tTest <- (sampleMean - U_0) / (s / sqrt(n))

c('test statistic',tTest)

pValue <- pt(tTest, df=n-1, lower.tail = FALSE) * 2

alpha <- 0.05

c('pValue',pValue)

if (pValue < alpha) print("We Reject the Null Hypothesis")else print("We Accept the Null Hypothesis")
