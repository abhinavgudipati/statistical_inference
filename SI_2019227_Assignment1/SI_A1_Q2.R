set.seed(12345)
if( !is.null(dev.list() )) dev.off() 
rm(list = ls())

setwd("/Users/abhinavgudipati/Desktop")

data = read.table('data_Q2.csv',header = TRUE,sep=",",colClasses = c("NULL","numeric"))[,1]

mainFunction <- function(parameters)
{
  mean = parameters[1]
  variance = parameters[2]
  log.likelihood.normal.distribution = -(1000/2) * log(variance) - (1000/2)*log(2*pi) - sum((data-mean)^2)/(2 * variance)
  return (-1*log.likelihood.normal.distribution)
}

helperFunction <- function(parameters){
  return (-mainFunction(parameters))
}
result <- nlminb(start=c(2, 2), objective=mainFunction, hessian = TRUE)
print(result)
parameters.values = seq(1, 100)

# PART A 

llnormal = c()
i = 0  

while(i<=100)
{
  llnormal[i] = helperFunction(c(parameters.values[i], 9))
  i = i + 1 
}

plot(parameters.values, llnormal, xlab="VARIANCE CONSTANT", ylab="LOG-LIKELIHOOD", main = "PART A",col="red")

# PART B 

llnormal = c()
i = 0 

while(i <= 20)
{
  llnormal[i] = helperFunction(c(70, parameters.values[i]))
  i = i + 1
}

plot(seq(1,20), llnormal, xlab="MEAN CONSTANT", ylab="LOG-LIKELIHOOD", main = "PART B",col="blue")
