# QUESTION 1 

rm(list = ls()) 
if( !is.null(dev.list() )) dev.off() 
set.seed(12345) 

## PART A 

data <- rgamma(n = 1000, shape = 5, rate = 2)

results_A <- function(parameters)
{
  r <- parameters[1] 
  lambda <- parameters[2] 
  loglikelihood.gamma <- ( 1000 * r * log(lambda) - lambda * sum(data) + (r - 1) * sum(log(data)) - 1000 * log(gamma(r)) ) 
  return(loglikelihood.gamma)
}

## PART B 

results_B <- function(parameters)
{
  return(-results_A(parameters))
}

InitialValueOne <- nlminb(c(1, 1), objective = results_B, hessian = TRUE)
InitialValueTwo <- nlminb(c(5, 5), objective = results_B, hessian = TRUE)
InitialValueThree <- nlminb(c(10, 10), objective = results_B, hessian = TRUE)

list_Data <- list(InitialValueOne,InitialValueTwo,InitialValueThree)
print( list_Data ) 

## PART C 

lambda.values <- seq(1, 50 , 2)
gamma.values <- c()

i <- 1
for(lambda in lambda.values)
{
  parameters <- c(5, lambda)
  gamma.values[i] <- results_A(parameters)
  i <- i + 1
}

plot(lambda.values, gamma.values, xlab = "for various values of lambda, r = 5 (constant)", ylab = "LOG-LIKELIHOOD", main = "LOG VALUES FOR VARIOUS VALUES OF Lambda", col="red")

## PART D 

RandomValues <- seq(1, 50 , 2 )
gamma.values <- c()

i <- 1
for(rv in RandomValues )
{
  parameters <- c(rv, rv)
  gamma.values[i] <- results_A(parameters)
  i <- i + 1
}

plot(RandomValues, gamma.values, xlab = "for various values of lambda, r", ylab = "LOG-LIKELIHOOD", main = "LOG VALUES FOR VARIOUS VALUES OF Lambda and R", col="red")
