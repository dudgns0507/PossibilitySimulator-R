rwalk <- function() {
  # initial price
  p0 <- 1950
  
  mu <- 0.1
  sigma <- 0.2
  
  # set daily variation value
    days <- 500
  mu.daily <- mu/days
  sigma.daily <- sigma * 1/sqrt(days)
  
  # generate 365 random number
  r <- rnorm(365, mu.daily, sigma.daily)
  
  # log(start price) + accumulate total of random number
  logPrice <- log(p0) + cumsum(r)
  
  # exponential distribution of logPrice
  Prices <- exp(logPrice)
  return(Prices)
}

c <- rainbow(5)
for(x in 1:1) {
  result <- rwalk()
  if(x == 1)
    plot(result, type="l", ylim=c(1800,2200))
  # plot(result, type="l", col = c[x])
  lines(result, col = c[x])
}