#PERSONNAL DATA STUDY: FREQUENCY OF BOOKS BY WEIGHT CLASS
#Load libraries
library(ggplot2)
#Data
X <- seq(from = 0.0001, to = 4.0001, by = 0.25)
Y <- (c(0, 1, 6, 3, 3, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0)/21)
DF<- data.frame(X,Y)
ggplot(data=DF, aes(X, Y)) +
  geom_point(col = "darkgreen", size=2)

#Data is skewed to the right, seems like a lognormal or poisson distribution 

#Lognormal
#Scientific model
logreg <- function(X, mu, sigma) {
  mean <- (exp(1)^-((log(X))^2/(2*(sigma^2))))/(X*sigma*sqrt(2*pi))
  return(mean)
}


logreg(X, 1.3, 1.7)
with(DF, plot(X, logreg(X, 1.3, 1.7)))

DF <-  DF %>% mutate(avg = logreg(X, 1.3, 1.7))
View(DF)
ggplot(DF, aes(X, Y))+
  geom_point(col = "darkgreen")+
  geom_point(aes(X,avg), col = "steelblue")

#likelihood function
llreg <- function(y, x, mu, sigma, stdv) {
  mean <- (exp(1)^-((log(x))^2/(2*(sigma^2))))/(x*sigma*sqrt(2*pi))
  loglike <- sum(dlnorm(y, mean, sd=stdv, log=TRUE))
  return(loglike)
}

llreg(Y, X, 1.3, 1.7, 4)

runs <- 5000
muvec <- runif(runs)
sigvec <- numeric(runs)
sdvec <- numeric(runs)
loglikevec <- numeric(runs)

for(i in 1:runs) {
  mu <- runif(1, min=0, max=2)
  sigma <- runif(1, min=0, max=2)
  stdv <- runif(1, min=0, max=0.5)
  loglikevec[i] <- llreg(Y, X, mu, sigma, stdv)
  muvec[i] <- mu
  sigvec[i] <- sigma
  sdvec[i] <- stdv
}

which.max(loglikevec)
Q<- c(muvec[which.max(loglikevec)], 
      sigvec[which.max(loglikevec)],
      sdvec[which.max(loglikevec)])

mu = Q[1]
sigma = Q[2]
ytrue=(exp(1)^-((log(X))^2/(2*(sigma^2))))/(X*sigma*sqrt(2*pi))
ytrue2=ytrue/sum(ytrue)
DS <- data.frame(ytrue2,X)

Plot <- ggplot() +
  geom_point(data=DF, aes(X, Y,), col = "darkgreen") +
  geom_point(data=DS, aes(X, ytrue2), col = "steelblue")
Plot+ labs(y="Probability", x="Weight of books (kg)")