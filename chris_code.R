
#Goodness-of-fit tests
#This runs three GOF tests: Chi square, Freeman Tukey, and SSE. What your basically looking for is that your data doesn't fall near the extremes of the bootstrapped distribution. A good threshold for each statistic might be >.1 and <0.9. Probably good to run at least 100 simulations, more is preferable (nsim in the bottom code). These can be slow.

#m12 is your model fit - if this doesn't work, you made need to play around with the model output you are using and figure out how to extract the required elements below
fm <- m12

#Function for the GOF tests - if this doesn't work, check your model output and make sure observed, expected, and residuals are being properly extracted and amend those first three lines as needed. I haven't tried this with a GLM output, but it should be an easy fix if it doesn't work out of the box.
fitstats <- function(fm) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  resids <- residuals(fm)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}

#Bootstrapping the GOF - this is using parboot() from Unmarked. It may not play nicely with GLM output. If not, try the boot() function from the boot package in place of parboot(). I think the syntax is nearly identical except maybe the 'report' part which isn't important.
library(unmarked)
(pb <- parboot(fm, fitstats, nsim=100, report=1))

#Plot isn't necessary but might help you understand the outputs - may need some tweeking to work for boot()
plot(pb, main="")




#Next section

#Converting logit scale to natural scale. The link function for binomial is logit, so you need to backtransform to get from that scale to a probability of occurence (0-1). Use the following function. Be sure to add together any coefficients and intercepts first - all arithmetic should be done before backtransformation:

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#If you do anything that uses Poisson, that's on the log scale so you can just use exp() to convert to the natural scale.