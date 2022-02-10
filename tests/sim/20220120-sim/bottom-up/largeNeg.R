#' ## Investigating large negative estimates

#' We restrict ourselve to the 2nd replication of 1st workload
load("results2.RData")
load("bottomUp-forEric.RData")
coefEsts <- R[, 2, ]

#' Observed values are the truth plus the noise from the 2nd replicate.
yObs <- y_true + noise[, 2]

summary(coefEsts)

#' So the question is what is causing the -221 estimate.
#' Lets check if there is only one large negative value:

head(sort(coefEsts))    # Only one large negative value

#' Index of that large negative entry

minIdx <- which.min(coefEsts)
minIdx

#' What is the observed value for this index.
#' We conjecture that true value is 0.

data.frame(trueValue = c(A)[minIdx],
           obsValue  = yObs[minIdx],
           noiseValue = noise[minIdx, 2],
           estValue  = coefEsts[minIdx])

#' ***The true value is 109!!***
#' What could be pushing a true value with observed value 108 to be estimated
#' as -221 ????
