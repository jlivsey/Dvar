library(survey)
library(tidyverse)
library(reshape2)

source("tests/survey/20210309-fullDemoFram.R")

# N = size of population
# n = # cells in full cross
Basewt = rep(N/n, n)

# The svydesign() function wants a data.frame
demoFramReduced = as.data.frame(demoFramReduced)

tmpdsgn = svydesign(~1, data=demoFramReduced, weights=~Basewt)

#
# In our examples, the column "Basewt" should contain all entries equal to N/ncell , where ncell is the total number of cells in our contingency table, and N is the desired table total.
#
# (iii) Make sure that
#
# Zmat = data.matrix(demoFram[,c("(Intercept)", paste0("V",1:16)])
Zmat = data.matrix(demoFramReduced)
Zmat

# Zmat = [intercept, nonref-male/female, ]
#
#                             are the columns for which you want to have  sum(final.wts*col) = totvec .  The vector "totvec" will generally start by containing totals for a national population, with the first entry equal to the national pop total. The dimension of "totvec" should match the column dimension of Zmat (including initial column of 1's).
#
# Now for a contingency table with total N (say the number 10,000) that we talked about, you should make sure to include a pair of code lines like
#
# totvec = totvec*(N/totvec[1])
# names(totvec) = dimnames(Zmat)[[2]]

# N = 500
# totvec = sum(final.wts*col)
# totvec = totvec*(N/totvec[1])



totvec <-
  N *
  c(
     1,            # intercept
    .5,           # own
    .5,            # sex
    rep(.15, 6), # race
    .3, .3, # age
    .5,   # hisp
    rep(.0475, 19), #geo
    rep(.025, 39)   # race x geo
  )

# Sanity check
length(totvec) == ncol(demoFram)

# reduce totvec for linearly dependent columns in DemoFram
# Check that my colIdxSet has same length as QR-rank
totvecReduced = totvec[colIdxSet]


names(totvecReduced) = dimnames(Zmat)[[2]]
names(totvecReduced)[1] = "(Intercept)"
#
#     (iv) Apply the "calibrate" function in the R package "survey" to do the calibration:
#
# Syntax for (iii):
#
#   raked.obj = calibrate(tmpdsgn,  formula(paste("~", paste0( names(totvec)[-1], collapse="+"))), totvec, calfun="raking")

raked.obj <-
  calibrate(
    design = tmpdsgn,
    formula = formula(paste("~", paste0( names(totvecReduced)[-1], collapse="+"))),
    population = totvecReduced,
    calfun="raking"
  )
#
# Finally the component in the output list "raked.obj" that you want as a set of "final adjusted weights" to populate the table whose entries correspond to the rows of "infram" is:   1/raked.obj$prob

1/raked.obj$prob

# CHECK - Recover table totals by factor level
as.numeric(1/raked.obj$prob) %*% data.matrix(demoFramReduced)
