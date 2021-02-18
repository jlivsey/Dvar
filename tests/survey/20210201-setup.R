library(survey)
library(tidyverse)

# (i) Define a data-frame including variables with names (that are non-empty but can be meaningless like V1-V50) consisting of linearly independent columns, the first of which is a column of all 1's, whose weighted totals will be raked to totals supplied in a vector. The column of 1's can be called "(Intercept)" (but may not need a name); all the other nonredundant columns being used in the calibration MUST have names that I assume below are V1-V16.

A <- array(0, dim = c(2, 2, 3))
dimnames(A)[[1]] <- c("male", "female")
dimnames(A)[[2]] <- c("own", "rent")
dimnames(A)[[3]] <- c("young", "midage", "old")

A['male', ,] <- 1
male_vec <- c(A)
A['male', ,] <- 0

A['female', ,] <- 1
female_vec <- c(A)
A['female', ,] <- 0

A[, 'own',] <- 1
own_vec <- c(A)
A[, 'own',] <- 0

A[, 'rent',] <- 1
rent_vec <- c(A)
A[, 'rent',] <- 0

A[, , "young"] <- 1
young_vec <- c(A)
A[, ,'young'] <- 0

A[, , "midage"] <- 1
midage_vec <- c(A)
A[, ,'midage'] <- 0

A[, , "old"] <- 1
old_vec <- c(A)
A[, ,'old'] <- 0


infram <- data.frame(Intercept = as.factor(rep(1, 12)),
                     male      = as.factor(male_vec),
                     female    = as.factor(female_vec),
                     own       = as.factor(own_vec),
                     rent      = as.factor(rent_vec),
                     young     = as.factor(young_vec),
                     midage    = as.factor(midage_vec),
                     old       = as.factor(old_vec)     )

# infram <- data.frame(Intercept = rep(1, 12),
#                      male      = c(1, rep(0, 11)),
#                      female    = c(rep(0, 1), 1, rep(0, 10)),
#                      own       = c(rep(0, 2), 1, rep(0, 9)),
#                      rent      = c(rep(0, 3), 1, rep(0, 8)),
#                      young     = c(rep(0, 4), 1, rep(0, 7)),
#                      midage    = c(rep(0, 5), 1, rep(0, 6)),
#                      old       = c(rep(0, 6), 1, rep(0, 5))   )


# Step (i) is the only tricky one, because one generally starts with categorical covariates in an input data-frame "infram" corresponding to the raking variables for which we have input target proportions. If that is so, then you want to make sure that the categorical variables are defined as factors, and that their "contrasts" are Treatment-type constrasts (which is generally the default, so not a problem). Then the dummy columns you would place into the data-frame "demoFram" corresponding to the categorical variable "Cvar1" in "infram" are:
#
# contrasts(Cvar1)[as.numeric(Cvar1),]

contrasts(infram$male)[as.numeric(infram$male), ]

fac <- as.factor(sample(x = 1:7, size = 30, replace = TRUE))
contrasts(fac)
contrasts(fac)[as.numeric(fac), ]

# Livsey - this matches the setup I have for infram

demoFram <- infram %>%
  select(Intercept, male, own, young, midage) %>%
  mutate(male = as.numeric(male)-1) %>%
  mutate(own = as.numeric(own)-1) %>%
  mutate(young = as.numeric(young)-1) %>%
  mutate(midage = as.numeric(midage)-1)

# The simplest situation is one where the categorical variables in "infram" are not formed from any common constituent variables (as would happen e.g. if one was AgeGroup x Race and another Education x Race). If that happens then the "contrasts" code-step above when done on all categorical variables would result in linearly dependent columns in the matrix formed from the "demoFram" data-frame appearing below as an argument in the "svydesign" function, and that might cause problems so that you would have to remove redundant columns from the matrix and also corresponding entries from the "totvec" vector.
#
# Assume that the "infram" data-frame has 1 record for each desired cell of the contingency table, in the desired scanning order as the table entries viewed as a multi-way array.
#
# (ii) Apply the "svydesign" function in the R package "survey" to set up the calibration:
#
#   Syntax for (ii) [for a survey or table without structure related to weighting]:
#

N = 500
Basewt = rep(N/12, 12)

tmpdsgn = svydesign(~1, data=demoFram, weights=~Basewt)

#
# In our examples, the column "Basewt" should contain all entries equal to N/ncell , where ncell is the total number of cells in our contingency table, and N is the desired table total.
#
# (iii) Make sure that
#
# Zmat = data.matrix(demoFram[,c("(Intercept)", paste0("V",1:16)])
Zmat = data.matrix(demoFram)
Zmat

# Zmat = [intercept, nonref-male/female, ]
#
#                             are the columns for which you want to have  sum(final.wts*col) = totvec .  The vector "totvec" will generally start by containing totals for a national population, with the first entry equal to the national pop total. The dimension of "totvec" should match the column dimension of Zmat (including initial column of 1's).
#
# Now for a contingency table with total N (say the number 10,000) that we talked about, you should make sure to include a pair of code lines like
#
# totvec = totvec*(N/totvec[1])
# names(totvec) = dimnames(Zmat)[[2]]

N = 500
# totvec = sum(final.wts*col)
# totvec = totvec*(N/totvec[1])
totvec = N * c(1, .5, .25, .3, .5)
names(totvec) = dimnames(Zmat)[[2]]
names(totvec)[1] = "(Intercept)"
#
#     (iv) Apply the "calibrate" function in the R package "survey" to do the calibration:
#
# Syntax for (iii):
#
#   raked.obj = calibrate(tmpdsgn,  formula(paste("~", paste0( names(totvec)[-1], collapse="+"))), totvec, calfun="raking")

raked.obj <-
  calibrate(
    design = tmpdsgn,
    formula = formula(paste("~", paste0( names(totvec)[-1], collapse="+"))),
    population = totvec,
    calfun="raking"
  )
#
# Finally the component in the output list "raked.obj" that you want as a set of "final adjusted weights" to populate the table whose entries correspond to the rows of "infram" is:   1/raked.obj$prob

1/raked.obj$prob

# CHECK - Recover table totals by factor level
as.numeric(1/raked.obj$prob) %*% data.matrix(demoFram)
