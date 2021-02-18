library(survey)
library(tidyverse)
library(reshape2)

# (i) Define a data-frame including variables with names (that are non-empty but can be meaningless like V1-V50) consisting of linearly independent columns, the first of which is a column of all 1's, whose weighted totals will be raked to totals supplied in a vector. The column of 1's can be called "(Intercept)" (but may not need a name); all the other nonredundant columns being used in the calibration MUST have names that I assume below are V1-V16.

# Sex      = 2 levels
# own/rent = 2 levels
# race     = 4 levels
# geo      = 7 levels

(n.full = 2*2*4*7)

(N = 20*n.full)

intercept = rep(1, n.full)


sex = factor(rep(0:1, n.full/2), labels = c("male", "female"))
own = factor(rep(0:1, n.full/2), labels = c("own" , "rent"))
rac = factor(rep(0:3, n.full/4))
geo = factor(rep(0:6, n.full/7))
sexgeo = interaction(sex, geo)

# Are all rows unique ?
df <- data.frame(sex, own, rac, geo, sexgeo)
df %>%
  group_by_all() %>%
  summarise(COUNT = n())

df <- melt(A)
v = df[1, -ncol(df)]
class(v)
class(v[1])
v[1] == 'male'
as.character(v[1])


n.col = (2-1)*(2-1)*(4-1)*(7-1)
infram = matrix(nrow = n.full, ncol = n.col)


A <- array(0, dim = c(2, 2, 4, 7))
dimnames(A)[[1]] <- c("male", "female")
dimnames(A)[[2]] <- c("own", "rent")
dimnames(A)[[3]] <- c(letters[1:4])
dimnames(A)[[4]] <- c(LETTERS[1:7])

A['male', ,,] <- 1
male_vec <- c(A)
A['male', ,,] <- 0

A[, 'own',,] <- 1
own_vec <- c(A)
A[, 'own',,] <- 0

A[, , "b",] <- 1
rac_b_vec <- c(A)
A[, ,'b',] <- 0

A[, , "c",] <- 1
rac_c_vec <- c(A)
A[, ,'c',] <- 0

A[, , "d",] <- 1
rac_d_vec <- c(A)
A[, ,'d',] <- 0

A[,,,"B"] <- 1
geo_B_vec <- c(A)
A[,,,"B"] <- 0

A[,,,"C"] <- 1
geo_C_vec <- c(A)
A[,,,"C"] <- 0

A[,,,"D"] <- 1
geo_D_vec <- c(A)
A[,,,"D"] <- 0

A[,,,"E"] <- 1
geo_E_vec <- c(A)
A[,,,"E"] <- 0

A[,,,"F"] <- 1
geo_F_vec <- c(A)
A[,,,"F"] <- 0

A[,,,"G"] <- 1
geo_G_vec <- c(A)
A[,,,"G"] <- 0


infram <- data.frame(Intercept = rep(1, n.full),
                     sex       = male_vec,
                     own       = own_vec,
                     rac1      = rac_b_vec,
                     rac2      = rac_c_vec,
                     rac3      = rac_d_vec,
                     geo1      = geo_B_vec,
                     geo2      = geo_C_vec,
                     geo3      = geo_D_vec,
                     geo4      = geo_E_vec,
                     geo5      = geo_F_vec,
                     geo6      = geo_G_vec  )

demoFram = infram

# Step (i) is the only tricky one, because one generally starts with categorical covariates in an input data-frame "infram" corresponding to the raking variables for which we have input target proportions. If that is so, then you want to make sure that the categorical variables are defined as factors, and that their "contrasts" are Treatment-type constrasts (which is generally the default, so not a problem). Then the dummy columns you would place into the data-frame "demoFram" corresponding to the categorical variable "Cvar1" in "infram" are:
#
# contrasts(Cvar1)[as.numeric(Cvar1),]

contrasts(sexgeo)[as.numeric(sexgeo),  ]
contrasts(geo)



# The simplest situation is one where the categorical variables in "infram" are not formed from any common constituent variables (as would happen e.g. if one was AgeGroup x Race and another Education x Race). If that happens then the "contrasts" code-step above when done on all categorical variables would result in linearly dependent columns in the matrix formed from the "demoFram" data-frame appearing below as an argument in the "svydesign" function, and that might cause problems so that you would have to remove redundant columns from the matrix and also corresponding entries from the "totvec" vector.
#
# Assume that the "infram" data-frame has 1 record for each desired cell of the contingency table, in the desired scanning order as the table entries viewed as a multi-way array.
#
# (ii) Apply the "svydesign" function in the R package "survey" to set up the calibration:
#
#   Syntax for (ii) [for a survey or table without structure related to weighting]:
#


Basewt = rep(N/n.full, n.full)

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
