library(survey)
library(tidyverse)
library(reshape2)
library(tictoc)

source("tests/survey/20210309-fullDemoFram.R")
source('tests/survey/20210410-raceHisp-tarWts.R')

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

raceHisp_geo_tarWts <- raceHisp_x_geo %>%
  select(-geoid) %>%
  prop.table() %>%
  data.matrix()
  # as.numeric() %>%
  # round(6)

geo_tarWts <- raceHisp_geo_tarWts %>%
  # select(-geoid) %>%
  # data.matrix() %>%
  rowSums()

hisp_tarWts <- raceHisp_geo_tarWts %>%
  # select(-geoid) %>%
  # data.matrix() %>%
  colSums()

rac_tarWts <- hisp_tarWts[-1] %>%
  prop.table()

# weight and location for hisp entrys of raceHisp x geo
hisp_wt <- (1/2) * (1/43)
hisp_loc <- grepl("hisp", colnames(contrasts(rhg)))

# weight for Non-hisp entrys of raceHisp x geo
nonhisp_wt <- (1/14) * (1/43)
nonhisp_loc <- !hisp_loc

# Create wt vector for raceHisp x geo
raceHisp_geo_equalWts <- rep(hisp_wt, ncol(contrasts(rhg)))
raceHisp_geo_equalWts[nonhisp_loc] <- nonhisp_wt


# ---- Equal Wts ----
totvec <-
  N *
  c(
    1,              # intercept
    .5,             # own
    .5,             # sex
    rep(1/7, 6),    # race
    1/3, 1/3,       # age
    1/2,            # hisp
    rep(1/43, 42),  # geo
    #rep(1/6, 5),    # age x sex
    #rep(1/86, 85),  # sex x geo
    #rep(1/28, 27),   # own x race x hispanicty
    #rep(1/903, 902) # race x age x geo
    # rep(1/14, 13)    # race x hisp
    #rep(1/14, 7) # collapsed race x hisp into (hisp, ...)
    raceHisp_geo_equalWts    # raceHisp x geo
  )

# ---- Realistic Wts ----
totvec <-
  N *
  c(
     1,             # intercept
    .5,             # own
    .5,             # sex
    rac_tarWts[-1], # race
    1/3, 1/3,       # age
    1 - hisp_tarWts[1], # hisp
    geo_tarWts[-1],  # geo
    t(raceHisp_geo_tarWts)[-1]   # raceHisp x geo
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

tic()

raked.obj <-
  calibrate(
    design = tmpdsgn,
    formula = formula(paste("~", paste0( names(totvecReduced)[-1], collapse="+"))),
    population = totvecReduced,
    calfun="raking"
  )

toc()

#
# Finally the component in the output list "raked.obj" that you want as a set of "final adjusted weights" to populate the table whose entries correspond to the rows of "infram" is:   1/raked.obj$prob

1/raked.obj$prob
pt = prop.table(1 / raked.obj$prob)

# CHECK - Recover table totals by factor level
tabTot <- as.numeric(1/raked.obj$prob) %*% data.matrix(demoFramReduced)
dim(tabTot)


# Create synthetic data.frame
xv = rmultinom(n = 1, size = N, prob = pt)

(n  = prod(dim(A)))
(N  = 20*n)
xA = array(xv, dim = c(2, 2, 7, 3, 2, n_tracts))
dimnames(xA)[[1]] <- c("own", "rnt")
dimnames(xA)[[2]] <- c("mal", "fem")
dimnames(xA)[[3]] <- c("wh", "bl", "as", "aian", "pac", "oth", "twp")
dimnames(xA)[[4]] <- c("0-17", "18-62", "62p")
dimnames(xA)[[5]] <- c("hisp", "nonhisp")
dimnames(xA)[[6]] <- paste0(rep("tr", n_tracts), 1:n_tracts)

# Save synthetic population
# A <- xA
# save(A, file = "tests/sim/20210429-sim/true-table.Rdata")





