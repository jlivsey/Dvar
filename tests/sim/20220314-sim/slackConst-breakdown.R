# Purpose: look at slack Constraints of L1 regression problem.
#          Specifically, the values of absoluate value constraints which are
#          exactly satisfied after estimation broken down by detailed cells and
#          marginal cells.

library(Dvar)
library(L1pack)
library(tidyverse)

#' True table
load("true-table.Rdata")

# load marPack object
load('marPacks-BU.RData')

# privacy budget parameters
# (make sure consistent with coefEst results)
bpar <- 12
eps = 10    # new code inputs eps not bpar
geoMod = c(0.4,0.3,0.3)
queryMod = c(0.2,0.25,0.25,0.3)

resultsFiles <-
  c(
    "results2.RData",
    "results3.RData",
    "results4.RData",
    "results5.RData",
    "results6.RData",
    "results7.RData",
    "results8.RData",
    "results9.RData",
    "results10.RData",
    "results11.RData",
    "results12.RData"
  )

n_res <- length(resultsFiles)
n_reps <- 20

margin_zero_nonSlack_prop <- array(NA, dim = c(n_reps, n_res, 2))
dimnames(margin_zero_nonSlack_prop)[[1]] <- paste0("rep", 1:n_reps)
dimnames(margin_zero_nonSlack_prop)[[2]] <- paste0("WL", 2:12)
dimnames(margin_zero_nonSlack_prop)[[3]] <- c("old", "new")

nMar_vec <- numeric(n_res)

for(r in seq_along(resultsFiles)){

  # Store the old X before loading the next results file
  if(r > 1){
    Xmar_old <- Xmar
    nMar_old <- nMar
    idx_zero_nonSlack_old <- idx_zero_nonSlack
    idx_nonZero_slack_old <- idx_nonZero_slack
  }

  # load coef estimates
  load(resultsFiles[r])

  for(idx_rep in 1:n_reps){

    print(paste("r = ", r, ", idx_rep = ", idx_rep))

    coefEsts2 = coefEsts[idx_rep, ]
    infNoise = noise[, idx_rep]
    y <- y_true + infNoise

    # Construct marinal totals using true counts
    nMar = length(marPack)
    nMar_vec[r] <- nMar
    Xmar = X[(nrow(X)-nMar+1):nrow(X), ] #  select last nMar rows of X

    # Calculate which rows of Xmar are new
    if(r > 1){
      idx_isOld <- duplicated_rows(Xmar_old, Xmar)
    }

    # S = data.frame(marNoise = infNoise[(nrow(X) - nMar + 1):nrow(X)])
    # M <- S %>%
    #   mutate(truMar = Xmar %*% c(A)) %>%
    #   mutate(sumInteriorDP = Xmar %*% y[1:7224]) %>%
    #   mutate(estMar = Xmar %*% coefEsts2) %>%
    #   mutate(marDP = truMar + marNoise) %>%
    #   mutate(startingErr = abs(sumInteriorDP - marDP)) %>%
    #   mutate(slackVal = round(estMar - marDP, 10))

    e = y - X %*% coefEsts2
    e2 = round(e, 6)
    # sum(e2 == 0)
    # sum(e2 != 0)
    idx_zero_nonSlack <- which(e2 == 0)
    # idx_nonZero_slack <- which(e2 != 0)

    e2mar <- e2[(7224+1):(7224+nMar)]
    e2mar_old <- e2mar[idx_isOld]
    e2mar_new <- e2mar[!idx_isOld]

    idx_zero_old <- e2mar_old == 0
    idx_zero_new <- e2mar_new == 0

    # How many changes (slack) were in detailed cells vs margins
    # sum(idx_nonZero_slack <= 7224)
    # sum(idx_nonZero_slack > 7224)

    # Proportion of marginal constraints non-slack (exactly satisfied)
    # sum(idx_nonZero_slack > 7224) / nMar
    # sum(idx_zero_nonSlack > 7224) / nMar

    if(r == 1){
      margin_zero_nonSlack_prop[idx_rep, r, "new"] <- mean(idx_zero_nonSlack)
    } else {
      margin_zero_nonSlack_prop[idx_rep, r, "old"] <- mean(idx_zero_old)
      margin_zero_nonSlack_prop[idx_rep, r, "new"] <- mean(idx_zero_new)
    }



  }
}

# save(margin_zero_nonSlack_prop, file = "margin_zero_nonSlack_prop.RData" )


margin_nonSlack <- rowSums(margin_nonSlack_prop) / 20
cbind(margin_nonSlack, nMar_vec)




# Convert to array indexing
e2Arr = array(e2, dim = dim(A))
idxArr_nonZero <- which(e2Arr != 0, arr.ind = TRUE)
B <-
  cbind(idxArr_nonZero,     # Array index of non-zero
        A[idxArr_nonZero],  # True cell count
        y[e2 != 0],         # True + noise
        coefEsts2[e2 != 0])  # estimated value
B = data.frame(B)
colnames(B) <- c("ownRent", "sex", "race", "age", "hisp", "tract", "true", "DPobs", "estim")
head(B, 20)


# Sum of all estimated changes in Owner/Tract 1 cells
own_tract1 = B %>%
  filter(tract == 1) %>%
  filter(ownRent == 1) %>%
  mutate(estimErr = estim - DPobs)

sum(own_tract1$estimErr) # This ALWAYS turns out to be 6.33


# Sum of all estimated changes in Owner/Tract 4 cells
own_tract4 = B %>%
  filter(tract == 4) %>%
  filter(ownRent == 1) %>%
  mutate(estimErr = estim - DPobs)

sum(own_tract4$estimErr) # This ALWAYS turns out to be 6.33

# Look at number of margins changed per tract
table(idxArr_nonZero[, 5])

# plot error as function of True + noise cell value
nonZeroE2 = e2[idx_nonZero]
plot(nonZeroE2[1:1324]~B[,8])
abline(h = 0 )

# Look at all detailed cells in tract 20
idxArr_nonZero[idxArr_nonZero[,6] == 20, ]


# compare noise from nonZero cells to Laplace density of noise infused
estNoise = B[, 8] - B[, 7]
infNoise2 = infNoise[1:7224][-idx_nonZero]

summary(abs(estNoise))
summary(abs(infNoise2))

boxplot(estNoise,
        infNoise2,
        M$startingErr[M$slackVal != 0],
        M$startingErr[M$slackVal == 0],
        names = c("interior-notZero", "interior-zero",
                  "margin-notZero", "margin-zero"))
