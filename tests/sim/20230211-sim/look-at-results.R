load("~/github/Dvar/tests/sim/20230123-sim/results01.RData")
ls()

rowIDX
colIDX


# Things that change with each different subset
tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")

coefEsts_all <- matrix(nrow = 7224, ncol = 50)

totalTime = 0
for(j in seq_along(tract_nums_pad)){

  regionID = tract_nums_pad[j]

  saveFileName <- sprintf("results%s.RData", regionID)
  print(saveFileName)
  load(saveFileName)

  totalTime = totalTime + sum(repTime)

  coefEsts_all[colIDX, ] = coefEsts[1:168, ]

}

# Load results from full WL runs of older sim
load("~/github/Dvar/tests/sim/20221105-sim/results04.RData")
fullWLtime <- sum(repTime)
fullCoefEst = coefEsts

fullWLtime / totalTime

# Do coef ests match?
colSet = 1:50    # replications
rowSet = 1:7224  # coef ests
cbind(fullCoefEst[rowSet, colSet], coefEsts_all[rowSet, colSet])
coefDiff = fullCoefEst[rowSet, colSet] - coefEsts_all[rowSet, colSet]
sum(abs(coefDiff) > 10^(-20))






