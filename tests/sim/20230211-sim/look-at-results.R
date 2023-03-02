# Top level of simulation directory
simDir <- "~/github/Dvar/tests/sim/20230211-sim"

# Load all files needed later
load(file.path(simDir, "RData", "X.RData"))
load(file.path(simDir, "RData", "noise.RData"))
load(file.path(simDir, "RData", "true-table.RData"))
load(file.path(simDir, "RData", "epsMod.RData"))

# List all results files in the results/ directory
resultsFiles <- file.path(simDir, "results") |>
                  list.files()

# grep all results files into workloads
WL2 <- grep(pattern = "results[0-9]{2}_hhgq[.]RData", x = resultsFiles, value = TRUE)
WL3 <- grep(pattern = "results[0-9]{2}_hhgq_votingAge_hisp_cenrace[.]RData", x = resultsFiles, value = TRUE)
WL4 <- grep(pattern = "results[0-9]{2}_hhgq_votingAge_hisp_cenrace_age_sex[.]RData", x = resultsFiles, value = TRUE)
WL5 <- grep(pattern = "_[0-9]{1}_hhgq[.]RData$", x = resultsFiles, value = TRUE)
WL6 <- grep(pattern = "_[0-9]{1}_hhgq_votingAge_hisp_cenrace[.]RData$", x = resultsFiles, value = TRUE)
WL7 <- grep(pattern = "_[0-9]{1}_hhgq_votingAge_hisp_cenrace_age_sex[.]RData$", x = resultsFiles, value = TRUE)
WL_list = list(WL2, WL3, WL4, WL5, WL6, WL7)

# Initialize storage for output
R = array(NA, dim = c(7224, 7, 50))
timeWL <- rep(NA, 6)

# Fill workload 1 - only detailed. BLUE = released DP counts
R[, 1, ] <- c(A) + noise[1:7224, 1:50]

# Fill in rest of R matrix.
for(i in seq_along(WL_list)){

  WL <- WL_list[[i]]
  timeWL[i] <- 0

  for(j in seq_along(WL)){

      ff <- WL[j]
      load(file.path(simDir, "results", ff))

      R[colIDX, i+1, ]  <- coefEsts[, 1:50]

      timeWL[i] = timeWL[i] + sum(repTime[1:50])

  }
}

# Load results from full WL runs of older sim
load("~/github/Dvar/tests/sim/20221105-sim/results05.RData")
fullCoefEst = coefEsts # 7224 x 50

# Do coef ests match?
coefDiff = fullCoefEst - R[, 5, 1:50]
sum(abs(coefDiff) > 10^(-20))




# Run launch.R with j = 44 and w = 1 to get 1344 coefs for tracts 1-8
#   in workload 5
#   Compare these coefficient estimates with full run from 20221105
save_coefEsts = coefEsts

cbind(
  decoupled = round(save_coefEsts, 3),
  full      = round(fullCoefEst[colIDX, 1], 3),
  diff      = round(save_coefEsts - fullCoefEst[colIDX, 1], 3)
) |> View()






