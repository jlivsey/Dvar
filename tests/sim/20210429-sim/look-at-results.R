# Pick a results file to load
simDir <- "tests/sim/20210429-sim/"
load(file.path(simDir, "20210504-results-allMargins.RData")) # Full workload
coefFull = OUT
load(file.path(simDir, "20210505-results-noMargins.RData"))  # empty workload
coefEmty = OUT

# ---- True Data ----
load(file.path(simDir, "true-table.Rdata")) # Loads array A
Av <- c(A)

# ---- Format ----
M_orig <- data.frame(true = Av, all = coefFull, none = coefEmty)

M <- M_orig %>%
  mutate(allErr = true - all) %>%
  mutate(noneErr= true - none)

# ---- Results ----
summary(M)

boxplot(M[, 1:3])
boxplot(M[, 4:5])


