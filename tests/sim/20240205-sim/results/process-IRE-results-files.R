# Each output file is a list of length 2
# R - Arrary dim c(7224, 3, nreps)
# X_info_upd


# ---- tract1 ----

Rout = array(NA, dim = c(7224, 4, 100))


filePath = function(letter_string){
  file.path("~/GitHub/Dvar/tests/sim/20240205-sim/results/",
            letter_string,
            "/out_eps10_unif_tract1.rds")
}


out_eps10_unif_tract1 <- readRDS(filePath("A"))
Rtemp = out_eps10_unif_tract1$R
Rout[, 1:3, 1:33] = Rtemp

out_eps10_unif_tract1 <- readRDS(filePath("B"))
Rtemp = out_eps10_unif_tract1$R
Rout[, 1:3, 1:33 + 33] = Rtemp

out_eps10_unif_tract1 <- readRDS(filePath("C"))
Rtemp = out_eps10_unif_tract1$R
Rout[, 1:3, 1:34 + 66] = Rtemp

out_eps10_unif_full <-
  readRDS("~/GitHub/Dvar/tests/sim/20240205-sim/results/A/out_eps10_unif_full.rds")
Rtemp = out_eps10_unif_full$R
Rout[, 4, 1:33] <- Rtemp

out_eps10_unif_full <-
  readRDS("~/GitHub/Dvar/tests/sim/20240205-sim/results/B/out_eps10_unif_full.rds")
Rtemp = out_eps10_unif_full$R
Rout[, 4, 1:33 + 33] <- Rtemp

out_eps10_unif_full <-
  readRDS("~/GitHub/Dvar/tests/sim/20240205-sim/results/C/out_eps10_unif_full.rds")
Rtemp = out_eps10_unif_full$R
Rout[, 4, 1:34 + 66] <- Rtemp


saveRDS(Rout, file = "~/GitHub/Dvar/tests/sim/20240205-sim/results/out_eps10_unif_tract1.rds")


# ---- County1 ----

Rout = array(NA, dim = c(7224, 4, 100))

filePath = function(letter_string){
  file.path("~/GitHub/Dvar/tests/sim/20240205-sim/results/",
            letter_string,
            "/out_eps10__unif_county1.rds")
}

x <- readRDS(filePath("A"))
Rtemp = x$R
Rout[, 1:3, 1:33] = Rtemp

x <- readRDS(filePath("B"))
Rtemp = x$R
Rout[, 1:3, 1:33 + 33] = Rtemp

x <- readRDS(filePath("C"))
Rtemp = x$R
Rout[, 1:3, 1:34 + 66] = Rtemp

out_eps10_unif_full <-
  readRDS("~/GitHub/Dvar/tests/sim/20240205-sim/results/A/out_eps10_unif_full.rds")
Rtemp = out_eps10_unif_full$R
Rout[, 4, 1:33] <- Rtemp

out_eps10_unif_full <-
  readRDS("~/GitHub/Dvar/tests/sim/20240205-sim/results/B/out_eps10_unif_full.rds")
Rtemp = out_eps10_unif_full$R
Rout[, 4, 1:33 + 33] <- Rtemp

out_eps10_unif_full <-
  readRDS("~/GitHub/Dvar/tests/sim/20240205-sim/results/C/out_eps10_unif_full.rds")
Rtemp = out_eps10_unif_full$R
Rout[, 4, 1:34 + 66] <- Rtemp

saveRDS(Rout, file = "~/GitHub/Dvar/tests/sim/20240205-sim/results/out_eps10_unif_county1.rds")
