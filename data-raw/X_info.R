# setup marPack
marPack = list()
idx <- 1


# ----- Tract Detailed -------------------------------------------------------
tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")
nmar = 1881
X_info = tibble::tibble(
  region_id = c(rep(tract_nums_pad, each = 168), rep(NA, 1881)),
  region_type = c(rep("TRACT", 7224), rep(NA, nmar)),
  query = c(rep("detailed_or_total", 7224), rep(NA, nmar))
  # geomod = c(rep(geoMod[3], 7224), rep(NA, nmar)),
  # querymod = c(rep(queryMod[1], 7224), rep(NA, nmar))
)

# Add row index column for margins and for original X
X_info$row_idx <- 1:9105 # hardcoded for sanity check
X_info$mar_idx <- c(rep(NA, 7224), 1:1881)

# ---- County Totals-----------------------------------------------------------
for(j in 1:7){  # county
  marPack[[idx]] <- list(0, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
  X_info$region_id[7224 + idx]   = j
  X_info$region_type[7224 + idx] = "COUNTY"
  X_info$query[7224 + idx] <- "detailed_or_total"
  idx <- idx + 1
}

# ---- State Totals -----------------------------------------------------------
for(j in 1:2){ # state
  marPack[[idx]] <- list(0, 0, 0, 0, 0, stateRecode[j, 1]:stateRecode[j, 2])
  X_info$region_id[7224 + idx]   = j
  X_info$region_type[7224 + idx] = "STATE"
  X_info$query[7224 + idx] <- "detailed_or_total"
  idx <- idx + 1
}


# ----Tract HH/GQ -------------------------------------------------------------
for(i in 1:2){     # hh/gq
  for(j in 1:43){  # tract
    marPack[[idx]] <- list(i, 0, 0, 0, 0, j)
    X_info$region_id[7224 + idx]   = stringr::str_pad(j, 2, pad = "0")
    X_info$region_type[7224 + idx] = "TRACT"
    X_info$query[7224 + idx] <- "hhgq"
    idx <- idx + 1
  }
}

# ---- County HH/GQ -----------------------------------------------------------
for(i in 1:2){    # hh/gq
  for(j in 1:7){  # county
    marPack[[idx]] <- list(i, 0, 0, 0, 0, countyRecode[j, 1]:countyRecode[j, 2])
    X_info$region_id[7224 + idx]   = j
    X_info$region_type[7224 + idx] = "COUNTY"
    X_info$query[7224 + idx] <- "hhgq"
    idx <- idx + 1
  }
}

# ---- State HH/GQ ------------------------------------------------------------
for(i in 1:2){   # hhgq
  for(j in 1:2){ # state
    marPack[[idx]] <- list(i, 0, 0, 0, 0, stateRecode[j, 1]:stateRecode[j, 2])
    X_info$region_id[7224 + idx]   = j
    X_info$region_type[7224 + idx] = "STATE"
    X_info$query[7224 + idx] <- "hhgq"
    idx <- idx + 1
  }
}

# ---- Tract Age x Sex --------------------------------------------------------
for(i in 1:43){ # tract levels
  for(j in 1:3){ # age levels
    for(k in 1:2){ # sex levels
      marPack[[idx]] <- list(0, k, 0, j, 0, i)
      X_info$region_id[7224 + idx]   = stringr::str_pad(i, 2, pad = "0")
      X_info$region_type[7224 + idx] = "TRACT"
      X_info$query[7224 + idx] <- "age_sex"
      idx <- idx + 1
    }
  }
}

# ---- County Age x Sex -------------------------------------------------------
for(i in 1:7){ # county levels
  for(j in 1:3){ # age levels
    for(k in 1:2){ # sex levels
      marPack[[idx]] <- list(0, k, 0, j, 0,
                             countyRecode[i,1]:countyRecode[i, 2])
      X_info$region_id[7224 + idx]   = i
      X_info$region_type[7224 + idx] = "COUNTY"
      X_info$query[7224 + idx] <- "age_sex"
      idx <- idx + 1
    }
  }
}

# ---- State Age x Sex --------------------------------------------------------
for(i in 1:2){ # state levels
  for(j in 1:3){ # age levels
    for(k in 1:2){ # sex levels
      marPack[[idx]] <- list(0, k, 0, j, 0,
                             stateRecode[i,1]:stateRecode[i, 2])
      X_info$region_id[7224 + idx]   = i
      X_info$region_type[7224 + idx] = "STATE"
      X_info$query[7224 + idx] <- "age_sex"
      idx <- idx + 1
    }
  }
}

# ---- Tract votingAge x hisp x cenRace ---------------------------------------
votingAgeRecode <- rbind(c(1, 1),
                         c(2, 3))

for(i in 1:43){ # tract levels
  for(j in 1:2){ # voting age levels
    for(k in 1:2){ # hisp levels
      for(el in 1:7){ # cenRace levels
        marPack[[idx]] <- list(0, 0, el,
                               votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
                               k, i)
        X_info$region_id[7224 + idx]   = stringr::str_pad(i, 2, pad = "0")
        X_info$region_type[7224 + idx] = "TRACT"
        X_info$query[7224 + idx] <- "votingAge_hisp_cenrace"
        idx <- idx + 1
      }
    }
  }
}

# ---- County votingAge x hisp x cenRace --------------------------------------
for(i in 1:7){ # county levels
  for(j in 1:2){ # voting age levels
    for(k in 1:2){ # hisp levels
      for(el in 1:7){ # cenRace levels
        marPack[[idx]] <- list(0, 0, el,
                               votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
                               k,
                               countyRecode[i, 1]:countyRecode[i, 2])
        X_info$region_id[7224 + idx]   = i
        X_info$region_type[7224 + idx] = "COUNTY"
        X_info$query[7224 + idx] <- "votingAge_hisp_cenrace"
        idx <- idx + 1
      }
    }
  }
}

# ---- State votingAge x hisp x cenRace ---------------------------------------
for(i in 1:2){ # state levels
  for(j in 1:2){ # voting age levels
    for(k in 1:2){ # hisp levels
      for(el in 1:7){ # cenRace levels
        marPack[[idx]] <- list(0, 0, el,
                               votingAgeRecode[j, 1]:votingAgeRecode[j, 2],
                               k,
                               stateRecode[i, 1]:stateRecode[i, 2])
        X_info$region_id[7224 + idx]   = i
        X_info$region_type[7224 + idx] = "STATE"
        X_info$query[7224 + idx] <- "votingAge_hisp_cenrace"
        idx <- idx + 1
      }
    }
  }
}

# usethis::use_data(X_info)













