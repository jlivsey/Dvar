# devtools::load_all()

L <- vector(mode = "list", length = 8)

for(i in 1:4){
  L[[i]] <- vector(mode = "list", length = 43)
}

for(i in 5:8){
  L[[i]] <- vector(mode = "list", length = 7)
}

# WL 1 - Track, detailed
for(i in 1:43){
  L[[1]][[i]] <-  (168 * (i - 1)) + 1:168
}

tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")

# WL 2 - Tract, HH/GQ
for(i in 1:43){
  L[[2]][[i]] <-
    which(X_info$region_type == "TRACT" &
            X_info$query == "hhgq" &
            X_info$region_id == tract_nums_pad[i])
}

# WL 3 - Tract, Age x sex
for(i in 1:43){
  L[[3]][[i]] <-
    which(X_info$region_type == "TRACT" &
            X_info$query == "age_sex" &
            X_info$region_id == tract_nums_pad[i])
}

# WL 4 - Tract, votingAge_hisp_cenrace
for(i in 1:43){
  L[[4]][[i]] <-
    which(X_info$region_type == "TRACT" &
            X_info$query == "votingAge_hisp_cenrace" &
            X_info$region_id == tract_nums_pad[i])
}

# WL 5 - County, HH/GQ
for(i in 1:7){
  L[[5]][[i]] <-
    which(X_info$region_type == "COUNTY" &
            X_info$query == "hhgq" &
            X_info$region_id == i)
}

# WL 6 - County, Age x sex
for(i in 1:7){
  L[[6]][[i]] <-
    which(X_info$region_type == "COUNTY" &
            X_info$query == "age_sex" &
            X_info$region_id == i)
}

# WL 7 - county, votingAge_hisp_cenrace
for(i in 1:7){
  L[[7]][[i]] <-
    which(X_info$region_type == "COUNTY" &
            X_info$query == "votingAge_hisp_cenrace" &
            X_info$region_id == i)
}

# WL 8 - county, detailed_or_total
for(i in 1:7){
  L[[8]][[i]] <-
    which(X_info$region_type == "COUNTY" &
            X_info$query == "detailed_or_total" &
            X_info$region_id == i)
}
