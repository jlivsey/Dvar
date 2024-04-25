

add_X_info_modifiers <- function(eps, geoMod, queryMod) {
  # Add names to geoMod and queryMod
  names(geoMod) = c("STATE", "COUNTY", "TRACT")
  names(queryMod) = c("detailed_or_total",
                      "hhgq",
                      "age_sex",
                      "votingAge_hisp_cenrace")
  epsMod <- outer(geoMod, queryMod)

  # Add modifier columns to X_info (default fill with NA)
  X_info <- X_info %>%
    mutate(geoMod = NA) %>%
    mutate(queryMod = NA) %>%
    mutate(epsMod = NA) %>%
    mutate(bpar = NA)

  # Loop over all geo-modifiers and query-modifiers and add to X_info
  for (g in names(geoMod)) {
    for (q in names(queryMod)) {
      rows <- X_info %>%
        filter(query == q) %>%
        filter(region_type == g) %>%
        pull(row_idx)
      X_info$queryMod[rows] <- queryMod[q]
      X_info$geoMod[rows] <- geoMod[g]
      X_info$epsMod[rows] <- epsMod[g, q]
      X_info$bpar[rows] <- 1 / (eps * epsMod[g, q])
    }
  }

  return(X_info)
}
