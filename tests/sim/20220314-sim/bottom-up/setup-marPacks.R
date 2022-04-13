# 1. Detailed Tract
marPack1 <- NULL

# 2. Tract HH/GQ
marPack2 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = F,
                trct_agsx = F,
                cnty_dtld = F,
                cnty_hhgq = F,
                cnty_vhcr = F,
                cnty_agsx = F,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

# 3. Tract votingAge × hisp × cenRace
marPack3 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = F,
                cnty_dtld = F,
                cnty_hhgq = F,
                cnty_vhcr = F,
                cnty_agsx = F,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

# 4. Tract age × sex
marPack4 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = F,
                cnty_vhcr = F,
                cnty_agsx = F,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

# 5. County HH/GQ
marPack5 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = TRUE,
                cnty_vhcr = F,
                cnty_agsx = F,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

# 6. County votingAge × hisp × cenRace
marPack6 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = F,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

# 7. County age × sex
marPack7 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = TRUE,
                stat_dtld = F,
                stat_hhgq = F,
                stat_vhcr = F,
                stat_agsx = F)

# 8. State HH/GQ
marPack8 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = TRUE,
                stat_dtld = F,
                stat_hhgq = TRUE,
                stat_vhcr = F,
                stat_agsx = F)

# 9. State votingAge × hisp × cenRace
marPack9 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = TRUE,
                stat_dtld = F,
                stat_hhgq = TRUE,
                stat_vhcr = TRUE,
                stat_agsx = F)

# 10. State age × sex
marPack10 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = F,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = TRUE,
                stat_dtld = F,
                stat_hhgq = TRUE,
                stat_vhcr = TRUE,
                stat_agsx = TRUE)

# 11. Detailed County
marPack11 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = TRUE,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = TRUE,
                stat_dtld = F,
                stat_hhgq = TRUE,
                stat_vhcr = TRUE,
                stat_agsx = TRUE)

# 12. Detailed State
marPack12 <-
  setup_margins(trct_dtld = F,
                trct_hhgq = TRUE,
                trct_vhcr = TRUE,
                trct_agsx = TRUE,
                cnty_dtld = TRUE,
                cnty_hhgq = TRUE,
                cnty_vhcr = TRUE,
                cnty_agsx = TRUE,
                stat_dtld = TRUE,
                stat_hhgq = TRUE,
                stat_vhcr = TRUE,
                stat_agsx = TRUE)


# Save all marPacks
save(list =
       c("marPack1",
         "marPack2",
         "marPack3",
         "marPack4",
         "marPack5",
         "marPack6",
         "marPack7",
         "marPack8",
         "marPack9",
         "marPack10",
         "marPack11",
         "marPack12"),
     file = "marPacks-BU.RData")
