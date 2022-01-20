

# 1. Detailed Tract
marPack1 <-
setup_margins(trct_dtld = TRUE, 
              trct_hhgq = FALSE, 
              trct_vhcr = FALSE, 
              trct_agsx = FALSE, 
              cnty_dtld = FALSE, 
              cnty_hhgq = FALSE, 
              cnty_vhcr = FALSE, 
              cnty_agsx = FALSE, 
              stat_dtld = FALSE, 
              stat_hhgq = FALSE, 
              stat_vhcr = FALSE, 
              stat_agsx = FALSE)

# 2. Detailed State
marPack2 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = F, 
                cnty_hhgq = F, 
                cnty_vhcr = F, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = F, 
                stat_vhcr = F, 
                stat_agsx = F)

# 3. Detailed County
marPack3 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = F, 
                cnty_vhcr = F, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = F, 
                stat_vhcr = F, 
                stat_agsx = F)

# 4. State HH/GQ
marPack4 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = F, 
                cnty_vhcr = F, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = F, 
                stat_agsx = F)

# 5. State votingAge × hisp × cenRace
marPack5 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = F, 
                cnty_vhcr = F, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = F)

# 6. State age × sex
marPack6 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = F, 
                cnty_vhcr = F, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = TRUE)

# 7. County HH/GQ
marPack7 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = TRUE, 
                cnty_vhcr = F, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = TRUE)

# 8. County votingAge × hisp × cenRace
marPack8 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = TRUE, 
                cnty_vhcr = TRUE, 
                cnty_agsx = F, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = TRUE)

# 9. County age × sex
marPack9 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = F, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = TRUE, 
                cnty_vhcr = TRUE, 
                cnty_agsx = TRUE, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = TRUE)

# 10. Tract HH/GQ
marPack10 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = TRUE, 
                trct_vhcr = F, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = TRUE, 
                cnty_vhcr = TRUE, 
                cnty_agsx = TRUE, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = TRUE)

# 11. Tract votingAge × hisp × cenRace
marPack11 <-
  setup_margins(trct_dtld = F, 
                trct_hhgq = TRUE, 
                trct_vhcr = TRUE, 
                trct_agsx = F, 
                cnty_dtld = TRUE, 
                cnty_hhgq = TRUE, 
                cnty_vhcr = TRUE, 
                cnty_agsx = TRUE, 
                stat_dtld = TRUE, 
                stat_hhgq = TRUE, 
                stat_vhcr = TRUE, 
                stat_agsx = TRUE)

# 12. Tract age × sex
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
     file = "marPacks-TD.RData")





