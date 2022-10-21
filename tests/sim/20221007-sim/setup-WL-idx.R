# Setup sequence of workloads for sim study

# Sequence of workloads:
# 1. Detailed Tract
# 2. Tract HH/GQ
# 3. Tract votingAge × hisp × cenRace
# 4. Tract age × sex
# 5. County HH/GQ
# 6. County votingAge × hisp × cenRace
# 7. County age × sex
# 8. State HH/GQ
# 9. State votingAge × hisp × cenRace
# 10. State age × sex
# 11. Detailed County
# 12. Detailed State

# Load WLlist object. List of length 12 with WLlist[[i]] giving row index
#    of corresponding i(th) WL query.
load("WLlist.RData")

# ---- WL 1 ----
WL1idx <- rep(FALSE, 9105)
WL1idx[WLlist[['trct_dtld']]] <- TRUE

# ---- WL 2 ----
WL2idx <- WL1idx
WL2idx[WLlist[['trct_hhgq']]] <- TRUE

# ---- WL 3 ----
WL3idx <- WL2idx
WL3idx[WLlist[['trct_vhcr']]] <- TRUE

# ---- WL 4 ----
WL4idx <- WL3idx
WL4idx[WLlist[['trct_agsx']]] <- TRUE

# ---- WL 5 ----
WL5idx <- WL4idx
WL5idx[WLlist[['cnty_hhgq']]] <- TRUE

# ---- WL 6 ---
WL6idx <- WL5idx
WL6idx[WLlist[['cnty_vhcr']]] <- TRUE

# ---- WL 7 ---
WL7idx <- WL6idx
WL7idx[WLlist[['cnty_agsx']]] <- TRUE

# ---- WL 8 ---
WL8idx <- WL7idx
WL8idx[WLlist[['stat_hhgq']]] <- TRUE

# ---- WL 9 ---
WL9idx <- WL8idx
WL9idx[WLlist[['stat_vhcr']]] <- TRUE

# ---- WL 10 ---
WL10idx <- WL9idx
WL10idx[WLlist[['stat_agsx']]] <- TRUE

# ---- WL 11 ---
WL11idx <- WL10idx
WL11idx[WLlist[['cnty_dtld']]] <- TRUE

# ---- WL 12 ---
WL12idx <- WL11idx
WL12idx[WLlist[['stat_dtld']]] <- TRUE

WLlist_fullLen <- list(
  WL1idx,
  WL2idx,
  WL3idx,
  WL4idx,
  WL5idx,
  WL6idx,
  WL7idx,
  WL8idx,
  WL9idx,
  WL10idx,
  WL11idx,
  WL12idx
)

save(WLlist_fullLen, file = "WLlist_fullLen.RData")
