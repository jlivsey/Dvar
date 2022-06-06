#' ## Question:
#' When adding a new margin
#' 1. later in the sequence of workloads
#' 1. involving many interior cells
#' How many interior cells estimates change? or many margins change?
#'
#' Let's start this investigation with the last 2 margins that are added in the bottom up approach.
#' Namely, the state level detailed margins.
#' This means each margin contains roughly half of all interior cells.

#' First we set some parameters of the privacy budget and epsilon allocation.

eps = 10
geoMod = c(0.4,0.3,0.3)
queryMod = c(0.2,0.25,0.25,0.3)

