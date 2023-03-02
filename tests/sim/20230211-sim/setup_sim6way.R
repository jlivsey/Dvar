
devtools::load_all()

# Path to current sim (relative to DVar path)
simDir = "tests/sim/20230211-sim/"

load(file.path(simDir, "RData","true-table.Rdata"))

setupMargins <-
  setup_margins(trct_dtld = TRUE,
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

marPack = setupMargins$marPack

geoidPack = setupMargins$geoidPack

eps = 10
geoMod = c(0.4,0.3,0.3)
queryMod = c(0.2,0.25,0.25,0.3)

names(geoMod) <- c("State", "County", "Tract")
names(queryMod) <- c("detailed_or_total", "hhgq", "votingAge_hisp_cenrace", "age_sex")
outer(queryMod, geoMod)

#' True table
intab <- A

# Don't use weights in L1fit
W = NULL

# Change l1pack to not print warning of non-unique solution
options(warn = -1)

# dimension of 'middle' of table. No margins
dim0 = dim(intab)
d <- prod(dim0) # total number of unknowns
# number of total covariate that go in model (including specified margins)
nmar = length(marPack)
ndat = nmar + d

# vectorize true values at indices that go into model (will add noise)
y_true <- rep(-99, ndat)
y_true[1:d] <- c(intab)

# Initalize Design matrix for model at Identity = will be using all scalar
#   elements of table for model.
#   Will rbind margins within function
X <- matrix(-99, ndat, d)
X[1:d, 1:d] <- diag(d)

# Initialize epsilon modifier
epsMod_init <- geoMod[3] * # tract
               queryMod[1] # detailed
epsMod <- rep(-99, ndat)
epsMod[1:d] <- epsMod_init

# If no weight matrix is passed initialize to all ones
# if(is.null(W)) W <- rep(1, d + length(marPack))

# Number of internal cells associated with each tract
d_tract_inner = prod(dim0[-6])

# Construct array indexing similar to ES `MargLabs` in
# 20230119-ImpliedMarginContasts.r
Atrue <- array(TRUE, dim = dim(A))
Aidx <- which(Atrue, arr.ind = TRUE)
MargLabs_detailed <- apply(Aidx, 1, paste0, collapse="|")
source(file.path(simDir, "setup-MargLabs.R")) # Get MargLabs object

# Setup X_info matrix. We know the first 7224 rows are all detailed tract.
#   These detailed tract are filled in and if we know the margin values already
#   they are filled in too, otherwise NA is used.
tract_nums_pad <-  stringr::str_pad(1:43, 2, pad = "0")
X_info = data.frame(
  region_id = c(rep(tract_nums_pad, each = d_tract_inner), geoidPack$REGION_ID),
  region_type = c(rep("TRACT", 7224), geoidPack$REGION_TYPE),
  query = c(rep("detailed", 7224), rep(NA, nmar)),
  geomod = c(rep(geoMod[3], 7224), rep(NA, nmar)),
  querymod = c(rep(queryMod[1], 7224), rep(NA, nmar)),
  MargLabs = c(MargLabs_detailed, MargLabs)
)

# Loop over marpack and add corresponding row to X and value to y
for (i in 1:length(marPack)) {
  if(i %% 100 == 0) print(i)
  # Check what geo and query are associated with i^th marginal
  # Need to do this now because expects 0 for totals
  g <- geo_of_mar(marPack[[i]])
  q <- query_of_mar(marPack[[i]])
  # extract i-th element from marPack and formulate as full vectors not 0.
  mar <- marginZero2vec(marPack[[i]], mydim = dim0)
  # Find next row for design matrix
  Zv <- c(arraySetOnes(dim0, mar))
  # Add to design matrix
  X[d + i,] <- Zv
  # Find next row for y
  newY <- sum(arrayEval(intab, mar))
  # Append newY to y_true vector
  y_true[d + i] <- newY
  # Append epsilon modifier to match with y_true and specified margin
  epsMod_new <- geoChar2geoMod(geoMod, g) *
    queryChar2queryMod(queryMod, q)
  epsMod[d + i] <- epsMod_new
  # Fill in info about this margin
  X_info$query[7224 + i] = q
  X_info$geomod[7224 + i] = geoChar2geoMod(geoMod, g)
  X_info$querymod[7224 + i] = queryChar2queryMod(queryMod, q)
}

# add index column to keep track of row numbers after any filtering later
X_info$row_idx = 1:nrow(X_info)

