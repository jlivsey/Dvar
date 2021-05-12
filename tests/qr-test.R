library(tictoc)


#' ## Intro
#' We want to test the following conjecture on demoFram:
#' If M is the nxp matrix that we want to check for column removal
#' (with p=397 in our case), then doing a lm fit and checking for "missing"
#' coefficients seems to do the trick:
#' `which(  is.na(  lm( runif(nrow(M)) ~ M - 1)  )  )`
#'
#' ## Load Matrix
#'
#' We start with a matrix the is not full

load("demoFram.Rdata")
M = demoFram
dim(M)
qr(M)$rank

#' We see M rank 354 and 397 rows. So the question is which 43 rows need to be
#' removed to get a full rank matrix?
#'
#' ## Brute-force results
#'
#' By looping over ever sub-matrix one at a time and checking if all columns
#' are linearly independent (code not shown here)
#' we arrive at the following set of columns. The
#' issue is this is very computationally expensive. It took **106.481 seconds**
#' to run.

# Set of linearly independent columns
colIdxSet <-
  c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
    15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L,
    28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L,
    41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L,
    54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L,
    67L, 68L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 78L, 79L, 80L, 81L,
    82L, 83L, 84L, 86L, 87L, 88L, 89L, 90L, 91L, 92L, 94L, 95L, 96L,
    97L, 98L, 99L, 100L, 102L, 103L, 104L, 105L, 106L, 107L, 108L,
    110L, 111L, 112L, 113L, 114L, 115L, 116L, 118L, 119L, 120L, 121L,
    122L, 123L, 124L, 126L, 127L, 128L, 129L, 130L, 131L, 132L, 134L,
    135L, 136L, 137L, 138L, 139L, 140L, 142L, 143L, 144L, 145L, 146L,
    147L, 148L, 150L, 151L, 152L, 153L, 154L, 155L, 156L, 158L, 159L,
    160L, 161L, 162L, 163L, 164L, 166L, 167L, 168L, 169L, 170L, 171L,
    172L, 174L, 175L, 176L, 177L, 178L, 179L, 180L, 182L, 183L, 184L,
    185L, 186L, 187L, 188L, 190L, 191L, 192L, 193L, 194L, 195L, 196L,
    198L, 199L, 200L, 201L, 202L, 203L, 204L, 206L, 207L, 208L, 209L,
    210L, 211L, 212L, 214L, 215L, 216L, 217L, 218L, 219L, 220L, 222L,
    223L, 224L, 225L, 226L, 227L, 228L, 230L, 231L, 232L, 233L, 234L,
    235L, 236L, 238L, 239L, 240L, 241L, 242L, 243L, 244L, 246L, 247L,
    248L, 249L, 250L, 251L, 252L, 254L, 255L, 256L, 257L, 258L, 259L,
    260L, 262L, 263L, 264L, 265L, 266L, 267L, 268L, 270L, 271L, 272L,
    273L, 274L, 275L, 276L, 278L, 279L, 280L, 281L, 282L, 283L, 284L,
    286L, 287L, 288L, 289L, 290L, 291L, 292L, 294L, 295L, 296L, 297L,
    298L, 299L, 300L, 302L, 303L, 304L, 305L, 306L, 307L, 308L, 310L,
    311L, 312L, 313L, 314L, 315L, 316L, 318L, 319L, 320L, 321L, 322L,
    323L, 324L, 326L, 327L, 328L, 329L, 330L, 331L, 332L, 334L, 335L,
    336L, 337L, 338L, 339L, 340L, 342L, 343L, 344L, 345L, 346L, 347L,
    348L, 350L, 351L, 352L, 353L, 354L, 355L, 356L, 358L, 359L, 360L,
    361L, 362L, 363L, 364L, 366L, 367L, 368L, 369L, 370L, 371L, 372L,
    374L, 375L, 376L, 377L, 378L, 379L, 380L, 382L, 383L, 384L, 385L,
    386L, 387L, 388L, 391L, 392L, 393L, 394L, 395L, 396L)

# Removed columns
rmIdxSet <- (1:397)[-colIdxSet]

#' ## Erics lm() idea

tic()
fit <- lm( runif(nrow(M)) ~ M - 1)
rmIdxSet_lm <- which(is.na(fit$coefficients))
toc()

#' ## Are results the same?
names(rmIdxSet_lm) = NULL
cb = cbind(rmIdxSet, rmIdxSet_lm)
head(cb)
tail(cb)
all.equal(rmIdxSet, rmIdxSet_lm)

#' Gives the same result and about **100 times faster!**




