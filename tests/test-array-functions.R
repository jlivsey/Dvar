#' dimension of full array
mydim <- c(6, 2, 10)

#' structure of single element of marPack
mar <- list(1:3, 0, 0)

#' converter function if NULL is used for full dimension vector
marginZero2vec(mar, mydim = mydim)

#' Simulate data
A <- array(sample(1:10, size = prod(mydim), replace = TRUE), mydim)
Av <- c(A)

#' margins to look at
marPack <- list(list(NULL, 2, NULL),
                list(2, 2, NULL),
                list(4, NULL, NULL))

# lets look at just the first mar from marPack
mar <- marginNull2(marPack[[1]], mydim = mydim)
Zv <- c(arraySetOnes(mydim, mar))


# Tests
sum(A[,2,])
sum(arrayEval(A, mar))
Av %*% Zv
Av
Zv


# second mar from marPack
mar <- marginNull2(marPack[[2]], mydim = mydim)
Zv <- c(arraySetOnes(mydim, mar))


# Tests
sum(arrayEval(A, mar))
Av %*% Zv
Av
Zv

# third mar from marPack
mar <- marginNull2(marPack[[3]], mydim = mydim)
Zv <- c(arraySetOnes(mydim, mar))


# Tests
sum(arrayEval(A, mar))
Av %*% Zv
Av
Zv
