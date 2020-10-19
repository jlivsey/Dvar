# Example of method A from Erics write-up
# Fully specified probability array
# Want to map to parameters of log-linear model

library(survey)

# ficticious setup: (sex, race, geo) categories
#                      2,    2,   2  total levels in each category

I <- c(3, 4, 5)

J <- list()
J[[1]] <- 1
J[[2]] <- 2
J[[3]] <- 3
J[[4]] <- c(1,2)
J[[5]] <- c(2, 3)
J[[6]] <- c(1, 2, 3)

next_loop_seq = function(input.seq, max.seq){
  N = length(input.seq)
  output.seq = input.seq
  if(length(max.seq)!=N) stop("length of input.seq and max.seq do not match")
  if(all(input.seq==max.seq)) stop("input.seq is the last sequence in the loop")
  for(idx in N:1){
    if(input.seq[idx] < max.seq[idx]){
      output.seq[idx] = output.seq[idx] + 1
      return(output.seq)
    }else{
      output.seq[idx] = 1
    }
  }
}

is_end <- function(input.seq, max.seq){
  all(input.seq==max.seq)
}


initialize_A <- function(J, I){
  n.J <- length(J)
  A <- vector(mode = 'list', length = n.J)

  # i <- 1

  for(i in 1:n.J){
    n_categories <- I[J[[i]]]
    A[[i]] <- array(dim = n_categories)
    M <- matrix(ncol = length(n_categories))
    newrow <- rep(1, length(n_categories))
    target <- n_categories - 1
    M[1, ] <- newrow
    while(!is_end(newrow, target)){
      newrow <- next_loop_seq(newrow, target)
      M <- rbind(M, newrow)
    }
    A[[i]][M] <- rnorm(nrow(M))
  }
  return(A)
}

A <- initialize_A(J, I)

fill_empty <- function(Aarray, I){

}


# Lets work on A[[6]] for now

Aa <- A[[6]]

full.dim <- dim(Aa)
# [1] 3 4 5

na.list <- which(is.na(Aa), arr.ind = TRUE)
# dim1 dim2 dim3
# [1,]    3    1    1
# [2,]    3    2    1
# [3,]    3    3    1

possible_to_fill <- function(candidate, na.list){

  candidate <- na.list[1, ]

  idx_at_end <- which(candidate == full.dim)
  next_value <- candidate[idx_at_end] - 1
  other_rows <- candidate
  other_rows[idx_at_end] <- next_value
  while(next_value > 1){
    next_value <- next_value - 1
    new_row <- candidate
    new_row[idx_at_end] <- next_value
    other_rows <- rbind(other_rows, new_row)
  }

  # CHECK IF all rows in other_rows are in na.list --> return(TRUE/FALSE)
  # Will need other_rows too
}


Aa[candidate] <- -sum(Aa[other_rows])












