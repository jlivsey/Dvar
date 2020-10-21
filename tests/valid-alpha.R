# Example of method A from Erics write-up
# Fully specified probability array
# Want to map to parameters of log-linear model

library(survey)

# ficticious setup: (sex, race, geo) categories
#                      2,    2,   2  total levels in each category

I <- c(2, 3, 4, 5)

J <- list()
J[[1]] <- 1
J[[2]] <- 2
J[[3]] <- 3
J[[4]] <- c(1,2)
J[[5]] <- c(2, 3)
J[[6]] <- c(1, 2, 3, 4)

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
  A <- vector(mode = 'list', length = length(J))
  for(i in 1:length(J)){
    n_categories <- I[J[[i]]]
    A[[i]] <- array(dim = n_categories)
    M <- matrix(ncol = length(n_categories))
    newrow <- rep(1, length(n_categories))
    target <- n_categories - 1
    M[1, ] <- newrow
    while(!is_end(newrow, target)){
      newrow <- next_loop_seq(newrow, target)
      M <- rbind(M, newrow) # binding to size. Allocate memory before
    }
    A[[i]][M] <- rnorm(nrow(M))
  }
  return(A)
}


check_rows <- function(M, v){
  bool_all_rows <- rowSums(M == v[col(M)]) == ncol(M)
  return(sum(bool_all_rows) > 0)
}

check_rows2 <- function(M, v){
  apply(v, 1, function(x) check_rows(B, x))
}


fill_na <- function(Aa){

  full.dim <- dim(Aa)

  # list of all NA element of Aa
  na.list <- which(is.na(Aa), arr.ind = TRUE)
  print(nrow(na.list))

  # Overall flag to exit while loop when completed filling in Aa
  flag <- TRUE
  if(nrow(na.list) == 1) flag <- FALSE

  # Find element v that can be filled in
  idx <- 0
  na.flag <- TRUE
  while(na.flag){
    idx <- idx + 1
    v <- na.list[idx, ]
    # Update flag to FALSE if v is valid
    end.dim <- which(full.dim == v)[1]
    all.dim <- 1:length(full.dim)
    fix.dim <- all.dim[-end.dim]

    M <- matrix(ncol = length(v), nrow = v[end.dim] - 1)
    for(i in 1:nrow(M)){
      M[i, ][end.dim] <- i
      M[i, ][fix.dim] <- v[fix.dim]
    }

    # compare M to na.list
    na.flag <- any(check_rows2(na.list, M))
  }

  # update NA element of array
  V <- matrix(v, nrow = 1)
  Aa[V] <- -sum(Aa[M])

  return(list(flag = flag, Aa = Aa))
}

# -----
A <- initialize_A(J, I)

# Lets work on A[[6]] for now
Aa <- A[[6]]

na.list <- which(is.na(Aa), arr.ind = TRUE)


flag <- TRUE
while(flag){
  out <- fill_na(Aa)
  Aa <- out$Aa
  # na.list <- out$na.list
  flag <- out$flag
}
