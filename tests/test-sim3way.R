mydim = c(6, 2, 10)

res <- 
Sim3Way(
Nrep = 100,
intab = array(sample(1:10, size = prod(mydim), replace = TRUE), mydim),
bpar = 1/3,
marPack = list(list(NULL, 2, NULL), 
               list(2, 2, NULL), 
               list(4, NULL, NULL),
               list(NULL, NULL, NULL))
)

# Construct marPack
eg <- expand.grid(0:6, 0:2, 0:10)
idx <- -9
for(i in 1:dim(eg)[1]){
  if(all( eg[i, ] != 0 )) idx <- c(idx, i)
}
idx <- idx[-1]

M <- eg[-idx, ]


Mlist <- list()
