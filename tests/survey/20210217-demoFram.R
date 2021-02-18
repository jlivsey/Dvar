
dfA <- melt(A)

demoFram = matrix(nrow = nrow(dfA), ncol = 24)

for(i in 1:nrow(dfA)){

  v = dfA[i, -ncol(dfA)]
  v = as.numeric(v)

  # Need to use value of sex[v[1]] and geo[v[4]] to put together the correct
  #    row of the sexgeo contrast form needed
  sexgeoChar = paste(sex[v[1]], geo[v[4]], sep = ".")

  demoFram[i, ] <- c(
                      contrasts(sex)[v[1], ],
                      contrasts(own)[v[2], ],
                      contrasts(rac)[v[3], ],
                      contrasts(geo)[v[4], ],
                      contrasts(sexgeo)[sexgeoChar, ]
                    )

}


# Are all rows unique?
temp <-  data.frame(demoFram) %>%
         group_by_all() %>%
         summarise(COUNT = n())
dim(temp)


eigen(demoFram)
