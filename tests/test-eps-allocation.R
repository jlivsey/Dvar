# first check character is returned correct
mar <- list(0, 1, 0, 1, 0, 1:3)
geo_of_mar(mar)
g <- geo_of_mar(mar)
query_of_mar(mar)
q <- query_of_mar(mar)

# set modifier levels
geoMod <- c(.5, .25, .25)
queryMod <- c(.1, .2, .5, .2)

# check character is converted to modifier
geoChar2geoMod(geoMod = geoMod, geo = g)
queryChar2queryMod(queryMod = queryMod, query = q)
