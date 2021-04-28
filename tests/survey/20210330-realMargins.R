# Extract margins from PDB and POP data

# ---- POP --------------------------------------------------------------------

# Load POP data
# load("tests/ES/PopEst.rdata")
library(readr)
x <- read_csv("data/cc-est2019-alldata-51.csv")

# Load array format from ES
# Age (18) x County x Sex x Hisp x Race (7)
source('tests/ES/PopEsts.R')
dimnames(XtrArr)


# Sex margins
prop.table(apply(XtrArr, 3, sum))
prop.table(apply(XtrArr[,"Culpeper County", , ,], 2, sum))

# Hisp margins
dimnames(XtrArr[, "Culpeper County", , ,])
prop.table(apply(XtrArr[, "Culpeper County", , ,], 3, sum))

# Sex X Geo margins
dimnames(XtrArr)
prop.table(apply(XtrArr, 2:3, sum))




county_list <- c(
  "Culpeper County",
  "Halifax County",
  "Madison County",
  "Northumberland County",
  "Rockbridge County",
  "Harrisonburg city",
  "Hopewell city"
)

x2 <- x %>%
  subset(YEAR == 12) %>%
  subset(AGEGRP == 0) %>%
  subset(CTYNAME %in% county_list) %>%
  select(CTYNAME,TOT_POP, TOT_MALE, TOT_FEMALE) %>%
  print()


# ---- PDB --------------------------------------------------------------------
# Load PBD data
source('tests/PDB/tract-variables.R')
source('~/github/Dvar/tests/PDB/pdb2array.R')

# Hispanicity margin from PBD
# Need to be consistant with ACS_13_17 or CEN_2010 !!!
dat2 <- dat %>%
  select(Tot_Population_ACS_13_17, Hispanic_CEN_2010) %>%
  mutate(pop = as.numeric(Tot_Population_ACS_13_17)) %>%
  mutate(hisp = as.numeric(Hispanic_CEN_2010)) %>%
  select(pop, hisp)

sum(dat2$hisp) / sum(dat2$pop)

# Now Hispanicity x Sex from POP
x2 <- x %>%
  select(TOT_POP, TOT_MALE, TOT_FEMALE)

prop.table(x2)
