library(jsonlite)
library(dplyr)

# PDB for State data
data_url2 = "https://api.census.gov/data/2020/pdb/statecounty?get=Males_CEN_2010,Females_CEN_2010,Tot_Population_CEN_2010&for=state:*&key=25488af6841e29e4b76b5e8fb877ea0d4ae2a017"

json_data <- fromJSON(data_url2)

pbd_dat <- data.frame(json_data[-1,])
colnames(pbd_dat) <- json_data[1,]

head(pbd_dat)


# ---- All tracts in Virginia ----
# have non-hispanic white alone but do not have hispanic white alone (subtract?)

data_url2 = "https://api.census.gov/data/2019/pdb/tract?get=State_name,Tot_Population_ACS_13_17,NH_White_alone_CEN_2010,NH_Blk_alone_CEN_2010,NH_Asian_alone_CEN_2010,NH_NHOPI_alone_CEN_2010,NH_SOR_alone_CEN_2010,Hispanic_CEN_2010,County_name&for=tract:*&in=state:51&in=county:*&key=25488af6841e29e4b76b5e8fb877ea0d4ae2a017"

json_data <- fromJSON(data_url2)

pbd_dat <- data.frame(json_data[-1,])
colnames(pbd_dat) <- json_data[1,]

head(pbd_dat)

# Filter by Counties Eric selected
county_list <- c(
  "Culpeper County",
  "Halifax County",
  "Madison County",
  "Northumberland County",
  "Rockbridge County",
  "Harrisonburg city",
  "Hopewell city"
)

dat <- pbd_dat %>%
  filter(County_name %in% county_list)


# ---- Quick look at estimates ----
library(tigris)
library(sf)

dat <- pbd_dat %>%
  filter(County_name %in% "Culpeper County") %>%
  select(NH_Asian_alone_CEN_2010, County_name, state, county, tract) %>%
  tidyr::unite(GEOID, state,county,tract, sep="", remove=TRUE) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(NH_Asian_alone_CEN_2010 = as.numeric(as.character(NH_Asian_alone_CEN_2010))) %>%
  mutate(NAME = as.character(County_name)) %>%
#  mutate(NAME = substring(NAME, 1, nchar(NAME)-10)) %>%
  mutate(NAME = gsub(NAME, pattern = " County", replacement = ""))
head(dat)

# read in Virginia counties shape file
va = tracts(state = "Virginia", county = "Culpeper", year = 2016) %>%
  st_as_sf() %>%
  st_transform(crs = 3857)

#' # Join data and Shape data
va_dat <- dat %>%
  inner_join(va, by = c('GEOID' = 'GEOID'))


#' Quick plots of the merged data.
library(ggplot2)
g1 <- ggplot(va_dat) +
  geom_sf(colour = "black", size = 0.05, aes(fill = NH_Asian_alone_CEN_2010)) +
  scale_fill_distiller("HHI", palette = "RdYlBu") +
  ggtitle("NH_Asian_alone_CEN_2010", subtitle = "Culpeper County") +
  theme_bw()
print(g1)
















