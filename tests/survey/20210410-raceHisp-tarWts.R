# Convert PDB data into array
# similar to tests/ES/PopEst.R
library(tidyverse)

county_list <-
  c("Culpeper County",
    "Halifax County",
    "Harrisonburg city",
    "Hopewell city",
    "Madison County",
    "Northumberland County",
    "Rockbridge County"
  )

x_orig <- read.csv("data/pdb2020trv2_us.csv")

x <- x_orig %>%
  filter(State_name == "Virginia") %>%
  filter(County_name %in% county_list)

raceHisp_x_geo <- x %>%
  # filter(County_name == "Culpeper County") %>%
  filter(County_name %in% county_list) %>%
  mutate(geoid = paste0(County, Tract), .before = "County") %>%
  select(geoid,
         total = Tot_Population_CEN_2010,
         hisp = Hispanic_CEN_2010,
         nhwa = NH_White_alone_CEN_2010,
         nhba = NH_Blk_alone_CEN_2010,
         nhaa = NH_Asian_alone_CEN_2010,
         nhia = NH_AIAN_alone_CEN_2010,
         nhpa = NH_NHOPI_alone_CEN_2010,
         nhoa = NH_SOR_alone_CEN_2010
  ) %>%
  mutate(nh2p = total - hisp - nhwa - nhba - nhaa - nhia - nhpa - nhoa) %>%
  select(-total) %>%
  filter_at(vars(-geoid), any_vars(. != 0)) # remove any rows of all zeros
