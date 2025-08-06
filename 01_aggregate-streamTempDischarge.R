# Aggregate HF stream temp & discharge
# Data 'ytd' are from the HF Data Archive for met: 
#     https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF001
# and for hydrology: 
#     https://harvardforest1.fas.harvard.edu/exist/apps/datasets/showData.html?id=HF070 
# Data 'new' at real-time web: https://harvardforest.fas.harvard.edu/data-archives/data-in-real-time/met-hydro-stations/

# Uses library(tidyverse) functions, but don't need whole library
library(magrittr)

min_year = 2024

# Hydro station 15-min data to daytime mean
hy_ytd = readr::read_csv("data/met-data/hf070-04-15min.csv")
hy_new = readr::read_csv("data/met-data/hqf.csv")
hydro15 = dplyr::bind_rows(hy_ytd,hy_new) %>%
  dplyr::mutate(hour = lubridate::hour(datetime),
                date = lubridate::date(datetime),
                year = lubridate::year(datetime)) %>%
  dplyr::filter(hour>=8 & hour<=16, year>=min_year)
  
# Met station 15-min data
met_ytd = readr::read_csv("data/met-data/hf001-10-15min-m.csv")
met_new = readr::read_csv("data/met-data/qfm.csv")
met15 = dplyr::bind_rows(met_ytd[1:(nrow(met_ytd)-1),],met_new) %>%
  dplyr::mutate(hour = lubridate::hour(datetime),
                date = lubridate::date(datetime),
                year = lubridate::year(datetime)) %>%
  dplyr::filter(hour>=8 & hour<=16, year>=min_year) 

# Combine hydrology and meteorology data
hydromet = dplyr::left_join(hydro15,met15)



