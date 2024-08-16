## 01-import.R
## Created: August 16, 2024
## Author: Lee Kennedy-Shaffer, PhD

require(tidyverse)

## Import FG data:
FG.dat <- NULL
for (bat in c("LHB","RHB","SHB")) {
  FG.dat <- FG.dat %>% 
    bind_rows(read_csv(file=paste0("data/fg-custom-",bat,"-2015-2024.csv"),
                       col_names=TRUE) %>%
                dplyr::mutate(Batter=bat))
}

## Import Savant data:
B.250 <- read_csv(file="data/savant-custom-250PA-2015-2024.csv",
                  col_names=TRUE) %>%
  bind_rows(read_csv(file="data/savant-custom-100PA-2020.csv",
                     col_names=TRUE)) %>%
  dplyr::rename(name=`last_name, first_name`)

Sav.Shifts <- NULL
for (year in 2016:2022) {
  Sav.Shifts <- Sav.Shifts %>%
    bind_rows(read_csv(file=paste0("data/savant-pos-",if_else(year==2020,"100","250"),
                                   "PA-",year,".csv"),
                       col_names=TRUE))
}

Sav.Shades <- NULL
for (year in 2023:2024) {
  Sav.Shades <- Sav.Shades %>%
    bind_rows(read_csv(file=paste0("data/savant-pos-",if_else(year==2020,"100","250"),
                                   "PA-",year,".csv"),
                       col_names=TRUE))
}

Sav.2016_22 <- B.250 %>% dplyr::filter(year %in% 2016:2022) %>%
  left_join(Sav.Shifts, by=c("year","name"))
