## 01-import.R
## Created: August 16, 2024
## Author: Lee Kennedy-Shaffer, PhD

require(tidyverse)

## Import FG data:

### Full data by handedness:
FG.dat <- NULL
for (bat in c("LHB","RHB","SHB")) {
  FG.dat <- FG.dat %>% 
    bind_rows(read_csv(file=paste0("data/fg-custom-",bat,"-2015-2024.csv"),
                       col_names=TRUE) %>%
                dplyr::mutate(Batter=bat))
}

### Bases empty data by handedness:
FG.dat.empty <- NULL
for (bat in c("LHB","RHB")) {
  FG.dat.empty <- FG.dat.empty %>%
    bind_rows(left_join(read_csv(file=paste0("data/fg-custom-",bat,"-empty-2015-2024.csv"),
                                 col_names=TRUE),
                        read_csv(file=paste0("data/fg-custom-",bat,"-empty-adv-2015-2024.csv"),
                                 col_names=TRUE),
                        by=c("Season","League","PA","AVG")) %>%
                dplyr::mutate(Batter=bat))
}

### Fix column names:
FG.dat <- FG.dat %>%
  dplyr::rename(`BB percent`=`BB%`,
                `K percent`=`K%`,
                `Pull percent`=`Pull%`,
                `Cent percent`=`Cent%`,
                `Oppo percent`=`Oppo%`)
FG.dat.empty <- FG.dat.empty %>%
  dplyr::rename(`BB percent`=`BB%`,
                `K percent`=`K%`,
                `BB to K`=`BB/K`,
                `wRC plus`=`wRC+`)

### Save data:
save(list=c("FG.dat","FG.dat.empty"),
     file="int/FG_data.Rda")

## Import Savant data:

### Overall player data:
B.250 <- read_csv(file="data/savant-custom-250PA-2015-2024.csv",
                  col_names=TRUE) %>%
  bind_rows(read_csv(file="data/savant-custom-100PA-2020.csv",
                     col_names=TRUE)) %>%
  dplyr::rename(name=`last_name, first_name`)
colnames(B.250) <- c("Name","Player_ID","Season",
                     "AB","PA","H","1B","2B","3B","HR","K percent","BB percent",
                     "AVG","SLG","OBP","OPS","BABIP","wOBA","xwOBA",
                     "Pull percent","Cent percent", "Oppo percent","batted ball")

### Player shift rates, pre-2023:
Sav.Shifts <- NULL
for (year in 2016:2022) {
  Sav.Shifts <- Sav.Shifts %>%
    bind_rows(read_csv(file=paste0("data/savant-pos-",if_else(year==2020,"100","250"),
                                   "PA-",year,".csv"),
                       col_names=TRUE))
}
colnames(Sav.Shifts) <- c("Season","Name","Team_ID","Batter",
                          "PA","PA_Shifts","PA_Shift_Percent","wOBA_Shift",
                          "PA_NoShift","PA_NoShift_Percent","wOBA_NoShift")

### Player shade rates, 2023--:
Sav.Shades <- NULL
for (year in 2023:2024) {
  Sav.Shades <- Sav.Shades %>%
    bind_rows(read_csv(file=paste0("data/savant-pos-",if_else(year==2020,"100","250"),
                                   "PA-",year,".csv"),
                       col_names=TRUE))
}
colnames(Sav.Shades) <- c("Season","Name","Team_ID","Batter",
                          "PA","PA_Shades","PA_Shade_Percent","wOBA_Shade",
                          "PA_NoShade","PA_NoShade_Percent","wOBA_NoShade")

### Combining overall player data with shift information:
Sav.2016_22 <- B.250 %>% dplyr::filter(Season %in% 2016:2022) %>%
  left_join(Sav.Shifts, by=c("Season","Name"))

### Save data:
save(list=c("B.250","Sav.Shades","Sav.2016_22"),
     file="int/Sav_data.Rda")
