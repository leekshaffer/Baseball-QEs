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
### Drop Jr./Sr./III/II
B.250$Name_Match <- sub(" Jr.", "", B.250$Name)
B.250$Name_Match <- sub(" Sr.", "", B.250$Name_Match)
B.250$Name_Match <- sub(" III", "", B.250$Name_Match)
B.250$Name_Match <- sub(" II", "", B.250$Name_Match)

### Player shift rates (pre-2023) and shade rights (2023-):
Sav.Shifts <- NULL
for (year in 2016:2024) {
  Sav.Shifts <- Sav.Shifts %>%
    bind_rows(read_csv(file=paste0("data/savant-pos-",if_else(year==2020,"25","50"),
                                   "PA-",year,".csv"),
                       col_names=TRUE))
}
colnames(Sav.Shifts) <- c("Season","Name","Team_ID","Batter",
                          "PA","PA_Shift","PA_Shift_Percent","wOBA_Shift",
                          "PA_NoShift","PA_NoShift_Percent","wOBA_NoShift",
                          "PA_Shade","PA_Shade_Percent","wOBA_Shade",
                          "PA_NoShade","PA_NoShade_Percent","wOBA_NoShade")
Sav.Shifts <- Sav.Shifts %>% group_by(Season,Name) %>%
  dplyr::summarize(PA_total=sum(PA), PA_Shift_total=sum(PA_Shift),
                PA_NoShift_total=sum(PA_NoShift),
                wOBA_Shift_total=sum(wOBA_Shift*PA_Shift)/sum(PA_Shift),
                wOBA_NoShift_total=sum(wOBA_NoShift*PA_NoShift)/sum(PA_NoShift),
                PA_Shade_total=sum(PA_Shade),
                PA_NoShade_total=sum(PA_NoShade),
                wOBA_Shade_total=sum(wOBA_Shade*PA_Shade)/sum(PA_Shade),
                wOBA_NoShade_total=sum(wOBA_NoShade*PA_NoShade)/sum(PA_NoShade)) %>%
  dplyr::rename(PA=PA_total, PA_Shift=PA_Shift_total, PA_NoShift=PA_NoShift_total,
                wOBA_Shift=wOBA_Shift_total, wOBA_NoShift=wOBA_NoShift_total,
                PA_Shade=PA_Shade_total, PA_NoShade=PA_NoShade_total,
                wOBA_Shade=wOBA_Shade_total, wOBA_NoShade=wOBA_NoShade_total) %>%
  dplyr::mutate(PA_Shift_Percent=PA_Shift/PA*100, PA_NoShift_Percent=PA_NoShift/PA*100,
                PA_Shade_Percent=PA_Shade/PA*100, PA_NoShade_Percent=PA_NoShade/PA*100)
### Drop Jr./Sr./III/II
Sav.Shifts$Name_Match <- sub(" Jr.", "", Sav.Shifts$Name)
Sav.Shifts$Name_Match <- sub(" Sr.", "", Sav.Shifts$Name_Match)
Sav.Shifts$Name_Match <- sub(" III", "", Sav.Shifts$Name_Match)
Sav.Shifts$Name_Match <- sub(" II", "", Sav.Shifts$Name_Match)

### Player shade rates, 2023--:
# Sav.Shades <- NULL
# for (year in 2023:2024) {
#   Sav.Shades <- Sav.Shades %>%
#     bind_rows(read_csv(file=paste0("data/savant-pos-",if_else(year==2020,"25","50"),
#                                    "PA-",year,".csv"),
#                        col_names=TRUE))
# }
# colnames(Sav.Shades) <- c("Season","Name","Team_ID","Batter",
#                           "PA","PA_Shade","PA_Shade_Percent","wOBA_Shade",
#                           "PA_NoShade","PA_NoShade_Percent","wOBA_NoShade")
# Sav.Shades <- Sav.Shades %>% group_by(Season,Name) %>%
#   dplyr::summarize(PA_total=sum(PA), PA_Shade_total=sum(PA_Shade),
#                 PA_NoShade_total=sum(PA_NoShade),
#                 wOBA_Shade_total=sum(wOBA_Shade*PA_Shade)/sum(PA_Shade),
#                 wOBA_NoShade_total=sum(wOBA_NoShade*PA_NoShade)/sum(PA_NoShade)) %>%
#   dplyr::rename(PA=PA_total, PA_Shade=PA_Shade_total, PA_NoShade=PA_NoShade_total,
#                 wOBA_Shade=wOBA_Shade_total, wOBA_NoShade=wOBA_NoShade_total) %>%
#   dplyr::mutate(PA_Shade_Percent=PA_Shade/PA*100, PA_NoShade_Percent=PA_NoShade/PA*100)
# ### Drop Jr./Sr./III/II
# Sav.Shades$Name_Match <- sub(" Jr.", "", Sav.Shades$Name)
# Sav.Shades$Name_Match <- sub(" Sr.", "", Sav.Shades$Name_Match)
# Sav.Shades$Name_Match <- sub(" III", "", Sav.Shades$Name_Match)
# Sav.Shades$Name_Match <- sub(" II", "", Sav.Shades$Name_Match)

### Combining overall player data with shift information:
Sav.Full <- B.250 %>% dplyr::filter(Season %in% 2016:2024) %>%
  left_join(Sav.Shifts %>% dplyr::select(!Name), by=c("Season","Name_Match"))

### Save data:
save(list=c("B.250","Sav.Full"),
     file="int/Sav_data.Rda")
