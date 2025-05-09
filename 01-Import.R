## 01-Import.R
## Importing, cleaning, and preparing Baseball Savant and FanGraphs data

require(tidyverse)
require(baseballr) ## used to map across IDs

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

## Import ID-matching info:
Crosswalk <- chadwick_player_lu()

## Import Savant data:

### Overall player data:
B.250 <- read_csv(file="data/savant-custom-250PA-2015-2024.csv",
                  col_names=TRUE) %>%
  bind_rows(read_csv(file="data/savant-custom-100PA-2020.csv",
                     col_names=TRUE)) %>%
  dplyr::rename(name=`last_name, first_name`)
colnames(B.250) <- c("Name","Player_ID","Season","Age",
                     "AB","PA","H","1B","2B","3B","HR","K percent","BB percent",
                     "AVG","SLG","OBP","OPS","BABIP","wOBA","xwOBA",
                     "Pull percent","Cent percent", "Oppo percent","batted ball")
### Drop Jr./Sr./III/II and create display name
B.250$Name_Match <- sub(" Jr.", "", B.250$Name)
B.250$Name_Match <- sub(" Sr.", "", B.250$Name_Match)
B.250$Name_Match <- sub(" III", "", B.250$Name_Match)
B.250$Name_Match <- sub(" II", "", B.250$Name_Match)
B.250$Name_Disp <- sub("(^.*),\\s(.*$)","\\2 \\1", B.250$Name)

### Add FG and BRef IDs:
B.250 <- B.250 %>% left_join(Crosswalk %>% dplyr::select(key_mlbam, key_bbref, key_fangraphs, 
                                                         name_first, name_last),
                             by=join_by(Player_ID==key_mlbam))

### Player shift rates (pre-2023) and shade rights (2023-):
Sav.Shifts <- NULL
for (year in 2016:2024) {
  Sav.Shifts <- Sav.Shifts %>%
    bind_rows(read_csv(file=paste0("data/savant-pos-10PA-",year,".csv"),
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
                PA_Shade_Percent=PA_Shade/PA*100, PA_NoShade_Percent=PA_NoShade/PA*100) %>%
  ungroup()
### Drop Jr./Sr./III/II
Sav.Shifts$Name_Match <- sub(" Jr.", "", Sav.Shifts$Name)
Sav.Shifts$Name_Match <- sub(" Sr.", "", Sav.Shifts$Name_Match)
Sav.Shifts$Name_Match <- sub(" III", "", Sav.Shifts$Name_Match)
Sav.Shifts$Name_Match <- sub(" II", "", Sav.Shifts$Name_Match)
### Specific Name Fixes:
Sav.Shifts[Sav.Shifts$Name=="García, Leury","Name_Match"] <- "Garcia, Leury"
Sav.Shifts[Sav.Shifts$Name=="Alvarez, Pedro","Name_Match"] <- "Álvarez, Pedro"

### Save data:
save(list=c("B.250","Sav.Shifts"),
     file="int/Sav_data.Rda")

