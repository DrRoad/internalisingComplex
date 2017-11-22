#NZ MoH Years comparison 

library(dplyr)
library(tidyr)

MoH <- read.csv("~/nbu/nbu_gitviagithub/internalisingComplex/traffic_data/NZ_MoH_Hospital_Discharges.csv",
                stringsAsFactors = FALSE)

MoH %>% select(ICDThreeChar) %>% distinct(ICDThreeChar) %>% arrange(ICDThreeChar) %>% View()

MoH %>% separate(ICDCode, into=c("v","a","b"), sep=c(1,2)) %>%
  mutate(mode = case_when(
    a == "0" ~ "pedestrian",
    a == "1" ~ "pedal cyclist",
    a == "2" ~ "motorcycle",
    a == "3" ~ "three-wheeler",
    a == "4" ~ "car",
    a == "5" ~ "pickup/van",
    a == "6" ~ "heavy transport",
    a == "7" ~ "bus",
    a == TRUE ~ "other"
    ),
  collision = case_when(
    mode == "pedestrian" & b == "0" ~ "pedestrian coveyance",
    mode != "pedestrian" & b == "0"  ~ "pedestrian",
    b == "1"  ~ "pedal cyclist",
    b == "2"  ~ "motorcycle",
    b == "3"  ~ "car/picup/van",
    b == "4"  ~ "heavy transport/bus",
    b == "5"  ~ "train",
    b == "6"  ~ "other nonmotor vehical",
    b == "7"  ~ "fixed/stationary object",
    b == "8"  ~ "no collision",
    b == "9"   ~ "other unspecified",
    b == TRUE ~ "other"
    )
  ) %>% 
  filter(mode %in% c("pedestrian", "pedal cyclist", "motorcycle", "car", "pickup/van"),
         collision %in% c("pedestrian", "pedal cyclist", "motorcycle",
                         "car/picup/van", "heavy transport/bus",
                         "fixed/stationary object", "no collision")) %>%
group_by(FinancialYear, mode, collision) %>% summarise(occurences = sum(Discharges)) %>%
  spread(key=FinancialYear, value=occurences) %>% 
  rename(yr13_14 = `2013/14`, yr14_15 = `2014/15`, 
         travel_mode = mode, collision_with = collision) %>%
  mutate(percent_change= 100 * yr14_15 / yr13_14) %>% 
  arrange(desc(percent_change)) %>% View()
