# Traffic Safety Data
David Hood  
11/21/2017  



## Traffic Data

This repository is for the R code for obtaining various public sources of data around traffic safety (mainly from a New Zealand perspective).

For the benefit of non-R users, I am making a .csv copy of the obtained data in the traffic_data folder.

## OECD data

OECD data is obtained using the OECD package in R, and by doing a get_datasets() command, you can then look through for the IDs or relevvant data sets


```r
dataFolder= "traffic_data"
library(OECD)
dataset_list <- get_datasets()
# search_dataset("accidents", data = dataset_list)
```

### ITF_ROAD_ACCIDENTS

csv obtained 2017-11-20

The OECD ITF_ROAD_ACCIDENTS dataset is road accident information supplied by the ITF


```r
dataset <- "ITF_ROAD_ACCIDENTS"
csvfile <- paste0(dataFolder,"/OECD_",dataset, ".csv")
if(!file.exists(csvfile)){
  dstruc <- get_data_structure(dataset)
  df <- get_dataset(dataset)
  df2 <- merge(df, dstruc$UNIT, by.x="UNIT", by.y="id")
  names(df2)[9] <- "UNIT_label"
  df3 <- merge(df2, dstruc$VARIABLE, by.x="VARIABLE", by.y="id")
  names(df3)[10] <- "VARIABLE_label"
  write.csv(df3, file=csvfile, row.names = FALSE)
}
```


### ITF_PASSENGER_TRANSPORT

csv obtained 2017-11-20

The OECD ITF_PASSENGER_TRANSPORT dataset is passenger transport supplied by the ITF


```r
dataset <- "ITF_PASSENGER_TRANSPORT"
csvfile <- paste0(dataFolder,"/OECD_",dataset, ".csv")
if(!file.exists(csvfile)){
  dstruc <- get_data_structure(dataset)
  df <- get_dataset(dataset)
  df2 <- merge(df, dstruc$UNIT, by.x="UNIT", by.y="id")
  names(df2)[9] <- "UNIT_label"
  df3 <- merge(df2, dstruc$VARIABLE, by.x="VARIABLE", by.y="id")
  names(df3)[10] <- "VARIABLE_label"
  write.csv(df3, file=csvfile, row.names = FALSE)
}
```


### ITF_INV-MTN_DATA

csv obtained 2017-11-20

The OECD ITF_INV-MTN_DATA dataset is transport infrastructure investment and maintenance spending supplied by the ITF


```r
dataset <- "ITF_INV-MTN_DATA"
csvfile <- paste0(dataFolder,"/OECD_",dataset, ".csv")
if(!file.exists(csvfile)){
  dstruc <- get_data_structure(dataset)
  df <- get_dataset(dataset)
  df2 <- merge(df, dstruc$UNIT, by.x="UNIT", by.y="id")
  names(df2)[9] <- "UNIT_label"
  df3 <- merge(df2, dstruc$VARIABLE, by.x="VARIABLE", by.y="id")
  names(df3)[10] <- "VARIABLE_label"
  write.csv(df3, file=csvfile, row.names = FALSE)
}
```

### IRTAD_CASUAL_BY_AGE

csv obtained 2017-11-21

The OECD IRTAD_CASUAL_BY_AGE dataset is Casualties by age & road user, not as up to date or as many countries as the headline ITF_ROAD_ACCIDENTS data, but more detail where available. As this is a huge dataset from all of the detail, this query was specifically for deaths 2005+


```r
dataset <- "IRTAD_CASUAL_BY_AGE"
csvfile <- paste0(dataFolder,"/OECD_",dataset, ".csv")
if(!file.exists(csvfile)){
  dstruc <- get_data_structure(dataset)
  df <- get_dataset(dataset, filter="AUS+AUT+BEL+CAN+CZE+DNK+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LTU+LUX+NLD+NZL+NOR+POL+PRT+SVN+ESP+SWE+CHE+GBR+GB-IRTAD+GBR-NIR+USA+ARG+CHL.740.110+120+130+140+150+190+230+240+250+260+270+280+290+310+370.1010+1050+1030+1040+1060+1100+1110+1120+1140.NUMBER.CORRECTED", start_time = 2005, end_time = 2016, pre_formatted = TRUE)
  df2 <- merge(df, dstruc$INJURY_TYPE, by.x="INJURY_TYPE", by.y="id")
  names(df2)[11] <- "INJURY_TYPE_label"
  df3 <- merge(df2, dstruc$AGE_GROUP, by.x="AGE_GROUP", by.y="id")
  names(df3)[12] <- "AGE_GROUP_label"
  df4 <- merge(df3, dstruc$TRAFFIC_PARTICIPATION, by.x="TRAFFIC_PARTICIPATION", by.y="id")
  names(df4)[13] <- "TRAFFIC_PARTICIPATION_label"
  write.csv(df4, file=csvfile, row.names = FALSE)
}
```




