#Challenge: Putting it all together

#required packages
library(tidyverse)

#load the two datasets
BOM_d <- read_csv("data/BOM_data.csv")

BOM_s <- read_csv("data/BOM_stations.csv")

#CHALLENGE 1
#create a dataset by grouping station_numbers and filtering with all the values except "-/-" within the variable Temp_min_max
BOM_d_temp <- BOM_d %>% 
  group_by(Station_number) %>% 
  filter(Temp_min_max != "-/-")

BOM_d_temp_sep <- separate(BOM_d_temp, Temp_min_max, into = c("Temp_min", "Temp_max"), sep="/")


BOM_d_temp_rain <- filter(BOM_d_temp_sep, Temp_min !="-" & Temp_max !="-" & Rainfall!="-")

Temp_rain_days <- group_by(BOM_d_temp_rain, Station_number) %>% 
  summarise(days=n())

#CHALLENGE 2

#Average temp by month
BOM_d_temp_rain$Temp_min <- as.numeric(BOM_d_temp_rain$Temp_min)
BOM_d_temp_rain$Temp_max <- as.numeric(BOM_d_temp_rain$Temp_max)
BOM_d_temp_rain$Rainfall <- as.numeric(BOM_d_temp_rain$Rainfall)

Average_month_temp_rain <- BOM_d_temp_rain %>% 
  group_by(Month) %>% 
  summarise(mean_min_Temp=mean(Temp_min), mean_max_Temp=mean(Temp_max), mean_Rainfall=mean(Rainfall))

Temp_diff <- mutate(Average_month_temp_rain, Temp_diff=mean_max_Temp-mean_min_Temp) %>% 
  arrange(Temp_diff)
