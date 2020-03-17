#Challenge: Putting it all together

#required packages
library(tidyverse)

#load the two datasets
BOM_d <- read_csv("data/BOM_data.csv")

BOM_s <- read_csv("data/BOM_stations.csv")

#CHALLENGE 1
#create a dataset by grouping station_numbers and filtering with all the values except "-/-" within the variable Temp_min_max

BOM_d_temp_sep <- separate(BOM_d, Temp_min_max, into = c("Temp_min", "Temp_max"), sep="/")


BOM_d_temp_rain <- filter(BOM_d_temp_sep, Temp_min !="-" & Temp_max !="-" & Rainfall!="-")

Temp_rain_days <- group_by(BOM_d_temp_rain, Station_number) %>% 
  summarise(days=n())

#CHALLENGE 2 (answer is June)

#Average temp by month
BOM_d_temp_rain$Temp_min <- as.numeric(BOM_d_temp_rain$Temp_min)
BOM_d_temp_rain$Temp_max <- as.numeric(BOM_d_temp_rain$Temp_max)
BOM_d_temp_rain$Rainfall <- as.numeric(BOM_d_temp_rain$Rainfall)

Average_month_temp_rain <- BOM_d_temp_rain %>% 
  group_by(Month) %>% 
  summarise(mean_min_Temp=mean(Temp_min), mean_max_Temp=mean(Temp_max), mean_Rainfall=mean(Rainfall))

Temp_diff <- mutate(Average_month_temp_rain, Temp_diff=mean_max_Temp-mean_min_Temp) %>% 
  arrange(Temp_diff)

#CHALLENGE 3 (answer is ACT)
BOM_s_long <- gather(BOM_s, key = "Station_number", value = "data", -info) #gather the data
BOM_s_wide <- spread(BOM_s_long, key="info", value="data")

BOM_stations <- group_by(BOM_d_temp_rain, Station_number) %>% 
  summarise(mean_min_Temp=mean(Temp_min), mean_max_Temp=mean(Temp_max), mean_Rainfall=mean(Rainfall))

BOM_s_wide$Station_number <- as.numeric(BOM_s_wide$Station_number)

combined_ds <- inner_join(BOM_stations, BOM_s_wide, by="Station_number")

Average_state_temp_rain <- combined_ds %>% 
  group_by(state) %>% 
  summarise(mean_state_min_temp=mean(mean_min_Temp), mean_state_max_temp=mean(mean_max_Temp)) %>% 
  arrange(mean_state_min_temp)
  
#Challenge 4 (answeris lowest longitude's average solar exp is 19.18 and highest longitude's average solar exposure is 19.49)
BOM_d_sun_exp <- BOM_d %>% 
  filter(Solar_exposure!="-")

BOM_d_sun_exp$Solar_exposure <- as.numeric(BOM_d_sun_exp$Solar_exposure)
Station_mean_sun_exp <- group_by(BOM_d_sun_exp, Station_number) %>% 
  summarise(mean_sun_exp=mean(Solar_exposure))

lon_compare <- inner_join(Station_mean_sun_exp, BOM_s_wide, by="Station_number") %>%
  group_by(lon) %>% 
  summarise(mean_sun_exp_lon=mean(mean_sun_exp))%>%
  arrange(lon)
