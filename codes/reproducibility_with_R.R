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

#CHALLENGE 3 (answer is VIC)
BOM_s_long <- gather(BOM_s, key = "Station_number", value = "data", -info) #gather the data
BOM_s_wide <- spread(BOM_s_long, key="info", value="data")


#codes from Elise for Challenge 3

joined_data <- full_join(BOM_d_temp_rain, BOM_s_wide, by="Station_number")

Q3 <- group_by(joined_data, state) %>% 
  mutate(daily_temp_diff = Temp_max - Temp_min) %>% 
  summarise(mean_daily_temp_diff = mean(daily_temp_diff, na.rm = T)) %>% #can also use drop.na() instead
  filter(mean_daily_temp_diff == min(mean_daily_temp_diff))


#Challenge 4 (answer is lowest longitude's average solar exp is 19.18 and highest longitude's average solar exposure is 19.49)
BOM_d_sun_exp <- BOM_d %>% 
  filter(Solar_exposure!="-")

BOM_d_sun_exp$Solar_exposure <- as.numeric(BOM_d_sun_exp$Solar_exposure)
Station_mean_sun_exp <- group_by(BOM_d_sun_exp, Station_number) %>% 
  summarise(mean_sun_exp=mean(Solar_exposure))

BOM_s_wide$Station_number <- as.numeric(BOM_s_wide$Station_number)

lon_compare <- inner_join(Station_mean_sun_exp, BOM_s_wide, by="Station_number") %>%
  group_by(lon) %>% 
  summarise(mean_sun_exp_lon=mean(mean_sun_exp))%>%
  arrange(lon)


