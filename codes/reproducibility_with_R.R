#Challenge: Putting it all together

#required packages
library(tidyverse)
library(cowplot)

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

#Week break challenges
#Challenge 1
perth1 <- filter(joined_data, Station_number==9225) 
perth <- filter(perth, Solar_exposure!="-")

plot1 <- ggplot(perth)+
  geom_point(aes(as.numeric(Temp_max), as.numeric(Temp_min), alpha=0.3), colour="dodgerblue3")+
  xlab("Maximum temperature")+
  ylab("Minimum temperature")+
  theme(legend.position = "none")


plot2 <- ggplot(perth)+
  geom_point(aes(as.numeric(Temp_max), as.numeric(Rainfall), alpha=0.2), colour="palegreen4")+
  xlab("Maximum temperature")+
  ylab("Rainfall")+
  theme(legend.position = "none")

plot3 <- ggplot(perth)+
  geom_point(aes(as.numeric(Temp_max), as.numeric(Solar_exposure), alpha=0.2), colour="orchid3")+
  xlab("Maximum temperature")+
  ylab("Solar exposure")+
  theme(legend.position = "none")

#Challenge 2

plot4 <- ggplot(perth)+
  geom_point(aes(as.numeric(Temp_max), as.numeric(Temp_min), size=as.numeric(Solar_exposure),
                 colour=as.numeric(Rainfall)), alpha=0.2)+
  labs(x="Maximum temperature",
       y="Minimum temperature",
       size="Solar exposure",
       colour="Rainfall")+
  theme(legend.text = element_text(size=8), legend.title = element_text(size=8))

#Challenge 3
combined_plot <- plot_grid(plot1, plot2, plot3, plot4, labels ="AUTO")

ggsave(filename="outputs/combined plot.png", plot=combined_plot, width=20, height=15, dpi=300, units="cm")

#Challenge 4
question4 <- select(joined_data, state, Station_number, Month, Rainfall)

question4 %>% 
  group_by(state, Station_number, Month) %>%
  summarise(mean_rainfall=mean(Rainfall)) %>%
  arrange(Month) %>% 
  ggplot(aes(as.factor(Month), mean_rainfall, colour=state, group=Station_number))+
    geom_line(size=1)+
  scale_colour_manual(values=c('#EC519D', '#8CA4D4', '#70C6A3', '#79C143', '#6CCDD1', '#fdae61', '#00B1D9'))

plot_question4 <- question4 %>% 
  group_by(state, Station_number, Month) %>%
  summarise(mean_rainfall=mean(Rainfall)) %>%
  arrange(Month) %>% 
  ggplot(aes(as.factor(Month), mean_rainfall, group=as.factor(Station_number), colour=state))+
  geom_line(size=1)+
  facet_wrap(~state)+
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.direction="horizontal")+
  labs(x="Month",
       y="Average rainfall",
       colour="State")
ggsave(filename="outputs/plot question4.png", plot=plot_question4, width=15, height=12, dpi=300, units="cm")
  