perth1 <- filter(joined_data, Station_number==9225) 
perth <- filter(perth1, Solar_exposure!="-")

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
  geom_point(aes(as.numeric(Temp_max), as.numeric(Temp_min), size=as.numeric(Rainfall),
                 colour=as.numeric(Solar_exposure)), alpha=0.2)+
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

question4$state_f <- factor(question4$state, levels = c("ACT", "NT", "QLD", "SA", "VIC", "WA", "NSW")) #to reorder the labels

question4 %>% 
  group_by(state, Station_number, Month) %>%
  summarise(mean_rainfall=mean(Rainfall)) %>%
  arrange(Month) %>% 
  ggplot(aes(as.factor(Month), mean_rainfall, colour=state, group=Station_number))+
  geom_line(size=1)+
  scale_colour_manual(values=c('#EC519D', '#8CA4D4', '#70C6A3', '#79C143', '#6CCDD1', '#fdae61', '#00B1D9'))

plot_question4 <- question4 %>% 
  group_by(state_f, Station_number, Month) %>%
  summarise(mean_rainfall=mean(Rainfall)) %>%
  arrange(Month) %>% 
  ggplot(aes(as.factor(Month), mean_rainfall, group=as.factor(Station_number), colour=state_f))+
  geom_line(size=1)+
  facet_wrap(~state_f)+
  theme(legend.justification=c(1,0), legend.position=c(1,0), legend.direction="horizontal")+
  labs(x="Month",
       y="Average rainfall",
       colour="State")
ggsave(filename="outputs/plot question4.png", plot=plot_question4, width=15, height=12, dpi=300, units="cm")
