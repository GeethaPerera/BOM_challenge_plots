library(tidyverse)

bom_data <- read_csv("raw_data/BOM_data.csv")
bom_data

bom_stations <- read_csv("raw_data/BOM_stations.csv")
bom_stations

# Question 1

# tidying up bom data (converting values to numeric data)

bom_numeric_data <- bom_data %>% 
  separate(Temp_min_max,into = c("t_min", "t_max"), sep="/")%>% 
  mutate(t_min = as.numeric(t_min))%>% 
  mutate(t_max = as.numeric(t_max ))%>%
  mutate(Rainfall= as.numeric(Rainfall))%>%
  mutate(Solar_exposure= as.numeric(Solar_exposure))%>%
  mutate(temp_difference = t_max-t_min)

bom_numeric_data

# tidying up bom stations data and converting station numbers 
# to numeric values) 

bom_stations_numeric <- gather(bom_stations,Station_number,value="value",2:21) %>% 
  spread(info, value="value") %>% 
  mutate(Station_number=as.numeric(Station_number))

bom_stations_numeric

# Data for Perth station ID 9225

data_9225 <- full_join(bom_numeric_data,bom_stations_numeric) %>% 
  filter (Station_number == "9225")

data_9225

# relationship between maximum and minimum temperature

tmax_tmin <- ggplot(data_9225, aes(x=t_max, y=t_min)
          )+geom_point(colour = "blue", size = 1, alpha=0.2)
  
tmax_tmin

plot_1 <- tmax_tmin+
  labs(title = "Figure 1", 
  x = "maximum temperature", 
  y= "minimum temperature")+theme_bw()+
  theme(axis.text = element_text(size = 4),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10))

plot_1
# relationship between maximum temperature and rainfall

tmax_rainfall <- ggplot(data_9225, aes(x=t_max, y=Rainfall)
)+geom_point()

tmax_rainfall

# relationship between maximum temperature and solar exposure

tmax_solarexposure <- ggplot(data_9225, aes(x=t_max, y=Solar_exposure)
)+geom_point()
tmax_solarexposure

# Question 2 - single scatter plot showing the relationships between 
# min and max temperature, rainfall and solar exposure

?geom_point

weather_data <- ggplot(data_9225, 
              aes(x=t_min, y=t_max, 
              colour=Solar_exposure, size=Rainfall)
              )+geom_point(
              )+theme(legend.position = "bottom")

weather_data

# Question 3 

library(cowplot)

final_plot <- plot_grid(tmax_tmin, tmax_rainfall, tmax_solarexposure, weather_data)
final_plot

ggsave(filename = "results/plot.png", plot = final_plot, width = 30, height = 20, dpi = 300, units = "cm")

# Question 4

bom_data <- read_csv("raw_data/BOM_data.csv")
bom_data

bom_rainfall_data <- bom_data %>% 
  filter(Rainfall !="-") %>% 
  mutate(Rainfall= as.numeric(Rainfall))

bom_rainfall_data 
view(bom_rainfall_data)




bom_stations_numeric

bom_rainfall_full <- full_join(bom_rainfall_data, bom_stations_numeric)
bom_rainfall_full

view(bom_rainfall_full)


rainfall_by_station

bom_monthly_total_RF <- bom_rainfall_full %>% 
  group_by(Station_number, name, state, Year, Month, state) %>% 
  summarise(monthly_total_RF=sum(Rainfall))


bom_monthly_average_RF<- bom_monthly_total_RF %>% 
  group_by(Station_number,name, state, Month) %>% 
  summarise(average_monthly_RF=mean(monthly_total_RF)) 

bom_monthly_average_RF

bom_text <- bom_monthly_average_RF %>% filter(Month==1)

bom_text 


# generating plots

rough_plot <- ggplot(bom_monthly_average_RF, 
       mapping = aes(x=Month, y=average_monthly_RF, colour= state)
)+geom_line()+facet_wrap("Station_number")

rough_plot

rough_plot+
  labs(title="Average monthly rainfall per station", 
                         y="Average Monthly Rainfall (mm)",
                         x="Month",
                         colour="Station location",
                         caption="SOURCE:BOM meterological data")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 4, 7,10), label = c("Jan", "Apr", "July","Oct"))
