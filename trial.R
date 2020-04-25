library(tidyverse)

bom_data <- read_csv("raw_data/BOM_data.csv")
bom_data

bom_stations <- read_csv("raw_data/BOM_stations.csv")
bom_stations

# Question 1

# filter columns with values and convert values to numeric data

bom_numeric_data <- bom_data %>% 
  separate(Temp_min_max,into = c("t_min", "t_max"), sep="/")%>% 
  filter(t_min != "-",t_max != "-", Rainfall !="-", Solar_exposure !="-") %>% 
  mutate(t_min = as.numeric(t_min))%>% 
  mutate(t_max = as.numeric(t_max ))%>%
  mutate(Rainfall= as.numeric(Rainfall))%>%
  mutate(Solar_exposure= as.numeric(Solar_exposure))%>%
  mutate(temp_difference = t_max-t_min)

bom_numeric_data

# convert station numbers to numeric data 

bom_stations_numeric <- gather(bom_stations,Station_number,value="value",2:21) %>% 
  spread(info, value="value") %>% 
  mutate(Station_number=as.numeric(Station_number))
bom_stations_numeric

# filter data for Perth station ID 9225

data_9225 <- full_join(bom_numeric_data,bom_stations_numeric) %>% 
  filter (Station_number == "9225")

data_9225

# relationship between maximum and minimum temperature

tmax_tmin <- ggplot(data_9225, aes(x=t_max, y=t_min)
          )+geom_point()
tmax_tmin

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
