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
# to numeric values 

bom_stations_numeric <- 
  gather(bom_stations,Station_number,value="value",2:21) %>% 
  spread(info, value="value") %>% 
  mutate(Station_number=as.numeric(Station_number))

bom_stations_numeric

# Data for Perth station ID 9225

data_9225 <- full_join(bom_numeric_data,bom_stations_numeric) %>% 
  filter (Station_number == "9225")

data_9225

# relationship between maximum and minimum temperature

tmax_tmin <- ggplot(data_9225, aes(x=t_max, y=t_min))+
  geom_point(colour = "red", size = 1, alpha=0.2)

tmax_tmin

plot_1 <- tmax_tmin+
  labs(title = "Figure 1: maximum temperature and minimum temperature", 
       x = "maximum temperature (°C)", 
       y= "minimum temperature (°C)")+
  theme_bw()+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10))

plot_1

# relationship between maximum temperature and rainfall

tmax_rainfall <- ggplot(data_9225, aes(x=t_max, y=Rainfall))+
  geom_point(colour = "blue", size = 1, alpha=0.2)

tmax_rainfall

plot_2 <- tmax_rainfall+
  labs(title = "Figure 2: maximum temperature and rainfall", 
       x = "maximum temperature (°C)", 
       y= "rainfall (mm)")+
  theme_bw()+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10))

plot_2

# relationship between maximum temperature and solar exposure

tmax_solarexp <- ggplot(data_9225, aes(x=t_max, y=Solar_exposure))+
  geom_point(colour = "orange", size = 1, alpha=0.2)

tmax_solarexp

plot_3 <- tmax_solarexp+
  labs(title = "Figure 3: maximum temperature and solar exposure", 
       x = "maximum temperature (°C)", 
       y= "solar exposure (MJ/ m2)")+
  theme_bw()+
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 10))

plot_3

