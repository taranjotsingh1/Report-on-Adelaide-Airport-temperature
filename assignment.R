#Libraries
library(tidyverse)
library(ggpubr)
library(ggthemes)
# Data
maximum_temp <- read.csv("maximum_temp/IDCJAC0010_023034_1800_Data.csv")
minimum_temp <- read.csv("minimum_temp/IDCJAC0011_023034_1800_Data.csv")
rainfall <- read.csv("rainfall/IDCJAC0009_023034_1800_Data.csv")
# renaming the columns name for easy use
names(maximum_temp)[6] <- "max_temp"
names(minimum_temp)[6] <- "min_temp"
names(rainfall)[6] <- "rainfall_amount"

## part one
# storing the graph in varibale
maximum_graph <- maximum_temp %>% # collecting the data
  select(Month, max_temp) %>% 
  ggplot(aes(x= Month, y = max_temp), na.rm= TRUE)+ # deciding the axis
  geom_point()+ # point graph
  labs(x = "Months", y = "maximum temp in Celsius", 
       title = "Maximum temperature in months
       from 1955 to 2021")+
  scale_x_continuous(breaks = seq(1,12,1))+ # deciding the x-axis
  theme_hc() # deciding the theme
summary(maximum_temp$max_temp) # looking at the summary for deciding the measures 
# looking at the minimum values of data
maximum_temp[maximum_temp$max_temp == min(maximum_temp$max_temp, na.rm = TRUE),]

# minimum temp graph
minimum_graph <- minimum_temp %>% 
  select(Month, min_temp) %>% 
  ggplot(aes(x = Month, y = min_temp))+ # deciding the axis
  geom_point()+ # point graph
  labs(x = "Months", y = "Minimum temp in Celsius", 
       title = "Minimum temperature in 
       months from 1955 to 2021")+
  scale_x_continuous(breaks = seq(1,12,1))+
  theme_economist()

summary(minimum_temp$min_temp) # looking at the summary for deciding the measures 
# looking at the maximum value of data
minimum_temp[minimum_temp$min_temp == max(minimum_temp$min_temp, na.rm = TRUE), ]

# rainfall data in list vectot
rainfall_grpah <- rainfall %>% 
  select(Year, rainfall_amount, Month) %>% 
  ggplot(aes(x = Month, y= rainfall_amount))+
  geom_point()+
  labs(x = "Months", y = "rainfall in millimeter",
  title = "Rainfall From 
  1955 to 2021 in months") +
  scale_x_continuous(breaks = seq(1,12,1))
summary(rainfall$rainfall_amount) # looking at the summary for deciding the measures 
# checking the maximum value raw
rainfall[rainfall$rainfall_amount == max(rainfall$rainfall_amount, na.rm = TRUE), ]
# plotting all three graphs 
ggpubr::ggarrange(maximum_graph, minimum_graph, rainfall_grpah)  # arranging the graphs


## part 2
maximum_temp %>%  # grabbing the data
  select(Year, Month, max_temp) %>% 
  drop_na() %>% # dropping all the NA's
  ggplot(aes(x = Year, y= max_temp))+ # deciding the axis 
  geom_point(aes(colour = Month))+ # putting months as a colour
  scale_x_binned(breaks = seq(1950,2020,10))+ # deciding the x-axis scales
  theme_economist()+
  labs(x = "Years", y = "Maximum temperature recorded in degree Celsius", 
       title = "Maximum temperature as per decade since 1955")




### part 3
# calculating the extreme heat days
extreme_heat <- maximum_temp %>% 
  filter(max_temp >= 35) %>%  # filtering the data
  group_by(Year) %>% # grouping by year
  summarise (hot_days = n())
# calcluating extreme cold days
extreme_cold <- minimum_temp %>% 
  filter(min_temp <= 5) %>% # filtering the data
  group_by(Year) %>% 
  summarise(cold_days = n())
# making graphs to see the comparison
extreme_cold %>% 
  inner_join(extreme_heat, by = "Year") %>% # joining the data
  select_all() %>% 
  # ploting the graph
  ggplot(aes(x = cold_days, y = hot_days))+
  geom_point(aes(colour = Year))+
  labs(x = "cold days", y = "Hot days", title = "Hot days vs Cold days")

# looking at the correlation between hot days and cold days
extreme_cold %>% 
  inner_join(extreme_heat, by = "Year") %>% 
  select_all() %>% 
  summarise(cor =  cor(hot_days, cold_days)) 

# making new data frame for further inquiries of linear relation
joined <- extreme_cold %>% 
  inner_join(extreme_heat, by = "Year")

# drawing the cold days graphs
joined %>% 
  ggplot(aes(x= Year, y= cold_days))+
  geom_point()+
  labs(x = "Year", y = "Cold days", title = "Cold days graph")+ # 
  theme_economist()+
  geom_smooth(method = "loess")

fit <- lm(cold_days ~Year, data= joined) # putting in function for linear relation
cor(joined$cold_days, joined$Year) # looking at correlation 
summary(fit)

# hot days graph 
joined %>% 
  ggplot(aes(x = Year, y = hot_days))+
  geom_point()+
  stat_smooth(method = "loess")+
  labs(x = "Year", y= "Hot days", title = "Hot days Graph")+
  theme_economist()

fit <- lm(hot_days ~ Year, data = joined) # looking at the linear relationship 
cor(joined$hot_days, joined$Year)# looking at the correlation 
summary(fit) # summary of linear relationship


## part 4
# summary of rainfall to decide the measure
summary(rainfall$rainfall_amount, na.rm = TRUE)
# plotting the point graph
rainfall %>% 
  drop_na() %>% # dropping NAs
  filter(rainfall_amount > 0) %>%  # removing the days with out rain
  select(rainfall_amount, Year, Month) %>% 
  ggplot(aes(x = Year, y = rainfall_amount))+ # choosing the axis. 
  geom_point(aes(colour = Month))+ # point graphs
  theme_economist()+
  labs(x = "Year", y = "Rain amount in millimetres", 
       title = "Rainfall from 1955 to 2021")+
  geom_smooth(method = "loess")

cor(rain_days$rainfall_amount, rain_days$Year)# looking at the correlation 

# aggregating the data to check the mean of rain in all years.
aggregate(rainfall_amount ~ Year, data = rainfall, FUN = 'mean')

# linear regression model
fit <- lm(rainfall_amount ~ Year, data = rainfall)
summary(fit) # summary of linear regression model







