# Import required libraries ----
library(tidyverse)
library(naniar)       # visualization of missing values
library(Amelia)
library(plyr)
library(plotly)
library(ggplot2)
library(calendR)
library(tidyquant)
library(tweenr)
library(ggmap)

library(caTools)
library(caret)
library(randomForest)
library(e1071)
library(forcats)
library(reshape2)
library(skimr)
library(magrittr)
library(rattle)
library(plotly)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(GGally)
library(ggcorrplot)
library(ggpubr)
library(ggrepel)
library(corrgram)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(rpart)
library(rpart.plot)
library(psych)
library(scales)
library(repr)
library(partykit)

#options(repr.plot.width=8, repr.plot.height=6)
#options(warn=-1)

# Data set ----

# Import data
data <- read.csv("https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv")
# data <- read.csv("data/Motor_Vehicle_Collisions_-_Crashes.csv")
head(data)

# replace uppercase with lowercase
names(data) <- tolower(names(data))

'''
# date to datetime object
class(data$crash.date)
data <- as.Date(data$crash.date)

# convert locations back the numeric features 
class(data$longitude)
class(data$latitude)

i <- c("longitude", "latitude")
data[ , i] <- apply(data[ , i], 2, 
                    function(x) as.numeric(as.character(x)))
sapply(data, class)
'''

# Check NULL Values

miss_value <- miss_var_summary(data)
miss_value

gg_miss_upset(data)
missmap(obj = data)


# data[is.na(data)] = 0

# missmap(obj = data)

# Exploratory Data Analysis ----

# Analysis by Borough 

# Number of accidents in each Borough ----

crash_borough <- data %>% group_by(borough) %>% 
  filter(!is.na(borough)) %>% summarise(Count=n()) %>% 
  
  ggplot(aes(x=borough, y=Count,na.rm = T)) + 
  geom_bar(position = 'dodge', stat="identity", fill="#f0902a", color="grey40") + 
  theme_minimal_hgrid() + 
  geom_text(aes(x=borough, label= Count), vjust=-0.5) + 
  labs(title="Crashes per borough", 
       x="Borough", 
       y="Count") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) + 
  theme(plot.title=element_text(hjust=0.5))
crash_borough


# Number of peoples Injured and Killed in each Borough ----

borough_dist <- data %>% select(borough, number.of.persons.injured, 
                                number.of.persons.killed) %>% 
  group_by(borough) %>% summarize(sum1=sum(number.of.persons.injured), 
                                  sum2=sum(number.of.persons.killed))

borough_dist <- borough_dist[2:6,]

inj_bor <- ggplot(borough_dist, aes(x=reorder(borough,-sum1), y=sum1)) +  
  geom_bar(stat="identity", width=.5, fill="#FE9A2E") + 
  coord_flip() + 
  theme_minimal() + 
  geom_text(aes(x=borough, label= sum1), hjust=-0.25) + 
  labs(title="Number of Injured person in each Borough", 
       x="Borough",
       y="") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

kil_bor <- ggplot(borough_dist, aes(x=reorder(borough,-sum2), y=sum2)) +  
  geom_bar(stat="identity", width=.5, fill="#FE9A2E") + 
  coord_flip() + 
  theme_minimal() + 
  geom_text(aes(x=borough, label= sum2), hjust=-0.25) + 
  labs(title="Number of Killed person in each Borough", 
       x = "",
       y="") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

plot_grid(inj_bor, kil_bor, ncol=2)


# Percentage of peoples Injured and Killed in each Borough ----

# Bar Chart

borough_dist_pct <- data %>% select(borough, number.of.persons.injured, 
                                    number.of.persons.killed) %>% 
  group_by(borough) %>% summarize(sum1=sum(number.of.persons.injured), 
                                  sum2=sum(number.of.persons.killed)) %>% 
  mutate(pct1=round(prop.table(sum1),2) * 100) %>% arrange(pct1) %>% 
  mutate(pct2=round(prop.table(sum2),2) * 100) %>% arrange(pct2)

borough_dist_pct <- borough_dist_pct[1:5,]

inj_bor_pct <- ggplot(borough_dist_pct, aes(x=reorder(borough,-pct1), y=pct1)) +  
  geom_bar(stat="identity", width=.5, fill="#FE9A2E") + 
  coord_flip() + 
  theme_minimal() + 
  geom_text(aes(x=borough, label = paste0(round(pct1), "%")), hjust=-0.25) + 
  labs(title="Number of Injured person in each Borough", 
       x="",
       y="") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  scale_y_discrete(breaks = NULL) + 
  #scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

kil_bor_pct <- ggplot(borough_dist_pct, aes(x=reorder(borough,-pct2), y=pct2)) +  
  geom_bar(stat="identity", width=.5, fill="#FE9A2E") + 
  coord_flip() + 
  theme_minimal() + 
  geom_text(aes(x=borough, label = paste0(round(pct2), "%")), hjust=-0.25) + 
  labs(title="Number of Killed person in each Borough", 
       x = "",
       y="") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  scale_y_discrete(breaks = NULL) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

plot_grid(inj_bor_pct, kil_bor_pct, ncol=2)


# Pie chart
chart_pie1 <- ggplot(borough_dist_pct[1:5,], aes(x="", y=pct1, fill = borough)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(pct1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Percentage of Injured") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290", "#3c8d53")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

chart_pie2 <- ggplot(borough_dist_pct[1:5,], aes(x="", y=pct2, fill = borough)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(pct2), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Percentage of Killed") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290", "#3c8d53")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

plot_grid(chart_pie1, chart_pie2, ncol=2)


# Number of Accidents Per Person and Per Square km Area ----

borough_demo <- data %>% group_by(borough) %>% summarise(n=n())

borough_demo$pop <- rep(0,dim(borough_demo)[1])
borough_demo$pop <- ifelse(borough_demo$borough=="MANHATTAN", 1694251, borough_demo$pop)
borough_demo$pop <- ifelse(borough_demo$borough=="BRONX", 1472654, borough_demo$pop)
borough_demo$pop <- ifelse(borough_demo$borough=="QUEENS", 2405464, borough_demo$pop)
borough_demo$pop <- ifelse(borough_demo$borough=="STATEN ISLAND", 495747, borough_demo$pop)
borough_demo$pop <- ifelse(borough_demo$borough=="BROOKLYN", 2736074,borough_demo$pop)

borough_demo$area <- rep(0,dim(borough_demo)[1])
borough_demo$area <- ifelse(borough_demo$borough=="MANHATTAN", 59.13, borough_demo$area)
borough_demo$area <- ifelse(borough_demo$borough=="BRONX", 109, borough_demo$area)
borough_demo$area <- ifelse(borough_demo$borough=="QUEENS", 460, borough_demo$area)
borough_demo$area <- ifelse(borough_demo$borough=="STATEN ISLAND", 152, borough_demo$area)
borough_demo$area <- ifelse(borough_demo$borough=="BROOKLYN",184, borough_demo$area)

borough_demo$per_cap <- round(borough_demo$n/borough_demo$pop,2)
borough_demo$per_area <- round(borough_demo$n/borough_demo$area,0)
borough_demo

# Number of Accidents Per Person
acc_per_cap <- ggplot(borough_demo, aes(x=borough, y=per_cap)) + 
  geom_bar(position = 'dodge', stat="identity", fill="#f0902a", color="grey40") + 
  theme_minimal_hgrid() + 
  coord_flip() + 
  ggtitle("Number of Accidents per Person") + 
  geom_text(aes(x=borough, label= per_cap), hjust=-0.2) + 
  labs(title="Number of Accidents per Person", 
       x="", 
       y="") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

# Number of Accidents Per Square km area
acc_per_area <- ggplot(borough_demo, aes(x=borough, y=per_area)) + 
  geom_bar(position = 'dodge', stat="identity", fill="#f0902a", color="grey40") + 
  theme_minimal_hgrid() + 
  #coord_flip() + 
  ggtitle("Number of Accidents per Square km Area") + 
  geom_text(aes(x=borough, label= per_area), vjust=-0.2) + 
  labs(title="Number of Accidents per Square km Area", 
       x="", 
       y="") + 
  xlim('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND') + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())

plot_grid(acc_per_cap, acc_per_area, nrow=2)


# Reasons for accidents to take place ----

acc_cause_temp <- data %>% select(number.of.persons.injured, number.of.persons.killed, 
                                  contributing.factor.vehicle.1, contributing.factor.vehicle.2, 
                                  contributing.factor.vehicle.3, contributing.factor.vehicle.4, 
                                  contributing.factor.vehicle.5) %>% gather(type, value, 1:2) %>% 
  gather(vehicle.type, cause, 1:5) %>% filter(value!=0, cause!="",cause!="Unspecified")

acc_cause <- acc_cause_temp %>% select(-vehicle.type) %>% group_by(type, cause) %>% 
  summarise(total=sum(value, na.rm=T)) %>% arrange(desc(total))


ggplot(data = acc_cause, aes(x = cause, y = log(total), fill = type)) +
  geom_bar(data = subset(acc_cause, type=="number.of.persons.injured"),
           stat = "identity") +
  geom_bar(data = subset(acc_cause, type=="number.of.persons.killed"),
           stat = "identity",
           position = "identity",
           mapping = aes(y = -log(total))) +
  scale_y_continuous(labels = abs) +
  coord_flip() +
  ggtitle('Causes of Accidents')

# Times Series Animation
time_anim <- data %>% select(number.of.persons.injured, number.of.persons.killed, 
                             contributing.factor.vehicle.1, contributing.factor.vehicle.2, 
                             contributing.factor.vehicle.3, contributing.factor.vehicle.4, 
                             contributing.factor.vehicle.5) %>% gather(type, value, 1:2) %>% 
  gather(vehicle.type, cause, 1:5) %>% filter(value!=0, cause!="",cause!="Unspecified")

t_anim <- time_anim %>% select(-vehicle.type) %>% group_by(type, cause) %>% 
  summarise(total=sum(value, na.rm=T))

theme_set(theme_bw())

g <- ggplot(data = t_anim, aes(x = cause, y = log(total), fill = type)) +
  geom_bar(data = subset(t_anim, type=="number.of.persons.injured"),
           stat = "identity") +
  geom_bar(data = subset(t_anim, type=="number.of.persons.killed"),
           stat = "identity",
           position = "identity",
           mapping = aes(y = -log(total))) +
  scale_y_continuous(labels = abs) +
  coord_flip() +
  ggtitle('Causes of Accidents')

gganimate(g,"animated_barchart.mp4")




# Date and Time Analysis ----

data$date.time <- paste(data$crash.date, data$crash.time)
data$date <- mdy(data$crash.date)
data$date.time <- mdy_hm(data$date.time)
data$time <- hm(data$crash.time)


data$day <- wday(data$date.time, label = TRUE)
data$month <- month(data$date.time)
data$hour <- hour(data$date.time)

data$month <- as.POSIXlt(data$date.time)$month
data$monthf<-factor(month(data$date.time),levels=as.character(1:12), 
                         labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), 
                    ordered=TRUE)
data$year <- year(data$date.time)

                         
# Which Day had the highest mean number of Accidents?
# Do weekends have higher accidents than week days?
data %>% group_by(borough,day)%>% summarise(n=mean(n())) %>% filter(borough!="") %>%
  ggplot(aes(x=day, y=n, fill=borough)) +
  geom_bar(position="dodge",stat = "identity")+
  #geom_text(aes(label=n), vjust=1.5, colour="black",position=position_dodge(.9), size=3)+
  ggtitle("Mean Number of Collisions Per Day")


data %>% group_by(day)%>% summarise(n=mean(n())) %>% 
  ggplot(aes(x=day, y=n)) + 
  geom_bar(position = 'dodge', stat="identity", fill="#f0902a", color="grey40") + 
  theme_minimal_hgrid() + 
  ggtitle("Mean Number of Collisions Per Day") + 
  geom_text(aes(label=n), vjust=-0.2, size=4) + 
  labs(title="Mean Number of Collisions Per Day", 
       x="", 
       y="") + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())



data$day.num <- wday(data$date.time, label = FALSE)

acc_hour <- data %>% select(borough, date, number.of.persons.injured, number.of.persons.killed, 
                            day.num, hour) %>% gather(type, value,3:5) %>% 
  group_by(borough, type, day.num, hour) %>% summarise(n=sum(value,na.rm=T))

acc_hour <- filter(acc_hour, borough!="")

hour_acc <- ggplot(acc_hour, aes(day_num, hour, fill=(n)))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Total Injury by Type",option ="C")
hour_acc <- hour_acc + facet_grid(borough~type)
hour_acc <- hour_acc + scale_y_continuous(trans = "reverse", breaks = c(0,4,8,12,16,20))
hour_acc <- hour_acc + theme_minimal(base_size = 8)
hour_acc <- hour_acc + labs(title= paste("Total Hourly Accidents and Deaths"), x="Day", y="Hour")
hour_acc <- hour_acc + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="gray"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  theme(plot.title=element_text(size=18),axis.text.x = element_text(angle=90, vjust=1))
hour_acc



# Location Wise Plot ----

nyc_map <- data %>% select(latitude, longitude, number.of.persons.killed) %>% gather(type, value,3) %>% 
  na.omit() %>% group_by(latitude, longitude, type) %>% summarise(total=sum(value, na.rm=T)) %>% 
  filter(total!=0)

nyc <- get_map("new york", zoom=12)

map_nyc <- ggmap(nyc) + 
  geom_point(data=subset(nyc_map, type=="number.of.persons,killed"), 
                            aes(x=longitude, y=latitude, colour=total), size=1, alpha=0.2) +
  ggtitle("Person Killed") + 
  scale_color_continuous(low = "red",  high = "black")


