######### Load all the data using readr library ######### 

library(readr)
data_2020_04 <- read_delim("202004-divvy-tripdata/202004-divvy-tripdata.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
data_2020_05 <- read_delim("202005-divvy-tripdata/202005-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_06 <- read_delim("202006-divvy-tripdata/202006-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_07 <- read_delim("202007-divvy-tripdata/202007-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_08 <- read_delim("202008-divvy-tripdata/202008-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_09 <- read_delim("202009-divvy-tripdata/202009-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_10 <- read_delim("202010-divvy-tripdata/202010-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_11 <- read_delim("202011-divvy-tripdata/202011-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2020_12 <- read_delim("202012-divvy-tripdata/202012-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2021_01 <- read_delim("202101-divvy-tripdata/202101-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2021_02 <- read_delim("202102-divvy-tripdata/202102-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
data_2021_03 <- read_delim("202103-divvy-tripdata/202103-divvy-tripdata.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)

######### Merge all the data into a single data set ######### 

#Before merging the data, I need to perform a transformation phase.
#In the data from 05 2020 to 11 2020, the columns ( start_station_id, end_station_id) 
#are of type double while the data from 12 2020 to 03 2021, the columns ( start_station_id, end_station_id) 
#are of type character because these columns contain characters.
#For coherent data we will transform the columns ( start_station_id, end_station_id) 
#between 04 2020 and 11 2020 into character type.

library(tidyverse)
data_2020_04_11_dou <- bind_rows(data_2020_05,data_2020_06,data_2020_07,data_2020_08,data_2020_09,data_2020_10,data_2020_11)
data_2020_04_11_char <- mutate(data_2020_04_11_dou, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
merged_data <- bind_rows(data_2020_04_11_char,data_2020_12,data_2021_01,data_2021_02,data_2021_03)

######### summarize the data ######### 

head(merged_data)
colnames(merged_data)
str(merged_data)
glimpse(merged_data)
summary(merged_data)

######### Data cleaning #########

###calculate the difference between the started and the ended station
merged_data$ride_length <- difftime(merged_data$ended_at,merged_data$started_at)

#Delete blank rows in the data
merged_data_clean <- drop_na(merged_data)
summary(merged_data_clean)

#merged_data_clean$started_at <- as.Date(merged_data_clean$started_at)

#Create new columns 
merged_data_clean1 <- transform(merged_data_clean, dates_ = format(started_at, "%d"), 
          months_ = format(started_at, "%m"), years_ = format(started_at, "%Y"), 
          Weekdays_ = format(started_at, "%A"))
merged_data_clean1 <- transform(merged_data_clean1,hours_ = format(started_at, "%H"))
View(merged_data_clean1)

#Create a new data frame without records that have ride length <= zero minute OR > 24 h 
#Removes inconsistent values 

merged_data_clean1 <- merged_data_clean1[!(merged_data_clean1$ride_length <= 30 | merged_data_clean1$ride_length > 86400),]

######### Analysis #########

ride_lenght_summarise <- merged_data_clean1 %>%  
  summarise(min_ride_lenght = min(ride_length),max_ride_lenght = max(ride_length),mean_ride_lenght = mean(ride_length),
            median_ride_lenght = median(ride_length) )
View(ride_lenght_summarise)

member_casual_mean_ride_length <- aggregate(merged_data_clean1$ride_length ~ merged_data_clean1$member_casual, FUN = mean )
member_casual_median_ride_length <- aggregate(merged_data_clean1$ride_length ~ merged_data_clean1$member_casual, FUN = median )
member_casual_min_ride_length <- aggregate(merged_data_clean1$ride_length ~ merged_data_clean1$member_casual, FUN = min )
member_casual_max_ride_length <- aggregate(merged_data_clean1$ride_length ~ merged_data_clean1$member_casual, FUN = max )
View(member_casual_mean_ride_length, member_casual_median_ride_length)

merged_data_clean1<- merged_data_clean1 %>% 
  mutate(Weekdays_=ifelse(Weekdays_=="lundi","Monday",Weekdays_),Weekdays_=ifelse(Weekdays_=="mardi","Tuesday",Weekdays_),
         Weekdays_=ifelse(Weekdays_=="mercredi","Wednesday",Weekdays_),
         Weekdays_=ifelse(Weekdays_=="jeudi","Thursday",Weekdays_), Weekdays_=ifelse(Weekdays_=="vendredi","Friday",Weekdays_),
         Weekdays_=ifelse(Weekdays_=="samedi","Saturday",Weekdays_), Weekdays_=ifelse(Weekdays_=="dimanche","Sunday",Weekdays_))
View(merged_data_clean1)

agg_member_ride_week_mean<-aggregate(merged_data_clean1$ride_length ~merged_data_clean1$member_casual + merged_data_clean1$Weekdays_, FUN =mean)
View(agg_member_ride_week_mean)

agg_member_ride_week_median <-aggregate(merged_data_clean1$ride_length ~merged_data_clean1$member_casual + merged_data_clean1$Weekdays_, FUN =median)
View(agg_member_ride_week_mean)

###

merber_casual_week_sumarise_n <- merged_data_clean1 %>% 
  group_by(member_casual, Weekdays_) %>% 
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(Weekdays_)

merber_casual_month_sumarise_median <- merged_data_clean1 %>%
  group_by(member_casual, months_) %>%
  summarise(average_ride_length = median(ride_length), .groups = 'drop') %>%
  arrange(months_)

merber_casual_month_sumarise_n <- merged_data_clean1 %>%
  group_by(member_casual, months_) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  arrange(months_)

merber_casual_rideable_sumarise_n <- merged_data_clean1 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(number_of_ride = n(), .groups = 'drop')

merber_casual_rideable_sumarise_n_b <- merged_data_clean1 %>% 
  filter(rideable_type == 'docked_bike') %>% 
  group_by(member_casual, Weekdays_) %>% 
  summarise(number_of_ride= n(),.groups = 'drop') %>%
  arrange(Weekdays_)

merber_casual_start_station_sumarise_n <- merged_data_clean1 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_ride = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual != 'member') %>%
  arrange(-number_of_ride) %>% 
  head(n=20)

member_casual_hours_summarise <- merged_data_clean1 %>% 
  group_by( member_casual,hours_) %>% 
  summarise(number_ride = n(), .groups = 'drop')
View(member_casual_hours_summarise) 


######### Share ########"


#position = "stack"
ggplot(data = merber_casual_week_sumarise_n, aes(x= Weekdays_, y= number_of_ride, fill=member_casual ))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~member_casual)+
  scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = "The number of rides per day of the week for casual and members riders.")

# In this graph, we can see that member cyclists have a stable number of rides 
# while occasional cyclists generally ride more on weekends.

ggplot(data = merber_casual_month_sumarise_median, aes(x=months_, y = average_ride_length, fill = member_casual))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~ member_casual)
#Dans ce plot on peut voir que c'est entre le mois de mai et le mois aout q'on a le plus de courses 

ggplot(data = merber_casual_month_sumarise_n, aes(x=months_, y = number_of_ride, fill = member_casual))+
  geom_bar(position = "dodge", stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))+
  facet_wrap(~member_casual)
# In this graph, we can see that members and occasional users make more trips between June and September.
  
ggplot(data = merber_casual_rideable_sumarise_n, aes(x=rideable_type, y = number_of_ride, fill = member_casual))+
  geom_bar(position = "dodge", stat = "identity")
# in this graph we can see that casual like more docked bike than the other

ggplot(data = merber_casual_start_station_sumarise_n, aes(x=start_station_name, y = number_of_ride, fill = member_casual))+
  geom_bar(position = "dodge", stat = "identity")+
  theme(axis.text.x = element_text(angle = 45))
# In this graph, we can see the 20 most popular stations for casual cyclists.

ggplot(data=member_casual_hours_summarise, aes(x=hours_, y= number_ride, fill= member_casual))+
  geom_bar(position = 'dodge', stat = 'identity')+
  facet_wrap(~member_casual)+
  theme(axis.text.x = element_text(angle = 45) )
# In this plot we can see that casual cycliste have more ride between 11 am and 19 PM
