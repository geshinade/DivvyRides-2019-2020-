
## This analysis is based on the case study "Sophisticated, Clear, and polished': 
## Divvy and data Visualization" written by Kevin Hartman (found here:
## https://artscience.blog/home/divvy-dataviz-case-study). The purpose of 
## this analysis is to try to answer the question: "In What ways do members
## and casual riders use Divvy bikes differently?"

# STEP 1:

# install required packges
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for vizualization


library(tidyverse)
library(lubridate)


## Collect the previous 12 months of divvy bike_trips data


q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

## Examine each dataset
View(q2_2019)
View(q3_2019)
View(q4_2019)
View(q1_2020)

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

# Compare column names in each of the files
# While the names don't have to be in the same order, they do need to match 
# perfectly before we can use a command to join them into one file
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

# Rename columns to make them consistent with q1_2020 (as this is the latest 
# dataset and possibly the table desin for divvy 


(q4_2019 <- rename(q4_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

(q3_2019 <- rename(q3_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,
                   start_station_name = from_station_name,
                   start_station_id = from_station_id,
                   end_station_name = to_station_name,
                   end_station_id = to_station_id,
                   member_casual = usertype))

(q2_2019 <- rename(q2_2019,
                   ride_id = "01 - Rental Details Rental ID",
                   rideable_type = "01 - Rental Details Bike ID",
                   started_at = "01 - Rental Details Local Start Time",
                   ended_at = "01 - Rental Details Local End Time",
                   start_station_name = "03 - Rental Start Station Name",
                   start_station_id = "03 - Rental Start Station ID",
                   end_station_name = "02 - Rental End Station Name",
                   end_station_id = "02 - Rental End Station ID",
                   member_casual = "User Type"))

# Inspect the dataframes and look for inconsistencies
glimpse(q1_2020)
glimpse(q4_2019)
glimpse(q3_2019)
glimpse(q2_2019)

# Convert ride_id and rideable_type to character to align the data types
q4_2019 <- mutate(q4_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q3_2019,
                  ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# Combine all the quaters into one dataframe
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
View(all_trips)

# Remove lat, lng, end_lat, end_lng, birthyear, gender fields as this 
# data was dropped in 2020
all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, gender))
View(all_trips)

# STEP3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

# Inspect the new table that has been created
colnames(all_trips) # List of column names
nrow(all_trips) # How many rows are there in the data frame>
dim(all_trips) # Dimensions of the data frame?
glimpse(all_trips) # See list of columns and data types(numeric, character, etc)
summary(all_trips) # Statistical summary of data. Mainly for numerics


# There are a few problems we need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" 
# and Subscriber") and two names for casual riders ("Customer" and "casual").
# These are recoded from four to two labels
# (2) For additional opportunities to aggregate the data, 
# there is a need to add some additional columns of data, namely day, month, year
# (3) We will want to add calculated field for length of ride since the 2020Q1
# did not have the "tripduration" column. We will add "ride_length" to the entire data 
# frame for consistency.
# (4) There are some rides where tripduration shows up as negative, including 
# several hundred rides where Divvy took bikes out of circulation for Quality
# Control reasons. We will want to delete these rides

# In the "member_casual" column, replace "Subscriber" with "member' and "Customer"
# with " casual"
# Before 2020, Divvy used different labels for these types of riders ... 
# We will want to make our dataframe consistent with their current nomenclature.
# N.B.: "Level" is a special property of a column that is retained even if a 
# subset does not contain any values from a specific level
# Begin by seeing how many observations fall under usertype
table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))
# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride.
# this will allow us to aggregate ride data for each month, day, or year ..
# before completing these operations we could only aggregate at the ride level
all_trips$date <-as.Date(all_trips$started_at) # The default format is yyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips(in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

# To eliminate redundancy, drop the tripduration column as it has been replaced by 
# ride_length
all_trips$tripduration <- NULL

# Inspect the structure of the columns
glimpse(all_trips)

# Convert "ride_length" from factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks
# and checked for quality by Divvy or ride_length was negative
# We wil create a new version of the dataframe(v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == 
                              "HQ QR" | all_trips$ride_length<0),]
glimpse(all_trips_v2)

View(all_trips_v2)

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

# Describe the spread, centrality and variance of ride_length 
# (all figures in seconds)

all_trips_v2 %>% 
  select(ride_length) %>% 
  summary

# Compare members and casual users relative to ride_length
all_trips_v2 %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(ride_length), 
            median = median(ride_length), max = max(ride_length),
            min = min(ride_length))


# See the average ride by each day for  members vs casual riders
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(mean = mean(ride_length))


# Notice that the days of the week are out of order. Let's fix that
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels = c("Sunday","Monday","Tuesday",
                                               "Wednesday", "Thursday", "Friday",
                                               "Saturday"))

# Now let's run the average ride time by each day for members vs casual users again
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(mean = mean(ride_length))


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # Creates weekday field using wday()
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday)

STEP 5: DATA VIZUALIZATION

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% # Creates weekday field using wday()
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual))+
               geom_col(position = "dodge")

## The chart suggest that a larger number of annual members ride through the week than 
# the casual riders.  But there seems to be a discernible pattern: While riding 
# tends to slightly decline from Tuesday through Sunday, the opposite is the case
# for casual riders. If demographic data had been available, 
# we might have been able to examine this discrepancy further.

# Let's create visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% # Creates weekday field using wday()
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual))+
  geom_col(position = "dodge")

## the chart of average duration tells a different story. In terms of number, the 
## annual members exceed the casual riders. But not so in terms of average length 
## of ride, which seems a good sign for the business task, 
## namely influencing casual riders to become members. The average 
## ride_length for casual riders was far higher than that 
## of annual members on a daily basis

STEP 6: CONCLUSION and RECOMMENDATION

## From the results above, it is quite clear that the annual members and casual 
## riders use divvy bikes differently. While the annual members are more than 
## casual riders, the higher average duration for casual riders suggest that with 
## the right motivation, they can be influenced to become (annual) members.

















