# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman 
# found here: https://artscience.blog/home/divvy-dataviz-case-study. 
# The purpose of this script is to consolidate downloaded Divvy data into a single dataframe. 
# conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

#Installed required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualisation

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(lubridate)

# set the working directory
s
etwd("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder")
getwd()

# STEP 1: COLLECT DATA

q1_2021<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_01_2022-03-02/202101_ext.csv")
q2_2021<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_02_2022-03-02/202102_ext.csv")
q3_2021<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_03_2022-03-02/202103_ext.csv")
q4_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_04_2022-03-02/202004_ext.csv")
q5_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_05_2022-03-02/202005_ext.csv")
q6_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_06_2022-03-02/202006_ext.csv")
q7_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_07_2022-03-02/202007_ext.csv")
q8_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_08_2022-03-02/202008_ext.csv")
q9_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_09_2022-03-02/202009_ext.csv")
q10_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_10_2022-03-02/202010_ext.csv")
q11_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_11_2022-03-02/202011_ext.csv")
q12_2020<- read_csv("C:/Users/Raghavi/Desktop/cyclistic_2022/CSV Subfolder/Cyclist_12_2022-03-02/202012_ext.csv")

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE

# Compare column names each of the files
colnames(q1_2021)
colnames(q2_2021)
colnames(q3_2021)
colnames(q4_2020)
colnames(q5_2020)

# Rename columns  to make them consistent with q1_2020

(q1_2020 <- rename(q1_2020
                    ,trip_id = ride_id
                    ,bikeid = rideable_type 
                    ,start_time = started_at 
                    ,end_time = ended_at  
                    ,from_station_name = start_station_name 
                    ,from_station_id = start_station_id 
                    ,to_station_name = end_station_name 
                    ,to_station_id = end_station_id
                     ))
                    
                   
				 
 str(q2_2021)

# Convert trip_id and bikeid to character so that they can stack correctly

(q1_2021 <-  mutate(q1_2021, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)))
(q2_2021 <-  mutate(q2_2021, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)))
(q3_2021 <-  mutate(q3_2021, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)))
(q4_2020 <-  mutate(q4_2020, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)))
(q5_2020 <-  mutate(q5_2020, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)))
(q6_2020 <-  mutate(q6_2020, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid)))


## mutated few more columns to make consistent
 q1_2021 <-  mutate(q1_2021, from_station_id = as.character(from_station_id)
                              ,to_station_id = as.character(to_station_id)
                              ,start_time= as.POSIXct(start_time, format= "%m/%d/%Y %H:%M")
                              ,end_time= as.POSIXct(end_time, format= "%m/%d/%Y %H:%M"))
 

## Stack individual quarter's data frames into one big data frame

all_trips <- bind_rows(q1_2021, q2_2021,q3_2021, q4_2020, q5_2020,q6_2020,q7_2020,q8_2020,q9_2020,q10_2020,q11_2020,q12_2020)

##removed unnecessary columns

all_trips <- all_trips %>%
select(-c("ride_length...9","day_of_week...10"))

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS

#to view the the data type and column names
 
 colnames(all_trips)
 [1] "trip_id"           "bikeid"            "start_time"        "end_time"         
 [5] "from_station_name" "from_station_id"   "to_station_name"   "to_station_id"    
 [9] "member_casual"     "day_of_week"      

str(all_trips)
tibble [3,489,748 x 10] (S3: tbl_df/tbl/data.frame)
 $ trip_id          : chr [1:3489748] "E19E6F1B8D4C42ED" "DC88F20C2C55F27F" "EC45C94683FE3F27" "4FA453A75AE377DB" ...
 $ bikeid           : chr [1:3489748] "electric_bike" "electric_bike" "electric_bike" "electric_bike" ...
 $ start_time       : POSIXct[1:3489748], format: "2021-01-23 16:14:00" "2021-01-27 18:43:00" ...
 $ end_time         : POSIXct[1:3489748], format: "2021-01-23 16:24:00" "2021-01-27 18:47:00" ...
 $ from_station_name: chr [1:3489748] "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" ...
 $ from_station_id  : chr [1:3489748] "17660" "17660" "17660" "17660" ...
 $ to_station_name  : chr [1:3489748] "N/A" "N/A" "N/A" "N/A" ...
 $ to_station_id    : chr [1:3489748] "N/A" "N/A" "N/A" "N/A" ...
 $ member_casual    : chr [1:3489748] "member" "member" "member" "member" ...
 $ day_of_week      : num [1:3489748] 7 4 5 5 7 7 2 5 7 1 ...

#total no of rows
nrow(all_trips)
 3489748

#dimensions of the data frame
dim(all_trips) 
3489748      10

#to see the first 6 rows of data frame
head(all_trips)
# A tibble: 6 x 10
  trip_id  bikeid start_time          end_time            from_station_na~ from_station_id
  <chr>    <chr>  <dttm>              <dttm>              <chr>            <chr>          
1 E19E6F1~ elect~ 2021-01-23 16:14:00 2021-01-23 16:24:00 California Ave ~ 17660          
2 DC88F20~ elect~ 2021-01-27 18:43:00 2021-01-27 18:47:00 California Ave ~ 17660          
3 EC45C94~ elect~ 2021-01-21 22:35:00 2021-01-21 22:37:00 California Ave ~ 17660          
4 4FA453A~ elect~ 2021-01-07 13:31:00 2021-01-07 13:42:00 California Ave ~ 17660          
5 BE5E8EB~ elect~ 2021-01-23 02:24:00 2021-01-23 02:24:00 California Ave ~ 17660          
6 5D8969F~ elect~ 2021-01-09 14:24:00 2021-01-09 15:17:00 California Ave ~ 17660          
# ... with 4 more variables: to_station_name <chr>, to_station_id <chr>,
#   member_casual <chr>, day_of_week <dbl>

glimpse(all_trips)
Rows: 3,489,748
Columns: 10
$ trip_id           <chr> "E19E6F1B8D4C42ED", "DC88F20C2C55F27F", "EC45C94683FE3F27", "4~
$ bikeid            <chr> "electric_bike", "electric_bike", "electric_bike", "electric_b~
$ start_time        <dttm> 2021-01-23 16:14:00, 2021-01-27 18:43:00, 2021-01-21 22:35:00~
$ end_time          <dttm> 2021-01-23 16:24:00, 2021-01-27 18:47:00, 2021-01-21 22:37:00~
$ from_station_name <chr> "California Ave & Cortez St", "California Ave & Cortez St", "C~
$ from_station_id   <chr> "17660", "17660", "17660", "17660", "17660", "17660", "17660",~
$ to_station_name   <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A",~
$ to_station_id     <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N/A",~
$ member_casual     <chr> "member", "member", "member", "member", "casual", "casual", "m~
$ day_of_week       <dbl> 7, 4, 5, 5, 7, 7, 2, 5, 7, 1, 7, 7, 7, 1, 6, 3, 7, 4, 6, 1, 2,~


#to remove na from dataframe
colSums(is.na(all_trips))
 trip_id            bikeid        start_time          end_time from_station_name 
                0                 0                 0                 0            122175 
  from_station_id   to_station_name     to_station_id     member_casual       day_of_week 
           122801            132965            133426            343005            343005 

#to remove missing values
all_trips_clean <- all_trips[complete.cases(all_trips), ]

#to remove "N/A" from dataframe
colSums(is.na(all_trips_clean))
all_trips_clean<-all_trips[!(all_trips$to_station_name=="N/A" | all_trips$to_station_id=="N/A"),]


#filtering start_time data that is greater than end_time
all_trips_clean <- all_trips_clean %>% 
  filter(all_trips_clean$start_time < all_trips_clean$end_time)

#create new columns date,month, year for each ride
all_trips_clean$date <- as.Date(all_trips_clean$start_time, format= "%m/%d/%Y %H:%M")
all_trips_clean$month <- format(as.Date(all_trips_clean$date), "%m")
all_trips_clean$day <- format(as.Date(all_trips_clean$date), "%d")
all_trips_clean$year <- format(as.Date(all_trips_clean$date), "%Y")
all_trips_clean$day_of_week <- format(as.Date(all_trips_clean$date), "%A")

str(all_trips_clean)
tibble [2,952,154 x 14] (S3: tbl_df/tbl/data.frame)
 $ trip_id          : chr [1:2952154] "B9F73448DFBE0D45" "457C7F4B5D3DA135" "57C750326F9FDABE" "4D518C65E338D070" ...
 $ bikeid           : chr [1:2952154] "classic_bike" "electric_bike" "electric_bike" "electric_bike" ...
 $ start_time       : POSIXct[1:2952154], format: "2021-01-24 19:15:00" "2021-01-23 12:57:00" ...
 $ end_time         : POSIXct[1:2952154], format: "2021-01-24 19:22:00" "2021-01-23 13:02:00" ...
 $ from_station_name: chr [1:2952154] "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" ...
 $ from_station_id  : chr [1:2952154] "17660" "17660" "17660" "17660" ...
 $ to_station_name  : chr [1:2952154] "Wood St & Augusta Blvd" "California Ave & North Ave" "Wood St & Augusta Blvd" "Wood St & Augusta Blvd" ...
 $ to_station_id    : chr [1:2952154] "657" "13258" "657" "657" ...
 $ member_casual    : chr [1:2952154] "member" "member" "casual" "casual" ...
 $ day_of_week      : chr [1:2952154] "Monday" "Saturday" "Saturday" "Saturday" ...
 $ date             : Date[1:2952154], format: "2021-01-25" "2021-01-23" ...
 $ month            : chr [1:2952154] "01" "01" "01" "01" ...
 $ day              : chr [1:2952154] "25" "23" "09" "09" ...
 $ year             : chr [1:2952154] "2021" "2021" "2021" "2021" ...
> 

glimpse(all_trips_clean)
#to view the first 6 rows
 head(all_trips_clean)
# A tibble: 6 x 14
  trip_id  bikeid start_time          end_time            from_station_na~ from_station_id
  <chr>    <chr>  <dttm>              <dttm>              <chr>            <chr>          
1 B9F7344~ class~ 2021-01-24 19:15:00 2021-01-24 19:22:00 California Ave ~ 17660          
2 457C7F4~ elect~ 2021-01-23 12:57:00 2021-01-23 13:02:00 California Ave ~ 17660          
3 57C7503~ elect~ 2021-01-09 15:28:00 2021-01-09 15:37:00 California Ave ~ 17660          
4 4D518C6~ elect~ 2021-01-09 15:28:00 2021-01-09 15:37:00 California Ave ~ 17660          
5 9D08A3A~ class~ 2021-01-24 15:56:00 2021-01-24 16:07:00 California Ave ~ 17660          
6 49FCE1F~ elect~ 2021-01-22 15:15:00 2021-01-22 15:36:00 California Ave ~ 17660          
# ... with 8 more variables: to_station_name <chr>, to_station_id <chr>,
#   member_casual <chr>, day_of_week <chr>, date <date>, month <chr>, day <chr>,
#   year <chr>

#to view the last 6 rows
 tail(all_trips_clean)

#Final summary of the data frame
 
summary(all_trips_clean)

   trip_id             bikeid            start_time                 
 Length:2952154     Length:2952154     Min.   :2020-04-01 00:00:00  
 Class :character   Class :character   1st Qu.:2020-07-26 15:00:00  
 Mode  :character   Mode  :character   Median :2020-09-03 10:18:30  
                                       Mean   :2020-09-15 11:20:05  
                                       3rd Qu.:2020-10-23 02:44:00  
                                       Max.   :2021-03-31 23:59:00  
    end_time                   from_station_name  from_station_id    to_station_name   
 Min.   :2020-04-01 00:10:00   Length:2952154     Length:2952154     Length:2952154    
 1st Qu.:2020-07-26 15:40:00   Class :character   Class :character   Class :character  
 Median :2020-09-03 10:40:00   Mode  :character   Mode  :character   Mode  :character  
 Mean   :2020-09-15 11:44:22                                                           
 3rd Qu.:2020-10-23 04:07:30                                                           
 Max.   :2021-04-06 11:00:00                                                           
 to_station_id      member_casual      day_of_week             date           
 Length:2952154     Length:2952154     Length:2952154     Min.   :2020-04-01  
 Class :character   Class :character   Class :character   1st Qu.:2020-07-26  
 Mode  :character   Mode  :character   Mode  :character   Median :2020-09-03  
                                                          Mean   :2020-09-15  
                                                          3rd Qu.:2020-10-23  
                                                          Max.   :2021-04-01  
    month               day                year          
 Length:2952154     Length:2952154     Length:2952154    
 Class :character   Class :character   Class :character  
 Mode  :character   Mode  :character   Mode  :character  
  

#dimensions of the Dataframe

dim(all_trips_clean)
[1] 2952154      14

glimpse(all_trips_clean)
Rows: 2,952,154
Columns: 14
$ trip_id           <chr> "B9F73448DFBE0D45", "457C7F4B5D3DA135", "57C750326F9FDABE", "4~
$ bikeid            <chr> "classic_bike", "electric_bike", "electric_bike", "electric_bi~
$ start_time        <dttm> 2021-01-24 19:15:00, 2021-01-23 12:57:00, 2021-01-09 15:28:00~
$ end_time          <dttm> 2021-01-24 19:22:00, 2021-01-23 13:02:00, 2021-01-09 15:37:00~
$ from_station_name <chr> "California Ave & Cortez St", "California Ave & Cortez St", "C~
$ from_station_id   <chr> "17660", "17660", "17660", "17660", "17660", "17660", "17660",~
$ to_station_name   <chr> "Wood St & Augusta Blvd", "California Ave & North Ave", "Wood ~
$ to_station_id     <chr> "657", "13258", "657", "657", "657", "KA1504000135", "KA150400~
$ member_casual     <chr> "member", "member", "casual", "casual", "casual", "member", "m~
$ day_of_week       <chr> "Monday", "Saturday", "Saturday", "Saturday", "Sunday", "Frida~
$ date              <date> 2021-01-25, 2021-01-23, 2021-01-09, 2021-01-09, 2021-01-24, 2~
$ month             <chr> "01", "01", "01", "01", "01", "01", "01", "01", "01", "01", "0~
$ day               <chr> "25", "23", "09", "09", "24", "22", "05", "30", "27", "15", "1~
$ year              <chr> "2021", "2021", "2021", "2021", "2021", "2021", "2021", "2021"~
 
#to remove "na" from dataframe
colSums(is.na(all_trips_clean))
all_trips_clean<-all_trips[!(all_trips$to_station_name=="N/A" | all_trips$to_station_id=="N/A"),]


#add new column to calculate each ride length in mins
all_trips_clean$ride_length <- difftime(all_trips_clean$end_time, 
                                                     all_trips_clean$start_time)
#inspect the structure of the columns
str(all_trips_clean)
tibble [2,952,154 x 15] (S3: tbl_df/tbl/data.frame)
 $ trip_id          : chr [1:2952154] "B9F73448DFBE0D45" "457C7F4B5D3DA135" "57C750326F9FDABE" "4D518C65E338D070" ...
 $ bikeid           : chr [1:2952154] "classic_bike" "electric_bike" "electric_bike" "electric_bike" ...
 $ start_time       : POSIXct[1:2952154], format: "2021-01-24 19:15:00" "2021-01-23 12:57:00" ...
 $ end_time         : POSIXct[1:2952154], format: "2021-01-24 19:22:00" "2021-01-23 13:02:00" ...
 $ from_station_name: chr [1:2952154] "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" "California Ave & Cortez St" ...
 $ from_station_id  : chr [1:2952154] "17660" "17660" "17660" "17660" ...
 $ to_station_name  : chr [1:2952154] "Wood St & Augusta Blvd" "California Ave & North Ave" "Wood St & Augusta Blvd" "Wood St & Augusta Blvd" ...
 $ to_station_id    : chr [1:2952154] "657" "13258" "657" "657" ...
 $ member_casual    : chr [1:2952154] "member" "member" "casual" "casual" ...
 $ day_of_week      : chr [1:2952154] "Monday" "Saturday" "Saturday" "Saturday" ...
 $ date             : Date[1:2952154], format: "2021-01-25" "2021-01-23" ...
 $ month            : chr [1:2952154] "01" "01" "01" "01" ...
 $ day              : chr [1:2952154] "25" "23" "09" "09" ...
 $ year             : chr [1:2952154] "2021" "2021" "2021" "2021" ...
 $ ride_length      : 'difftime' num [1:2952154] 420 300 540 540 ...
  ..- attr(*, "units")= chr "secs"

#convert “ride_length” from Factor to numeric so we can run calculations on the data
is.factor(all_trips_cleaned$ride_length)
FALSE
all_trips_clean$ride_length <- as.nemeric(as.character(all_trips_clean$ride_length))
is.numeric(all_trips_clean$ride_length)
TRUE

#remove “bad” data and store in a new dataframe
all_trips_v2 <- all_trips_clean[!(all_trips_clean$from_station_name == "HQ QR" | all_trips_clean$ride_length<0),]

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS

Descriptive analysis on ride_length (all figures in minutes)
mean(all_trips_v2$ride_length)

1642.642

median(all_trips_v2$ride_length)

##Compare members and casual users

> aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
  all_trips_v2$member_casual all_trips_v2$ride_length
1                     casual                2677.4437
2                     member                 936.7666
> aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
  all_trips_v2$member_casual all_trips_v2$ride_length
1                     casual                     1260
2                     member                      660
> aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
  all_trips_v2$member_casual all_trips_v2$ride_length
1                     casual                     1260
2                     member                      660
> aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
  all_trips_v2$member_casual all_trips_v2$ride_length
1                     casual                        0
2                     member                        0

#See the average ride time by each day for members vs casual users
#arranging the days of the week accordingly
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
   all_trips_v2$member_casual all_trips_v2$day_of_week all_trips_v2$ride_length
1                      casual                   Sunday                2961.7973
2                      member                   Sunday                1057.7037
3                      casual                   Monday                2865.5991
4                      member                   Monday                 926.2690
5                      casual                  Tuesday                2562.0612
6                      member                  Tuesday                 880.1240
7                      casual                Wednesday                2348.7857
8                      member                Wednesday                 871.1152
9                      casual                 Thursday                2412.1195
10                     member                 Thursday                 885.0432
11                     casual                   Friday                2499.4283
12                     member                   Friday                 911.9893
13                     casual                 Saturday                2773.8096
14                     member                 Saturday                1023.3728

#Analyze ridership data by type and weekday
 all_trips_v2 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()                             
                  ,average_duration = mean(ride_length)) %>%         
  arrange(member_casual, weekday) 

`summarise()` has grouped output by 'member_casual'. You can override using the `.groups`
argument.
# A tibble: 14 x 4
# Groups:   member_casual [2]
   member_casual weekday number_of_rides average_duration
   <chr>         <ord>             <int>            <dbl>
 1 casual        Sun              217392            3025.
 2 casual        Mon              127902            2681.
 3 casual        Tue              117900            2411.
 4 casual        Wed              132670            2420.
 5 casual        Thu              135509            2470.
 6 casual        Fri              178040            2559.
 7 casual        Sat              286443            2813.
 8 member        Sun              221052            1071.
 9 member        Mon              229934             894.
10 member        Tue              241511             880.
11 member        Wed              265385             878.
12 member        Thu              256124             880.
13 member        Fri              265028             920.
14 member        Sat              274071            1042.

# Visualise for number of rides grouped by rider type
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Day: Members vs. Casual Riders")

# create a visualization for average duration

all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(ride_length), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Day of Week", y = "Average Duration (min)", 
       fill = "Member/Casual",
       title = "Average Riding Duration by Day: Members vs. Casual Riders")

 
#Average Number of Rides by Month
all_trips_v2 %>% 
  group_by(month, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'casual') %>%
  drop_na() %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: Casual Riders")

#Visual for Top 10 Used Stations by Casual Members
ggplot(data = top_10_station_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "lightsalmon") +
  labs(title = "Top 10 Used Stations by Casual Riders", x = "", y = "Number of Rides") + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() 

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Divvy_Exercise/avg_ride_length.csv')

# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Divvy_Exercise/avg_ride_length.csv')

