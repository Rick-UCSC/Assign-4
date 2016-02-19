library(foreign)
library(dplyr)

RickShemanski_Assignment3 <- list(
  first =  "Rick",
  last = "Shemanski",
  email = "rshemans@ucsc.edu",
  studentid = 1504018
)


print(RickShemanski_Assignment3)

######  1

planes_df <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",stringsAsFactors = FALSE)
weather_df <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv", stringsAsFactors = FALSE)

airport_df <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv",stringsAsFactors = FALSE)
flights_df <- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv", stringsAsFactors = FALSE)

########################################################
###### 2
names(flights_df)
typeof(flights_df$date)

flights_df$date <- as.Date(flights_df$date)
typeof(flights_df$date)

names(weather_df)
weather_df$date <- as.Date(weather_df$date)
typeof(weather_df$date)

##no date
names(airport_df)

##no date
names(planes_df)

############################################################
#### 3
distinct(select(flights_df, dest))

flights.2a <- filter(flights_df, dest == "SFO" | dest == "OAK" )
print(nrow(flights.2a))

flights.2b <- flights_df%>%
  filter(dep_delay >= 1)

names(flights_df)

flights.2c <- flights_df%>%
  filter(arr_delay > 2* dep_delay)


###########################
#flights.3aa <- flights_df%>%
#  filter(dep_delay >= 1)%>%
# nrow()

#print(flights.3aa)
###########################################################

###   4

flights_df%>%
  select(contains("delay"))%>%
  head()

flights_df%>%
  select(ends_with("lay"))%>%
  head()

flights_df%>%
  select(starts_with("arr") , starts_with("dep"))%>%
  head()

################################
###     5

#    5a 
##most delayed flights
flights_df%>%
  arrange(desc(dep_delay))%>%
  head(5)

#flights_df%>%
#arrange( desc(arr_delay),dep_delay)%>%
#head(5)

## 5b
## cuaght up most time
flights_df%>%
  arrange(desc(dep_delay - arr_delay))%>%
  head(5)

################################################
#flights_df%>%    dests  with highest average delays
#arrange(dest, dep_delay)%>%
#head(25)
######################
#####          6

##  add variables spped and delta (gain flight)
## group parenthese around time and 60

flights_df <- flights_df%>%
  mutate(
    speed = dist/(time/60), 
    delta = dep_delay - arr_delay
    
  )
### if i leave early dep -10 but arrive late arr = 50, my gain is -10-50 so -60
##3 but if I leave late dep 20 but arrive early arr_delay 5 i gained 15
##

## 6a
#3 fastest spped
flights_df%>%
  arrange(desc(speed))%>%
  head()

## 6b
##gain- in delays
flights_df%>%
  arrange(desc(delta))%>%
  head()

##6c
### lost most time in delays
flights_df%>%
  arrange(delta)%>%
  head()


################################################
##     flights_new <- flights_df%>%
#arrange(carrier)       

#####         7

#### if you  group by and then arrange, it sorts with in group
by_carrier <- group_by(flights_df, carrier)

flights.7a <- by_carrier%>%
  summarise( 
    cancelled_flights = sum(cancelled, na.rm=T),
    tot_flight = (n()),
    percent_cancel = ((cancelled_flights/tot_flight)*100),
    min_delta = min(delta, na.rm =T),
    max_delta =  max(delta, na.rm =T),
    median_delta = median(delta, na.rm=T),
    mean_delta = mean(delta, na.rm = T),
    fist_quart = quantile(delta, 0.25, na.rm = T),
    third_quart = quantile(delta, 0.75, na.rm = T),
    ninety_quan = quantile(delta, 0.90, na.rm = T)
    
  )
#test  
#nrow(filter(by_carrier, carrier =="AA"))

flights.7a%>%
  arrange(desc(percent_cancel))%>%
  print()

### 7b

cat(
  " The code below filters the flights data so to remove all na's in 
  dep_delay attribute. Now rows that previously had na's in the dep_delay
  column are removed, we group the data by date, and find the average dep_delay
  for each date, and we find actual number of delays for each date = n.After
  we only assing these values to day_delay if the n is greater than 10."
  
)


## change using %>%

day_delay <- dplyr::filter(flights_df, !is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n=n()
    
  )

#############################################33
### 8

day_delay <- dplyr::filter(flights_df, !is.na(dep_delay))%>%
  group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n=n(),
    delay_lag = lag(delay,1),
    delay_diff = delay - delay_lag
  )

day_delay%>%
  arrange(desc(delay_diff))%>%
  head()

##########################################################################
###                9

dest_delay <- dplyr::filter(flights_df, !is.na(arr_delay))%>%
  group_by(dest)%>%
  summarize(
    arr_delay = mean(arr_delay),
    n = n()
  )

airport_df <- airport_df %>%
  rename(dest=iata, name=airport)
names(airport_df)

df.9a <- left_join(dest_delay, airport_df, by=c("dest"="dest"))

df.9a%>%
  arrange(desc(arr_delay))%>%
  head()

df.9b <- inner_join(dest_delay, airport_df, by=c("dest"="dest"))
nrow(df.9a)
nrow(df.9b)

print("The number of observations do not match.")

df.9c <- right_join(dest_delay, airport_df, by=c("dest"="dest"))
nrow(df.9c)
cat("Because there are more observations in airports than in dest_delay, 
    right joining fills the first data set (dest_delay) in order to make them equal")

df.9d <- full_join(dest_delay, airport_df, by=c("dest"="dest"))
nrow(df.9d)
print("Because airports has a different number of observations, both datasets are now fully represented")


###########################################################
###               10

hourly_delay <- dplyr::filter(flights_df, !is.na(dep_delay))%>%
  group_by(date, hour)%>%
  summarise(
    avg_hour_delay = mean(dep_delay)
  )

hourly_delay$date <- as.Date(hourly_delay$date)

df.10a <- left_join(hourly_delay, weather_df, by=c("date"="date"))

names(df.10a)


##   10.a 
### conditoins 
by_conditions <- group_by(df.10a, conditions)
table10.fa <- by_conditions%>%
  summarise(
    sum_delay = sum(avg_hour_delay, na.rm = T)
    
  )

arrange(table10.fa, desc(sum_delay))%>%
  print()


