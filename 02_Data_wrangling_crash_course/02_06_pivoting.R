# 2 Data wrangling crash course

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("hflights") # dataset
install.packages("dplyr")
install.packages("tidyr")
install.packages("tibble") # convert table to tibble

library(hflights)
library(dplyr)
library(tidyr)
library(tibble)

# First peek into data
help("hflights")

df <- tibble::as_tibble(hflights) # convert dataframe to tibble
View(df) # check table
print(df) # print to console
str(df) # table structure
nrow(df); ncol(df) # rows ~ cols



# 2.1 Variables manipulation

# select() - Columns selection

#   Extract columns Year, Month, DayofMonth
select(df, Year, Month, DayofMonth)
df.date <- select(df, Year, Month, DayofMonth)
rm(df.date)

#   Extract columns that begin with "Taxi"
select(df, starts_with("Taxi"))

#   Extract columns that contain "Time"
select(df, ends_with("Time"))


# mutate() - New variables creation

#   Convert time in minutes into hours, columns: ActualElapsedTime, AirTime, ArrDelay, DepDelay
df <- mutate(df, 
             `ActualElapsedTime hours` = round(ActualElapsedTime / 60, 2),
             `AirTime hours` = round(AirTime / 60, 2),
             `ArrDelay hours` = round(ArrDelay / 60, 2),
             `DepDelay hours` = round(DepDelay / 60, 2))


# rename()

#   Rename column Dest to Destination
df <- rename(df, Destination = Dest)



# 2.2 Cases manipulation

# filter() - Extract rows

#   Extract rows where DayOfWeek is 7 (Sunday)
df.sunday <- filter(df, DayOfWeek == 7)

#   Extract rows where carrier is "American Airlines" (AA) and flight distance is at least 250 miles
df.AA.250mil <- filter(df, UniqueCarrier == "AA" & Distance >= 250)


# distinct() - Extract distinct rows f

#   Extract distinct rows for columns Year, Month
distinct(select(df, Year, Month))

#   Extract distinct carriers 
distinct(select(df, UniqueCarrier))


# sample_n() - Extract n randomly selected rows

#   Extract 10 randomly selected rows from table
df.10 <- sample_n(df, size = 10, replace = F)


# arrange() - Sort rows

#   Sort rows by Distance
df <- arrange(df, Distance)

#   Sort rows by Year, Month, DayofMonth in reverse order
df <- arrange(df, desc(Year), desc(Month), desc(DayofMonth))



# 2.3 summarise & group

# summarise() - summarise data

#   Calculate mean flight distance
summarise(df, 
          `mean distance` = mean(Distance))

#   Calculate min, max, mean, median AirTime
summarise(df, 
          `min AirTime`  = min(AirTime, na.rm = T),
          `max AirTime`  = max(AirTime, na.rm = T),
          `mean AirTime` = mean(AirTime, na.rm = T),
          `median AirTime`  = median(AirTime, na.rm = T))

#   Count number of rows for each carrier
count(df, UniqueCarrier)


# group_by() - group cases

#   Group by carrier
df.carrier.groups <- group_by(df, UniqueCarrier)


# cobine summarise() & group_by() - summary statistics for grouped data

#   Calculate mean distance for each carrier
summarise(group_by(df, UniqueCarrier),
          `mean distance` = mean(Distance))

#   Count number of unique destinations for each carrier
summarise(group_by(df, UniqueCarrier),
          `distinct destination` = n_distinct(Destination))



# 2.4 pipe operator %>%

#   Count the rows wher carrier is American Airlines" (AA)
df %>% 
  filter(UniqueCarrier == "AA") %>% 
  summarise(n = n())

#   Filter rows for American Airlines" (AA) carrier and select columns indicating date of flight, flight number, origin and destination
df.AA <- df %>% 
  filter(UniqueCarrier == "AA") %>% 
  select(UniqueCarrier, Year, Month, DayofMonth, FlightNum, Origin, Destination)

#   Calculate total distance flown for each carrier and count number of flights, 
#     filter carriers with total distance over than 2 millions miles flown,
#     print table in descending order based od total distance
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Total distance` = sum(Distance),
            Cases = n(),
            `Distinct FlighNumbers` = n_distinct(FlightNum)) %>% 
  filter(`Total distance` > 2000000) %>% 
  arrange(desc(`Total distance`))



# 2.5 pivoting

# pivot_wider() - convert long data to wide data

#   Filter rows for selected carriers ("CO", "XE", "WN"), and select columns DayofWeek, UniqueCarrier, Distance
#   Sum distance flown for each carrier and day of week
#   Convert calculated long table to wide format, where each carrier has its own column for distance flown
df.long <- df %>% 
  filter(UniqueCarrier %in% c("CO", "XE", "WN")) %>% 
  select(DOW = DayOfWeek, Carrier = UniqueCarrier, Distance) %>% 
  group_by(DOW, Carrier) %>% 
  summarise(`Total distance` = sum(Distance)) %>% 
  arrange(DOW, Carrier)

df.wide <- df.long %>% 
  tidyr::pivot_wider(names_from = Carrier, values_from = `Total distance`)


# pivot_longer() - convert wide data to long data

#   Convert df.wide back to long format
df.long1 <- df.wide %>% 
  tidyr::pivot_longer(c("CO", "XE", "WN"), names_to = "Carrier", values_to = "Total distance" )


