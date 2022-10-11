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





















