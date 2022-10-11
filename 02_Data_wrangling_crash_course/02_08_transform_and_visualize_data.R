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
library(ggplot2)

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



# 2.6 separating and uniting

# unite() - combine multiple columns into one column

#   Select Year, Month, DayofMonth and create table df.date
#   create new column Date as a combination of all thgree columns
#   date format should be "YYYY-mm-dd" (year - month - day of month)
#   only keep distinct dates
#   convert column to date type
#   sort dates

# We will use library stringr for adding leading zeros for Month and DayofMonth column !!!

#install.packages("stringr")
library(stringr)

df.date <- df %>% 
  select(Year, Month, DayofMonth) %>% 
  mutate(Month = str_pad(Month, width = 2, side = "left", pad = "0"), # add leading zeros to Month
         DayofMonth = str_pad(DayofMonth, width = 2, side = "left", pad = "0")) %>% # add leading zeros to DayofMonth
  tidyr::unite(col = Date, Year, Month, DayofMonth, sep = "-") %>% 
  distinct() %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
  arrange(Date)


# separate() - split one column into multiple columns

#   split Date column into Year, Month, DayofMonth
#   remove leading zeros where necessary
#   sort columns
df.date1 <- df.date %>% 
  tidyr::separate(col = Date, into = c("Year", "Month", "DayofMonth"), sep = "-") %>% 
  mutate(Month = as.numeric(Month), # remove leading zeros
         DayofMonth = as.numeric(DayofMonth))%>% 
  arrange(Year, Month, DayofMonth)



# 2.7 Transform and visualize data

# idea: combine data transformation tools with data visualization tools (ggplot2)

# Visualize number of flights and distance flown for each carrier (scatter plot)
df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`Total distance flown in miles` = sum(Distance),
            `Total number of flights` = n(),
            `Different flights` = n_distinct(FlightNum)) %>% 
  mutate(Carrier = as.factor(UniqueCarrier)) %>% 
  ggplot(aes(x = `Total number of flights`, 
             y = `Total distance flown in miles`,
             color = Carrier, 
             label = Carrier)) +
  geom_text(size = 10, position = "jitter", show.legend = F) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Total number of flights (log10 scale)") +
  ylab("Total distance flown in miles (log10 scale)") +
  ggtitle("Number of fligts VS distance flown break down by carrier") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) 


# Visualize distribution of flights distance flown (histogram) 
# for top 5 carriers (based on total distance flown)
# use facetting
top.5.carriers <- df %>% 
  group_by(UniqueCarrier) %>% 
  summarise(Distance = sum(Distance)) %>% 
  arrange(desc(Distance)) %>% 
  slice(1:5) %>% 
  pull(UniqueCarrier)

df %>% 
  filter(UniqueCarrier %in% top.5.carriers) %>% 
  mutate(Carrier = factor(UniqueCarrier, levels = top.5.carriers)) %>% 
  ggplot(aes(x = Distance, fill = Carrier)) +
  geom_histogram(color = "black", bins = 40, show.legend = F) +
  facet_wrap( ~ Carrier) +
  xlab("Flight distance in miles") +
  ylab("Frequency") +
  ggtitle("Flight distance distribution - Top 5 carriers") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text = element_text(face = "bold", size = 16))


# Visualize total number of flights for each month (line plot)
df %>% 
  group_by(Month) %>% 
  summarise(`Number of flights` = n()) %>% 
  ungroup() %>% 
  mutate(Month = as.factor(Month)) %>% 
  ggplot(aes(x = Month, y = `Number of flights`, group = 1)) +
  geom_line(size = 1.2, color = "gray") +
  geom_point(size = 5, color = "brown1") +
  ggtitle("Number of fligts monthly") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) 

