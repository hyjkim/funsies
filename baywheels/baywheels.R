# Archives baywheels free bike data.

library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(ggmap)
library(lubridate) 

# Archival trip data
# bw <- read_delim("/Users/jkim/Downloads/201911-baywheels-tripdata.csv", ";")
free <- fromJSON("https://gbfs.baywheels.com/gbfs/en/free_bike_status.json")
bikes <- free$data$bikes
bikes$datetime <- as_datetime(now())
write_tsv(bikes, "~/git/funsies/baywheels/free_bikes.tsv", append=T)


nrow(bikes)

#stations <- fromJSON("https://gbfs.baywheels.com/gbfs/en/station_status.json")

# Cluster bikes
bikes$cluster = kmeans(dplyr::select(bikes, lat, lon), 2)$cluster

# Plot with color
#qmplot(lon, lat, data=bikes, color=factor(cluster))

# View lat and lon of bikes
#bikes %>%
#  select(lat, lon, cluster) %>%
#  gather(key, val, -cluster) %>%
#  ggplot(aes(x=val, color=factor(cluster))) +
#  geom_density() +
#  facet_wrap(~ key, scales="free")

# filter bikes in sf
sfbikes <- bikes %>%
  filter(lat > 37.6) %>%
  filter(lon < 122.3)

nrow(sfbikes)
# plot only sf bikes 
qmplot(lon, lat, data=sfbikes, color=factor(cluster), geom=c("point", "density2d"), alpha=0.75)


# history
history <- read_tsv("~/git/funsies/baywheels/free_bikes.tsv", col_names=colnames(sfbikes))

history %>%
  filter(!datetime %in% c("1", "2")) %>%
  mutate(datetime = parse_date_time(datetime, orders=c("ymdHMS"))) %>%
  group_by(datetime) %>%
  summarise(freebikes = n()) %>%
  ungroup() %>%
  mutate(hour = hour(datetime)) %>%
  mutate(commute_time = ifelse((hour >= 7 & hour <= 9) | (hour >= 16 & hour <= 18), T, F)) %>%
  ggplot(aes(x = datetime, y = freebikes, color=commute_time)) +
  geom_point() +
  labs(title = "Baywheels bike availability")
 