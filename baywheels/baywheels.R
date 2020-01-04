# Archives baywheels free bike data.

library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(ggmap)

bw <- read_delim("/Users/jkim/Downloads/201911-baywheels-tripdata.csv", ";")

free <- fromJSON("https://gbfs.baywheels.com/gbfs/en/free_bike_status.json")
bikes <- free$data$bikes

oldbikes <- bikes
free <- fromJSON("https://gbfs.baywheels.com/gbfs/en/free_bike_status.json")
bikes <- free$data$bikes

nrow(bikes)

shared <- bikes$name %in% oldbikes$name
sum(shared) / length(shared)

#stations <- fromJSON("https://gbfs.baywheels.com/gbfs/en/station_status.json")

# Cluster bikes
bikes$cluster = kmeans(dplyr::select(bikes, lat, lon), 2)$cluster

# Plot with color
qmplot(lon, lat, data=bikes, color=factor(cluster))

# View lat and lon of bikes
bikes %>%
  select(lat, lon, cluster) %>%
  gather(key, val, -cluster) %>%
  ggplot(aes(x=val, color=factor(cluster))) +
  geom_density() +
  facet_wrap(~ key, scales="free")

# filter bikes in sf
sfbikes <- bikes %>%
  filter(lat > 37.6) %>%
  filter(lon < 122.3)

nrow(sfbikes)

# plot only sf bikes 
qmplot(lon, lat, data=sfbikes, color=factor(cluster), geom=c("point", "density2d"), alpha=0.75)

# ggmap docs https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf