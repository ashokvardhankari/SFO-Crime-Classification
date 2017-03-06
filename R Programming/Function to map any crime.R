library(ggmap)
library(ggplot2)
library(dplyr)

train <- read.csv("train.csv")
train <- read.csv("../input/train.csv")
map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")

map_crime <- function(crime_df, crime) {
  filtered <- filter(crime_df, category_predict %in% crime)
  plot <- ggmap(map, extent='device') + 
    geom_point(data=filtered, aes(x=x, y=y, color=category_predict), alpha=0.6)
  return(plot)
}

map_crime(train, c('SUICIDE', 'LARCENY/THEFT'))
 