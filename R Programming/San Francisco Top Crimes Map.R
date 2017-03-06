
# Map the occurences of the top 12 crimes in San Francisco

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

train <- read_csv("../input/train.csv.zip")
map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")

train <- read.csv("train.csv")
map <- get_map(location="sanfrancisco",zoom=12,source="osm")
plot(map)

counts <- summarise(group_by(train, Category), Counts=length(Category))
counts <- counts[order(-counts$Counts),]
# This removes the "Other Offenses" category
top12 <- train[train$Category %in% counts$Category[c(1,3:13)],]

p <- ggmap(map) +
      geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
      guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                   title="Type of Crime")) +
      scale_colour_brewer(type="qual",palette="Paired") + 
      ggtitle("Top Crimes in San Francisco") +
      theme_light(base_size=20) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")


