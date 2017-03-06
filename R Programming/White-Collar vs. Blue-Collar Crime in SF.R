# White-Collar vs. Blue-Collar in San Francisco
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(readr)
library(lubridate)
library(ggmap)

#use clara() or pam() routines for clustering from {cluster}
#instead the kmeans() from {stats}
library(cluster)

# Read in the data, but we need Dates

train <- read_csv("../input/train.csv" ,col_types = list(Dates = col_datetime()))

train <- filter(train, Category != "OTHER OFFENSES", Category != "NON-CRIMINAL", Category != "WARRANTS")

train$Year <- year(ymd_hms(train$Dates))

crimes_per_year <- table(train$Category,train$Year)

crimes_per_year <- melt(crimes_per_year)

names(crimes_per_year) <- c("Category","Year","Count")

crimes_per_year=data.table(crimes_per_year)


#according to https://en.wikipedia.org/wiki/White-collar_crime#Blue-collar_crime
white_crime=c("FRAUD", "FORGERY/COUNTERFEITING", "BAD CHECKS" , "EXTORTION", "EMBEZZLEMENT", "SUSPICIOUS OCC",
              "BRIBERY")

blue_crime=c("VANDALISM", "LARCENY/THEFT", "STOLEN PROPERTY", "ROBBERY", "DRIVING UNDER THE INFLUENCE",
             "DISORDERLY CONDUCT", "LIQUOR LAWS", "VEHICLE THEFT", "ASSAULT", "KIDNAPPING", "TRESPASS", 
             "ARSON", "RECOVERED VEHICLE")
             
other_crime=c("MISSING PERSON", "RUNAWAY", "FAMILY OFFENSES", "SEX OFFENSES NON FORCIBLE",
             "PORNOGRAPHY/OBSCENE MAT", "WEAPON LAWS", "DRUNKENNESS", "SUICIDE", "TREA",
             "DRUG/NARCOTIC", "SEX OFFENSES FORCIBLE",  "LOITERING")


"%ni%" <- Negate("%in%")

crimes_per_year[, Collar := character(nrow(crimes_per_year))]

crimes_per_year[Category %in% blue_crime, Collar := "BLUE COLLAR"]
crimes_per_year[Category %in% white_crime, Collar := "WHITE COLLAR"]
crimes_per_year[Category %ni% blue_crime & Category %ni% white_crime, Collar := "OTHER"]

g <- ggplot(crimes_per_year,aes(x=Year, y=Count,fill = Collar)) + 
  geom_bar(stat = "identity")+
  coord_flip() +
  facet_grid(.~Collar) +
  labs(title="White-Collar vs Blue-Collar Crimes in SF")


ggsave("White_Collar_vs_Blue_Collar.png", g, width=14, height=10, units="in")

rm(crimes_per_year)
rm(g)


train=data.table(train)

train[, Collar := character(nrow(train))]

train[Category %in% blue_crime, Collar := "BLUE COLLAR"]
train[Category %in% white_crime, Collar := "WHITE COLLAR"]
train[Category %ni% blue_crime & Category %ni% white_crime, Collar := "OTHER"]

train <- filter(train, Collar != "OTHER")

geo_map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")


p_df <- train[train$Collar=="WHITE COLLAR",]
p_df <- data.frame(X=p_df$X, Y=p_df$Y)

#data is too large for pam, so let's use clara instead
p_clusters <- clara(p_df, 3)
p_clusters <- as.data.frame(p_clusters$medoids)

p <- ggmap(geo_map)+
  geom_point(data=train[train$Collar=="WHITE COLLAR"], aes(x=X, y=Y, color=factor(Collar)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Crime Type")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("White-Collar crimes in SF") +
 scale_color_manual(values=c("#FF0000", "#0000FF"))+
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

p <- p + geom_point(data=p_clusters, aes(x=X, y=Y, color="center"), alpha=0.3, size=30)

ggsave("White_Collar_Crime_Map.png", p, width=14, height=10, units="in")

rm(p, p_clusters, p_df)


b_df <- train[train$Collar=="BLUE COLLAR",]
b_df <- data.frame(X=b_df$X, Y=b_df$Y)
#data is too large for pam, so let's use clara instead
b_clusters <- clara(b_df, 3)
b_clusters <- as.data.frame(b_clusters$medoids)

b <- ggmap(geo_map)+
  geom_point(data=train[train$Collar=="BLUE COLLAR"], aes(x=X, y=Y, color=factor(Collar)), alpha=0.05) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                               title="Crime Type")) +
  scale_colour_brewer(type="qual",palette="Paired") + 
  ggtitle("Blue-Collar crimes in SF") +
 scale_color_manual(values=c("#0000FF", "#FF0000"))+
  theme_light(base_size=20) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


b <- b + geom_point(data=b_clusters, aes(x=X, y=Y, color="center"), alpha=0.3, size=30)

ggsave("Blue_Collar_Crime_Map.png", b, width=14, height=10, units="in")

rm(b, b_clusters, b_df)
rm(train)