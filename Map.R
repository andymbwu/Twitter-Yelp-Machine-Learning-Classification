# Installing packages
# install.packages(c('dplyr','ggplot2','rjson','jsonlite','leaflet','RCurl','stringr','tidyr'))

# Importing libraries
library(dplyr)
library(ggplot2)
library(rjson)
library(leaflet)
library(RCurl)
library(stringr)
library(tidyr)
library(doBy)

setwd("C:/Users/andywm/r-workspace/MLProject")

########### SENTIMENT MAPS #############

#--------------- All Tweets ---------------

# Map that shows all positive and negative tweets, with a select box 
# in the corner to select only positive or only negative tweets.
file = 'All'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")

# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

# Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Show the map
pal <- colorFactor(c("red", "darkgreen"), domain = c("positive", "negative"))

map = leaflet(tweets_df) %>% addTiles() %>%
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="negative")$lat,
    lng = subset(x=tweets_df,Sentiment=="negative")$long,
    radius = 6,
    color = "red",
    stroke = FALSE,
    group = 'Negative'
  )%>% 
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="positive")$lat,
    lng = subset(x=tweets_df,Sentiment=="positive")$long,
    radius = 6,
    color = "green",
    stroke = FALSE,
    group = 'Positive'
  )%>% 
  addLayersControl(
    overlayGroups = c("Positive", "Negative"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

addLegend(map, position = "bottomright", pal=pal, 
          values=tweets_df$Sentiment)


#--------------- December Tweets ---------------

# Map that shows all positive and negative tweets in December, with a select box 
# in the corner to select only positive or only negative tweets.
file = 'AllDec'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")

# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

# Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Show the map
pal <- colorFactor(c("red", "darkgreen"), domain = c("positive", "negative"))

map = leaflet(tweets_df) %>% addTiles() %>%
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="negative")$lat,
    lng = subset(x=tweets_df,Sentiment=="negative")$long,
    radius = 6,
    color = "red",
    stroke = FALSE,
    group = 'Negative'
  )%>% 
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="positive")$lat,
    lng = subset(x=tweets_df,Sentiment=="positive")$long,
    radius = 6,
    color = "green",
    stroke = FALSE,
    group = 'Positive'
  )%>% 
  addLayersControl(
    overlayGroups = c("Positive", "Negative"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

addLegend(map, position = "bottomright", pal=pal, 
          values=tweets_df$Sentiment)


#--------------- January Tweets ---------------

# Map that shows all positive and negative tweets in December, with a select box 
# in the corner to select only positive or only negative tweets.
file = 'AllJan'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")

# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

# Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Show the map
pal <- colorFactor(c("red", "darkgreen"), domain = c("positive", "negative"))

map = leaflet(tweets_df) %>% addTiles() %>%
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="negative")$lat,
    lng = subset(x=tweets_df,Sentiment=="negative")$long,
    radius = 6,
    color = "red",
    stroke = FALSE,
    group = 'Negative'
  )%>% 
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="positive")$lat,
    lng = subset(x=tweets_df,Sentiment=="positive")$long,
    radius = 6,
    color = "green",
    stroke = FALSE,
    group = 'Positive'
  )%>% 
  addLayersControl(
    overlayGroups = c("Positive", "Negative"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

addLegend(map, position = "bottomright", pal=pal, 
          values=tweets_df$Sentiment)


#--------------- February Tweets ---------------

# Map that shows all positive and negative tweets in December, with a select box 
# in the corner to select only positive or only negative tweets.
file = 'AllFeb'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")

# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

# Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Show the map
pal <- colorFactor(c("red", "darkgreen"), domain = c("positive", "negative"))

map = leaflet(tweets_df) %>% addTiles() %>%
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="negative")$lat,
    lng = subset(x=tweets_df,Sentiment=="negative")$long,
    radius = 6,
    color = "red",
    stroke = FALSE,
    group = 'Negative'
  )%>% 
  addCircleMarkers(
    lat = subset(x=tweets_df,Sentiment=="positive")$lat,
    lng = subset(x=tweets_df,Sentiment=="positive")$long,
    radius = 6,
    color = "green",
    stroke = FALSE,
    group = 'Positive'
  )%>% 
  addLayersControl(
    overlayGroups = c("Positive", "Negative"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

addLegend(map, position = "bottomright", pal=pal, 
          values=tweets_df$Sentiment)


################################################################
################################################################


########### HANGOVER MAPS #############

#--------------- December ---------------
file = 'HangoverDecember'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")

# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

#Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Showing the map
leaflet(tweets_df) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(),
)


#--------------- January ---------------
file = 'HangoverJanuary'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")

# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

#Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Showing the map
leaflet(tweets_df) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(),
)


#--------------- February ---------------
file = 'HangoverFebruary'

# Reading in the file using tab as delim
tweets = read.delim(file, header = TRUE, sep = "\t", dec = ".")

# Naming columns
names(tweets) <- c("Name", "Text", "SentimentNum", "Location", "Coord", "TopicNum", "Sentiment", "Topic", "Month")
 
# Turning into DF
df <- data.frame(tweets)

# Omiting NA values
df <- na.omit(df)

# Removing brackets from coordinate column
df$Coord <- gsub("[()]", "", df$Coord)

# Separating coordinates into 2 separate columns lat and long
tweets_df <- separate(df, col = Coord, into = c("lat","long"), sep = ", ")
tweets_df$lat <- as.numeric(as.character(tweets_df$lat))
tweets_df$long <- as.numeric(as.character(tweets_df$long))

# Showing the map
leaflet(tweets_df) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions(),
)

##################################################



