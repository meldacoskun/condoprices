library(rvest)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)

base = "https://condos.ca"
path = "/toronto?mode=Sale"
df = data.frame()

for(i in 1:26){
  if(i!=1){path = paste("/toronto?mode=Sale&page=", i, sep = "")}
  url = modify_url(base, path = path)
  document <- read_html(url)
  
  temp = document %>% html_elements(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "buduQR", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "kJLXXT", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "kbcwIq", " " ))]') %>% html_text2()
  
  temp = as.data.frame(temp[2:length(temp)])
  df = bind_rows(df, temp)
}
#df_wide = df
df_wide = data.frame("price" = "", "add"="", "info"="")
#i=5
for(i in 1:nrow(df)){
  if(i%%3 == 1){ df_wide[ceiling(i/3), 1] = df[i,]
  }else if(i%%3 == 2){df_wide[ceiling(i/3), 2] = df[i,]
  }else if(i%%3 == 0){df_wide[ceiling(i/3), 3] = df[i,]}
}

df_edited = df_wide %>% 
  separate( col = price, into = c("price", "time"), sep = "\n") %>% 
  separate(col = info, into = c("info2", "sqft"), sep = "\n")%>% 
  separate(col = info2, into = c("bedinfo", "parking"), sep = "\\$", remove = F)%>% 
  separate(col = bedinfo, into = c("den", "bedinfo"), sep = "\\+", remove = F,fill="left")%>% 
  separate(col = bedinfo, into = c("bed", "bath"), sep = "BD", remove = F,fill="left")%>% 
  separate(col = bath, into = c("bath", "parking"), sep = "BA", remove = F,fill="left")%>% 
  separate(col = parking, into = c("parking"), sep = "Parking",fill="left")%>% 
  separate(col = add, into = c("unit#", "streetName"), sep = " - ", remove = F, fill="left")%>% 
  separate(col = sqft, into = c("sqft1", "sqft_cat"), 
           sep = "-", fill = "right", remove = F)

df_edited = df_edited %>% 
  mutate(sqft_cat= gsub(",|(sqft)", "", sqft_cat))

df_edited = df_edited %>% select(price, streetName, bed, bath, den, parking, sqft_cat)
df_edited = df_edited %>%  mutate(price = str_replace(price, "\\$", ''))

df_edited$price = as.numeric(gsub(",", "", df_edited$price))
df_edited$bath = as.factor(df_edited$bath)
df_edited$bed = as.factor(df_edited$bed)
df_edited$den = as.factor(df_edited$den)
df_edited$parking = as.factor(df_edited$parking)
df_edited$sqft_cat = as.numeric(df_edited$sqft_cat)
summary(df_edited$sqft_cat)

#saveRDS(df_edited, "df_edited.rds")


#install.packages("leaflet")
library(leaflet)


#install.packages("tidygeocoder")
library(tidygeocoder)

# load packages
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(ggplot2)



key = apikey
#register_google(key = key)
#geocode = geocode(location = df_edited$streetName, output = "latlona", source = "google")
#saveRDS(geocode, "geocode.rds")
geocode = readRDS(file = "geocode.rds")
df_edited= readRDS("df_edited.rds")
df_edited = df_edited %>% mutate(lon = geocode$lon, lat = geocode$lat)

#df = df_edited %>% filter(bed==1, den==1, parking==1)


### EDA ###
df_edited %>% summary()


#the distribution of recent condo prices in toronto
gg_condoprices = ggplot(data = df_edited, aes(x=price)) + 
  geom_histogram(bins = 50, aes(y= ..density..), colour= "black", fill = "white") +
  geom_vline(aes(xintercept = mean(price)), 
             color = "red", linewidth = 1) + 
  geom_vline(aes(xintercept = median(price)), 
             color = "blue", linewidth =1) +
  scale_x_continuous(breaks=seq(100000,max(df_edited$price),80000)) +
  xlim(100000, max(df_edited$price)) +
  geom_density(fill="blue", alpha = .2) +  
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
                     
#density plots y axis is not informative most of the time, and can be removed.

#let's calculate sqft price for the condos
#ppsqft = price per sqft
df_edited = df_edited %>% mutate(ppsqft = price/sqft_cat)
df_edited %>% summary()
gg_ppsqft = ggplot(data = df_edited, aes(x=ppsqft)) + 
  geom_histogram(bins = 50, aes(y=..density..),
                 colour= "black", fill = "white") +
  geom_vline(aes(xintercept = mean(ppsqft,na.rm = T)), 
             color = "red", linewidth = 1) + 
  geom_vline(aes(xintercept = median(ppsqft,na.rm = T)), 
             color = "blue", linewidth =1) +
  geom_density(fill="blue", alpha = .2) +  
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

df_edited = df_edited %>% 
  mutate(ppsqft_bins = ntile(ppsqft, 4))
df = df_edited

pal <- 
  colorFactor(palette = c("blue", "green", "red", "black"), 
              levels = c("1", "2", "3", "4"))


my_map <- df %>% leaflet() %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles 
  # define the markers
  addCircleMarkers(~lon, ~lat, stroke = FALSE, 
                   fillColor = ~pal(ppsqft_bins), fillOpacity = .8)%>%
  setView(lng = median(df$lon, na.rm = T), 
          lat = median(df$lat, na.rm = T), zoom = 12) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLegend(labels = c("700<", "700-888", "888-1000", "1000-2100"), colors = c("blue", "green", "red", "black"))

#df %>% filter(ppsqft_bins ==4) %>% select(ppsqft) %>% range()

my_map


#### CLUSTERS ####
data <- df %>% select(ppsqft, lat, lon) %>% na.omit()
km.out <- kmeans(data, centers = 3, nstart = 20)
km.out




# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:8) {
  # Fit the model: km.out
  km.out <- kmeans(data, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:10, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )
# Select number of clusters
k <- 4
set.seed(123)
# Build model with k clusters: km.out
km.out <- kmeans(data, centers = k, nstart = 20)

data$cluster_id <- factor(km.out$cluster)

data %>% leaflet() %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles 
  # define the markers
  addCircleMarkers(~lon, ~lat, stroke = FALSE, 
                   fillColor = ~pal(cluster_id), fillOpacity = .8)%>%
  setView(lng = median(df$lon, na.rm = T), 
          lat = median(df$lat, na.rm = T), zoom = 12) %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
   addLegend(labels = c("700<", "700-888", "888-1000", "1000-2100"), colors = c("blue", "green", "red", "black"))
