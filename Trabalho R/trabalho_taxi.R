# Verify if all needed packages are installed and active
mypkgdf <- data.frame(installed.packages())

#Verify if package readr installed
if ('readr' %in% mypkgdf$Package) {
  print("package readr is installed")
  library(readr)
} else {
  #install package readr
  install.packages("readr")
  library(readr)
}

#Verify if package ggmap installed
if ('ggmap' %in% mypkgdf$Package) {
  print("package ggmap is installed")
  library(ggmap)
} else {
  #install package ggmap
  install.packages("ggmap")
  library(ggmap)
}

#Verify if package ggplot2 installed
if ('ggplot2' %in% mypkgdf$Package) {
  print("package ggplot2 is installed")
  library(ggplot2)
} else {
  #install package ggplot2
  install.packages("ggplot2")
  library(ggplot2)
}

#Verify if package mapview from google maps view installed
if ('mapview' %in% mypkgdf$Package) {
  print("package mapview is installed")
  library(mapview)
} else {
  #install package mapview
  install.packages("mapview")
  library(mapview)
}

#Verify if package sf installed
if ('sf' %in% mypkgdf$Package) {
  print("package sf is installed")
  library(sf)
} else {
  #install package sf
  install.packages("sf")
  library(sf)
}

#test_db <- read_csv("train/train.csv",locale = locale(encoding = "ISO-8859-1"))
#View(test_db)

#df = data.frame(test_db)

# Adicionar uma coluna dayofweek para colocar qual o dia da semana
#df$dayofweek <- weekdays(as.Date(df$pickup_datetime))

#Visualizar os dados
#View(df)

#locationsteste <- st_as_sf(locations, coords = c(-73.98519,40.75818), crs = 4326)
#mapview(locationsteste)
#?mapview

# Point of interest
#1. John F Kennedy Airport
poi1_lat <- c(40.6441666667)
poi1_long <- c(-73.7822222222)


lat <- c(40.75818,40.74932)
long <- c(-73.98519,-73.97258)
bbox <- make_bbox(long,lat,f=0.05)
b <- get_map(bbox,maptype="terrain",source="google")

ggmap(b)


# m <- get_map("New York",zoom=12,maptype="terrain",source="google")
# ggmap(m)

latpoint <- data.frame(40.754)
longpoint <- data.frame(-73.9800)

ggmap(b) +
  geom_point(data = bbox, aes(x = longpoint, y = latpoint, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


d <- data.frame(lat=c(50.659631, 50.607213, 50.608129),
                lon=c(3.09319, 3.011473, 3.031529))

Lille <- get_map("Lille,France", zoom=12)

p <- ggmap(Lille)
p <- p + geom_point(data=d, aes(x=lon, y=lat, fill = "red", alpha = 0.8),size=5)
p
ggplot_build(p)



