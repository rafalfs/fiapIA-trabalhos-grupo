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

#d <- data.frame(lat=c(50.659631, 50.607213, 50.608129),
#                lon=c(3.09319, 3.011473, 3.031529))

# Point of interest
#1. John F Kennedy Airport
poi1_coord <- data.frame(lat=c(40.6441666667),long=c(-73.7822222222))

#2. Central Park
poi2_coord <- data.frame(lat=c(40.785091),long=c(-73.968285))

#3. Times Square
poi3_coord <- data.frame(lat=c(40.758896),long=c(-73.985130))

#4. Broadway Theatre
poi4_coord <- data.frame(lat=c(40.753496986),long=c(-73.985162726))

#5. Wall Street Charging Bull
poi5_coord <- data.frame(lat=c(40.705576),long=c(-74.013421))

#6. Rockefeller Center
poi6_coord <- data.frame(lat=c(40.758740),long=c(-73.978674))

#7. Macy'S Herald Square Store
poi7_coord <- data.frame(lat=c(40.750782),long=c(-73.988959))

#8. Bank of America Tower
poi8_coord <- data.frame(lat=c(40.755604),long=c(-73.984932))

#9. NATIONAL SEPTEMBER 11 MEMORIAL
poi9_coord <- data.frame(lat=c(40.7070138386),long=c(-74.008166634))

#10. Bryant Park
poi10_coord <- data.frame(lat=c(40.755603),long=c(-73.984931))

#11. Statue of Liberty National Monument
poi11_coord <- data.frame(lat=c(40.689247),long=c(-74.044502))

#12. AMERICAN MUSEUM OF NATURAL HISTORY
poi12_coord <- data.frame(lat=c(40.7749969),long=c(-73.971496114))

#13. World Trade Center'S Liberty Park
poi13_coord <- data.frame(lat=c(40.710440),long=c(-74.013851))

#14. World Trade Center'S Liberty Park
poi14_coord <- data.frame(lat=c(40.7558303),long=c(-73.97416277))

#15. Madison Square Garden
poi15_coord <- data.frame(lat=c(40.750298),long=c(-73.993324))



lat <- c(40.600000,40.730610)
long <- c(-73.013421,	-73.935242)
bbox <- make_bbox(long,lat,f=0.05)
b <- get_map("New York city,United States",maptype="terrain",source="google",force = ifelse(source == "google", TRUE, TRUE))


ggmap(b) + geom_point(data=poi1_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi2_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi3_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi4_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi5_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi6_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi7_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi8_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi9_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi10_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi11_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi12_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi13_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi14_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE) +
           geom_point(data=poi15_coord, aes(x=long, y=lat, color = "red", size = 2.5), shape = 23, fill="blue", show.legend = FALSE)





