# Verify if all needed packages are installed and active

import = function(libName){
  result = tryCatch({
    library(libName, character.only = T)
  }, error = function(e) {
    install.packages(libName, repos = "http://cran.us.r-project.org")
    library(libName, character.only = T)
  })
}

import("readr")
import("ggmap")
import("ggplot2")
import("mapview")
import("sf")
import("dplyr")
import("shiny")
import("lubridate")
import("weathermetrics")
import("magrittr")
import("plotly")
import("geosphere")


train_db <- read_csv("train/train.csv",locale = locale(encoding = "ISO-8859-1"))
df = data.frame(train_db)

#weather_db = read_csv("train/weather_nyc_2016.csv",locale = locale(encoding = "ISO-8859-1"))
#df_weather = data.frame(weather_db)

# Adicionar uma coluna dayofweek para colocar qual o dia da semana
df$dayofweek_pickup <- weekdays(as.Date(df$pickup_datetime))
df$dayofweek_dropoff <- weekdays(as.Date(df$dropoff_datetime))

df$day_pickup <- day(df$pickup_datetime)
df$month_pickup <- month(df$pickup_datetime)
df$hour_pickup <- hour(df$pickup_datetime)
df$minute_pickup <- minute(df$pickup_datetime)
df$second_pickup <- second(df$pickup_datetime)

df$day_dropoff <- day(df$dropoff_datetime)
df$month_dropoff <- month(df$dropoff_datetime)
df$hour_dropoff <- hour(df$dropoff_datetime)
df$minute_dropoff <- minute(df$dropoff_datetime)
df$second_dropoff <- second(df$dropoff_datetime)


td <- seconds_to_period(df$trip_duration)
abc <- sprintf('%02d %02d:%02d:%02d', day(td), td@hour, minute(td), second(td))
df$tripdurationinhour <- abc


#distancia entre pickup and dropoff
#df$begin_end_dist_meter <- distCosine(c(df$pickup_longitude, df$pickup_latitude), c(df$dropoff_longitude, df$dropoff_latitude))

#Visualizar os dados
#View(df)
#head(df)

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



b <- get_map("New York city,United States",maptype="terrain",source="google",force = ifelse(source == "google", TRUE, TRUE))


ggmap(b) + geom_point(data=poi1_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi2_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi3_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi4_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi5_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi6_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi7_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi8_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi9_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi10_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi11_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi12_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi13_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi14_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
           geom_point(data=poi15_coord, aes(x=long, y=lat, color = "blue", size = 2.0), shape = 16, show.legend = FALSE)


#Plot numero de corridas por numero de passageiros
# groupby do numero de passageiros
groupbypass <- group_by(df, passenger_count)
countgroupbypass <- summarise(groupbypass, count = n())
df_pass <- data.frame(c(countgroupbypass))
#View(df_pass)


plot_ly(x = df_pass$passenger_count, y = df_pass$count, type = "scatter", mode = "lines") %>%
  layout(
    title = "Número de corridas por número de passageiros",
    scene = list(
      xaxis = list(title = "Número de passageiros"),
      yaxis = list(title = "Número de corridas")
      ))

#Conclusão
# a grande maioria das corridas de taxi tem apenas 1 passageiro

#curiosidades
# 60 corridas não tiveram passageiros. Será que o taxista só foi pegar uma pizza??
# Há apenas 1 corrida com 8 e outra corrida com 9 pessoas. É um taxi ou uma lotação???



#Plot numero de corridas pelo dia da semana
# groupby do numero de passageiros
groupbyDOW <- group_by(df, dayofweek_pickup)
countgroupbyDOW <- summarise(groupbyDOW, count = n())
df_DOW <- data.frame(c(countgroupbyDOW))
View(df_DOW)

plot_ly(x = df_DOW$dayofweek_pickup, y = df_DOW$count)
# Conclusão: 
#Há mais corridas durante o fim de semana (sexta, sabado e domingo) do que os outros dias da semana
#o dia que tem menos corrida é segunda-feira



#Plot numero de corridas por mes
# groupby do numero dos meses
groupbyDOM <- group_by(df, month_pickup)
countgroupbyDOM <- summarise(groupbyDOM, count = n())
df_DOM <- data.frame(c(countgroupbyDOM))
#View(df_DOM)

plot_ly(x = df_DOM$month_pickup, y = df_DOM$count, type = "bar")

# Conclusão
# No mês de janeiro, menos corridas em relação aos outros meses. Isso pode ser explicado pelo inverno rigoroso
# Ja o mês de março, foi o maior mês


#Plot numero de corridas pelo hora do dia
# groupby do numero da hora do dia
groupbyHOD <- group_by(df, hour_pickup)
countgroupbyHOD <- summarise(groupbyHOD, count = n())
df_HOD <- data.frame(c(countgroupbyHOD))
View(df_HOD)

plot_ly(x = df_HOD$hour_pickup, y = df_HOD$count, type = "scatter")

#Conclusão
# a maioria das corridas acontecem entre 18 e 22 horas
# entre 1 e 6 da manha são os horarios com menos corridas


#Plot Tempo médio das corridas pela hora do dia
countgroupbyHOD1 <- summarise(groupbyHOD, Mean = mean(trip_duration))
countgroupbyHOD1
#View(countgroupbyHOD1)
plot_ly(x = df_HOD$hour_pickup, y = countgroupbyHOD1$Mean, type = "scatter", showlegend = TRUE, linetype = "1")


# Conclusão
#Apos as 4 da manhã até as 8 da manhã, o tempo médio de uma corrida é menor que os outros horarios
#Das 10 as 18 horas, o tempo médio de uma corrida é maior do que os outros horarios

#verifica se as colunas de latitude e longitude não tem registro vazio
head(df)
filter(df, is.na(pickup_longitude))
filter(df, is.na(pickup_latitude))
filter(df, is.na(dropoff_longitude))
filter(df, is.na(dropoff_latitude))

df1 <- df
# Qual é o horario que mais tem corrida com somente 1 passageiro
df1 <- filter(df1, passenger_count == 1)
groupbyHODrunw1pax <- group_by(df1, hour_pickup)
countgroupbyHODrunw1pax <- summarise(groupbyHODrunw1pax, count = n())
df_HODrunw1pax <- data.frame(c(countgroupbyHODrunw1pax))

df2 <- df
# Qual é o horario que mais tem corrida com somente 2 passageiro
df2 <- filter(df2, passenger_count == 2)
groupbyHODrunw2pax <- group_by(df2, hour_pickup)
countgroupbyHODrunw2pax <- summarise(groupbyHODrunw2pax, count = n())
df_HODrunw2pax <- data.frame(c(countgroupbyHODrunw2pax))

df3 <- df
# Qual é o horario que mais tem corrida com somente 3 passageiro
df3 <- filter(df3, passenger_count == 3)
groupbyHODrunw3pax <- group_by(df3, hour_pickup)
countgroupbyHODrunw3pax <- summarise(groupbyHODrunw3pax, count = n())
df_HODrunw3pax <- data.frame(c(countgroupbyHODrunw3pax))

plot_ly(x = df_HODrunw1pax$hour_pickup, y = df_HODrunw1pax$count, type = "scatter", showlegend = TRUE, linetype = "1") %>%
  add_trace(x = df_HODrunw2pax$hour_pickup, y = df_HODrunw2pax$count) %>%
  add_trace(x = df_HODrunw3pax$hour_pickup, y = df_HODrunw3pax$count)


