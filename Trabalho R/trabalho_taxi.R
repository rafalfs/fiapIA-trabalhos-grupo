
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


################################ ENRIQUECIMENTO DO DATASET ################################

# Carga do dataset meteorológico
weather_db = read_csv("data/datasets/weather_nyc_2016.csv",locale = locale(encoding = "ISO-8859-1"))
df_weather = data.frame(weather_db)

# Carga do dataset de feriados
holidays_db = read_csv("data/datasets/holidays.csv",locale = locale(encoding = "ISO-8859-1"))
df_holidays = data.frame(holidays_db)

# Conversão das datas, temperaturas e flags de chuva e neve
df_weather %>%
  mutate(date = dmy(date),
         rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
         snow_fall = as.numeric(ifelse(snow.fall == "T", "0.01", snow.fall)),
         snow_depth = as.numeric(ifelse(snow.depth == "T", "0.01", snow.depth)),
         all_precip = snow_fall + rain,
         has_snow = (snow_fall > 0) | (snow_depth > 0),
         has_rain = rain > 0,
         max_temp = fahrenheit.to.celsius(maximum.temerature),
         min_temp = fahrenheit.to.celsius(minimum.temperature),
         avg_temp = fahrenheit.to.celsius(average.temperature)) -> df_weather

# Conversão da data do dataset de treino para fazer o join com os outro datasets posteriormente
df %>% 
  mutate(date = as.Date(ymd_hms(pickup_datetime))) -> df

# Seleciona somente as colunas do dataset meteorológico que nos interessam
weather <- df_weather %>%
  select(date, rain, snow_fall, all_precip, has_snow, has_rain, snow_depth, max_temp, min_temp)

# Join do dataset de treino com o dataset meteorológico
df = left_join(df, weather, by = "date")

# Renomeia algumas colunas do dataset
colnames(df_holidays)[1] = "holiday_weekday"
colnames(df_holidays)[3] = "holiday"

# Converte a data do dataset para o formato do lubridate
df_holidays %>% 
  mutate(date = ymd(date)) -> df_holidays

# Join do dataset de treino com o dataset de feriados
df = left_join(df, df_holidays, by = "date")



# Funções para calcular a distancia em km a partir de 2 coordenadas (latitude e longitude)
deg2rad = function (deg) {
  return(deg * (pi/180))
}
getDistanceFromLatLonInKm = function (lat1,lon1,lat2,lon2) {
  raio = 6371 # raio da terra em km
  dLat = deg2rad(lat2-lat1)
  dLon = deg2rad(lon2-lon1) 
  a = sin(dLat/2) * sin(dLat/2) +
    cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * 
    sin(dLon/2) * sin(dLon/2)
  
  c = 2 * atan2(sqrt(a), sqrt(1-a));
  d = raio * c;
  return(d);
}

# calcula a distancia euclidiana, distancia de manhattan e KMs percorridos
df %>% 
  mutate(dist_eucl = sqrt((pickup_longitude - dropoff_longitude) ** 2 + (pickup_latitude - dropoff_latitude) ** 2),
         dist_manh = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude),
         dist_km = getDistanceFromLatLonInKm(pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude)
  ) -> df

############################## FIM ENRIQUECIMENTO DO DATASET ##############################

############################## SEPARACAO DOS QUADRANTES ###################################

#Fizemos utilizando os 900m quadrados para cada quadrante. Para isso ao invés de criar um
#dataframe com todos os quadrantes, pois seriam milhões de quadrantes,
#fizemos uma função matemática para calcular o quadrante dado um ponto inicial 
#e um ponto final no mapa (abrangendo a cidade de new york)

verificarSeLatLonEstaEmNY = function(lat, lon){
  if(lat > 40.496423 && lat < 40.899755 &&
     lon > -74.258301 && lon < -73.702518)
    return(TRUE)
  else return(FALSE)
}

obterQuadranteLatLon = function(lat, lon){
  if(!verificarSeLatLonEstaEmNY(lat, lon))
    return(NULL)
  
  lat.inicial = 40.496423
  lon.inicial = -74.258301
  lat.final = 40.899755
  lon.final = -73.702518
  incremento = 0.000270
  
  lat.pos = ceiling((lat - lat.inicial) / incremento)
  lon.pos = ceiling((lon - lon.inicial) / incremento)
  
  quadrante = (lat.pos-1) * ((lon.final - lon.inicial) / incremento) + lon.pos
  return(quadrante)
}

df %>% 
  mutate(quadrante_origem = obterQuadranteLatLon(pickup_latitude, pickup_longitude),
         quadrante_destino = obterQuadranteLatLon(dropoff_latitude, dropoff_longitude)) -> df

############################## FIM SEPARACAO DOS QUADRANTES ###############################

# Adicionar uma coluna dayofweek para colocar qual o dia da semana
df$dayofweek_pickup <- weekdays(as.Date(df$pickup_datetime))
df$dayofweek_dropoff <- weekdays(as.Date(df$dropoff_datetime))

date <- as.Date(df$pickup_datetime)
df$day_pickup <- format.Date(date, "%d")
df$month_pickup <- format.Date(date, "%m")
df$hour_pickup <- strftime(df$pickup_datetime, format="%H")
df$minute_pickup <- strftime(df$pickup_datetime, format="%M")
df$second_pickup <- second(df$pickup_datetime)
df$hourmin_pickup <- paste(df$hour_pickup, df$minute_pickup, sep=":")

date1 <- as.Date(df$dropoff_datetime)
df$day_dropoff <- format.Date(date1, "%d")
df$month_dropoff <- format.Date(date1, "%m")
df$hour_dropoff <- strftime(df$dropoff_datetime, format="%H")
df$minute_dropoff <- strftime(df$dropoff_datetime, format="%M")
df$second_dropoff <- second(df$dropoff_datetime)
df$hourmin_dropoff <- paste(df$hour_dropoff, df$minute_dropoff, sep=":")

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
#1. John F Kennedy Airport terminal 4
#2. Central Park
#3. Times Square
#4. Broadway Theatre
#5. Wall Street Charging Bull
#6. Rockefeller Center
#7. Macy'S Herald Square Store
#8. Bank of America Tower
#9. NATIONAL SEPTEMBER 11 MEMORIAL
#10. Bryant Park
#11. Statue of Liberty National Monument
#12. AMERICAN MUSEUM OF NATURAL HISTORY
#13. World Trade Center'S Liberty Park
#14. 5 Avenue-Bryant Park Station
#15. Madison Square Garden
poi_coord <- data.frame(lat=c(40.6441666667, 
                              40.785383,
                              40.758896,
                              40.763262,
                              40.705576,
                              40.758740,
                              40.750782,
                              40.755604,
                              40.7070138386,
                              40.755603,
                              40.689247,
                              40.7749969,
                              40.710440,
                              40.753907,
                              40.750298),
                        long=c(-73.7822222222, 
                               -73.969336,
                               -73.985130,
                               -73.983067,
                               -74.013421,
                               -73.978674,
                               -73.988959,
                               -73.984932,
                               -74.008166634,
                               -73.984931,
                               -74.044502,
                               -73.971496114,
                               -74.013851,
                               -73.981929,
                               -73.993324), 
                        quadrante = c(obterQuadranteLatLon(40.6441666667, -73.7822222222),
                                      obterQuadranteLatLon(40.785383,-73.969336),
                                      obterQuadranteLatLon(40.758896,-73.985130),
                                      obterQuadranteLatLon(40.763262,-73.983067),
                                      obterQuadranteLatLon(40.705576,-74.013421),
                                      obterQuadranteLatLon(40.758740,-73.978674),
                                      obterQuadranteLatLon(40.750782,-73.988959),
                                      obterQuadranteLatLon(40.755604,-73.984932),
                                      obterQuadranteLatLon(40.7070138386,-74.008166634),
                                      obterQuadranteLatLon(40.755603,-73.984931),
                                      obterQuadranteLatLon(40.689247,-74.044502),
                                      obterQuadranteLatLon(40.7749969,-73.971496114),
                                      obterQuadranteLatLon(40.710440,-74.013851),
                                      obterQuadranteLatLon(40.753907,-73.981929),
                                      obterQuadranteLatLon(40.750298,-73.993324)))


######################################## SUBSETS ##########################################

#filtra todas as corridas com origem ou destino para um dos pontos de interesse mapeados
df_subset = df %>% 
  filter(quadrante_origem %in% poi_coord$quadrante |
         quadrante_destino %in% poi_coord$quadrante) -> df_subset



View(df_snowholydays)

##################################### FIM SUBSETS##########################################


#Plot dos pontos de interesse no mapa
b <- get_map("New York,United States",maptype="terrain",source="google",force = ifelse(source == "google", TRUE, TRUE))
ggmap(b) + geom_point(data=poi_coord, aes(x=long[1], y=lat[1], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[2], y=lat[2], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[3], y=lat[3], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[4], y=lat[4], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[5], y=lat[5], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[6], y=lat[6], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[7], y=lat[7], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[8], y=lat[8], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[9], y=lat[9], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[10], y=lat[10], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[11], y=lat[11], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[12], y=lat[12], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[13], y=lat[13], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[14], y=lat[14], color = "blue", size = 2.0), shape = 16, show.legend = FALSE) +
  geom_point(data=poi_coord, aes(x=long[15], y=lat[15], color = "blue", size = 2.0), shape = 16, show.legend = FALSE)


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
#View(df_DOW)

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
#View(df_HOD)

plot_ly(x = df_HOD$hour_pickup, y = df_HOD$count, type = "scatter")

#Conclusão
# a maioria das corridas acontecem entre 18 e 22 horas
# entre 1 e 6 da manha são os horarios com menos corridas


#Plot numero de corridas a cada 15 minutos do dia
# groupby do numero da hora do dia
#Plot numero de corridas a cada 15 minutos do dia
# groupby do numero da hora do dia
groupbyHOD15 <- group_by(df, hourmin_pickup)
countgroupbyHOD15 <- summarise(groupbyHOD15, count = n())
df_HOD15 <- data.frame(c(countgroupbyHOD15))
df_HOD15sort <- df_HOD15[order(df_HOD15$hourmin_pickup),]
#View(df_HOD15sort)

plot_ly(x = df_HOD15sort$hourmin_pickup, y = df_HOD15sort$count, type = "scatter")





#Plot Tempo médio das corridas pela hora do dia
countgroupbyHOD1 <- summarise(groupbyHOD, Mean = mean(trip_duration))
countgroupbyHOD1
#View(countgroupbyHOD1)
plot_ly(x = df_HOD$hour_pickup, y = countgroupbyHOD1$Mean, type = "scatter", showlegend = TRUE, linetype = "1")


# Conclusão
#Apos as 4 da manhã até as 8 da manhã, o tempo médio de uma corrida é menor que os outros horarios
#Das 10 as 18 horas, o tempo médio de uma corrida é maior do que os outros horarios

#verifica se as colunas de latitude e longitude não tem registro vazio
#head(df)
filter(df, is.na(pickup_longitude))
filter(df, is.na(pickup_latitude))
filter(df, is.na(dropoff_longitude))
filter(df, is.na(dropoff_latitude))


#Grafico para mostrar a quantidade de corridas com 1, 2 ou 3 passageiros ao longo do dia

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

plot_ly(x = df_HODrunw1pax$hour_pickup, y = df_HODrunw1pax$count, type = "scatter", mode = "lines", showlegend = TRUE, name = "com 1 pax") %>%
  add_trace(x = df_HODrunw2pax$hour_pickup, y = df_HODrunw2pax$count, name = "com 2 pax") %>%
  add_trace(x = df_HODrunw3pax$hour_pickup, y = df_HODrunw3pax$count, name = "com 3 pax")

#Conclusão.
#Este grafico mostra que o número de corridas de 1 pessoa é bem maior que as corridas com mais pessoas. a maior taxa de crescimento é entre 2 e 6 da manha.
#Para as corridas com duas ou mais pessoas, o intervalo de horario com maior frequencia é entre 15 e 19 horas.


#Tempo médio da viagem em função do horario
