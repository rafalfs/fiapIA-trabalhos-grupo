
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



#principais origens e plot em relação a horarios
# code para agrupar as colunas coord_dropoff e coord_pickup e achar os principais origens e destinos
#dfmaindest <- group_by(df, coord_dropoff)
#dfmaindest <- summarise(dfmaindest, count = n())
#dfmaindest <- filter(dfmaindest, count == 4)
#View(dfmaindest)

################################################################################################

#principais Origens em função do horario


#Laguardia Airport
Local1 <- c("40.7737693786621,-73.8709335327148",
            "40.7737617492676,-73.8708724975586",
            "40.8210029602051,-73.9546661376953",
            "40.7741813659668,-73.8730087280273",
            "40.7737007141113,-73.8708724975586",
            "40.7737197875977,-73.8708190917969",
            "40.7737197875977,-73.8708419799805",
            "40.7737197875977,-73.8708572387695",
            "40.7737312316895,-73.870849609375",
            "40.7737312316895,-73.8708572387695",
            "40.7737312316895,-73.8708801269531",
            "40.773738861084,-73.8708724975586",
            "40.773738861084,-73.8708801269531",
            "40.7737503051758,-73.8708877563477",
            "40.7737617492676,-73.8708724975586",
            "40.7737617492676,-73.8708877563477",
            "40.7737808227539,-73.8708724975586",
            "40.7741317749023,-73.8730316162109",
            "40.774169921875,-73.8729782104492",
            "40.7742195129395,-73.8730621337891",
            "40.7699584960938,-73.8633270263672",
            "40.7700119018555,-73.8633499145508",
            "40.7741279602051,-73.8730163574219",
            "40.7742004394531,-73.8728790283203",
            "40.7737350463867,-73.8709030151367",
            "40.7741508483887,-73.8730392456055",
            "40.7742004394531,-73.8729629516602",
            "40.7742004394531,-73.8729934692383")
dfLocal1 <- subset(df, coord_pickup %in% Local1)
dfmaindest1 <- group_by(dfLocal1, hour_pickup)
dfmaindest1 <- summarise(dfmaindest1, count = n())
plot_ly(x = dfmaindest1$hour_pickup, y = dfmaindest1$count, type = "scatter", showlegend = TRUE, linetype = "1")


#JFK Airport
Local2 <- c("40.6447486877441,-73.781852722168",
            "40.6453018188477,-73.7767181396484",
            "40.6453514099121,-73.7766723632812",
            "40.6453704833984,-73.7767105102539",
            "40.6446304321289,-73.7819900512695",
            "40.6446418762207,-73.7818603515625",
            "40.644660949707,-73.7818832397461",
            "40.6446838378906,-73.7818603515625",
            "40.6446914672852,-73.7817764282227",
            "40.6447486877441,-73.7817993164062",
            "40.6452407836914,-73.7767181396484",
            "40.6453132629395,-73.7766876220703",
            "40.645393371582,-73.7767181396484",
            "40.641529083252,-73.787956237793",
            "40.6445732116699,-73.7822341918945",
            "40.6447715759277,-73.7817764282227",
            "40.6453399658203,-73.7767333984375")
dfLocal2 <- subset(df, coord_pickup %in% Local2)
dfmaindest2 <- group_by(dfLocal2, hour_pickup)
dfmaindest2 <- summarise(dfmaindest2, count = n())
plot_ly(x = dfmaindest2$hour_pickup, y = dfmaindest2$count, type = "scatter", showlegend = TRUE, linetype = "1")


#Broadway Street
Local3 <- c("40.8106307983398,-73.9620208740234",
            "40.8147506713867,-73.9592132568359",
            "40.8150291442871,-73.9590301513672",
            "40.8043594360352,-73.9666976928711",
            "40.8030281066895,-73.9675903320312",
            "40.7973098754883,-73.9698867797852",
            "40.7948684692383,-73.9714889526367",
            "40.8210029602051,-73.9546661376953")
dfLocal3 <- subset(df, coord_pickup %in% Local3)
dfmaindest3 <- group_by(dfLocal3, hour_pickup)
dfmaindest3 <- summarise(dfmaindest3, count = n())
plot_ly(x = dfmaindest3$hour_pickup, y = dfmaindest3$count, type = "scatter", showlegend = TRUE, linetype = "1")

#Pensilvania Train Station
Local4 <- c("40.7510986328125,-73.9941177368164","40.7511596679688,-73.9940719604492","40.7512016296387,-73.9940719604492")
dfLocal4 <- subset(df, coord_pickup %in% Local4)
dfmaindest4 <- group_by(dfLocal4, hour_pickup)
dfmaindest4 <- summarise(dfmaindest4, count = n())
plot_ly(x = dfmaindest4$hour_pickup, y = dfmaindest4$count, type = "scatter", showlegend = TRUE, linetype = "1")


#Central Park
Local5 <- c("40.7763595581055,-73.9759368896484",
            "40.7511596679688,-73.9940719604492",
            "40.7512016296387,-73.9940719604492")
dfLocal5 <- subset(df, coord_pickup %in% Local5)
dfmaindest5 <- group_by(dfLocal5, hour_pickup)
dfmaindest5 <- summarise(dfmaindest5, count = n())
plot_ly(x = dfmaindest5$hour_pickup, y = dfmaindest5$count, type = "scatter", showlegend = TRUE, linetype = "1")




################################################################################################

#principais destinos em função do horario


#Envio de Valores La Nacion
Local6 <- c("40.8210029602051,-73.9546661376953")
dfLocal6 <- subset(df, coord_dropoff %in% Local6)
dfmaindest6 <- group_by(dfLocal6, hour_dropoff)
dfmaindest6 <- summarise(dfmaindest6, count = n())
plot_ly(x = dfmaindest6$hour_dropoff, y = dfmaindest6$count, type = "scatter", showlegend = TRUE, linetype = "1")


#Ferry Terminal
Local7 <- c("40.7605781555176,-74.0027694702148",
            "40.7605514526367,-74.0027694702148",
            "40.7605094909668,-74.0028228759766",
            "40.7605934143066,-74.0027694702148",
            "40.7606010437012,-74.0027236938477",
            "40.7606086730957,-74.0027770996094",
            "40.7606201171875,-74.0027618408203",
            "40.7606201171875,-74.0027694702148",
            "40.7606391906738,-74.0027236938477",
            "40.7606430053711,-74.0027084350586",
            "40.7606506347656,-74.0026779174805",
            "40.7606506347656,-74.0027313232422",
            "40.7606086730957,-74.0027618408203")
dfLocal7 <- subset(df, coord_dropoff %in% Local7)
dfmaindest7 <- group_by(dfLocal7, hour_dropoff)
dfmaindest7 <- summarise(dfmaindest7, count = n())
plot_ly(x = dfmaindest7$hour_dropoff, y = dfmaindest7$count, type = "scatter", showlegend = TRUE, linetype = "1")

#Pensilvania Station
Local8 <- c("40.7503890991211,-73.9946823120117",
            "40.7503700256348,-73.9946670532227",
            "40.7504081726074,-73.9946594238281",
            "40.7504615783691,-73.9946670532227",
            "40.7493209838867,-73.9921493530273",
            "40.7501487731934,-73.9912643432617",
            "40.7504386901855,-73.9946365356445",
            "40.7504615783691,-73.9946212768555",
            "40.7500190734863,-73.9915084838867",
            "40.7501182556152,-73.9948883056641",
            "40.7501411437988,-73.9948806762695",
            "40.7501411437988,-73.9949111938477",
            "40.7492980957031,-73.9921875",
            "40.7500534057617,-73.9949264526367",
            "40.7500915527344,-73.9912719726562",
            "40.7501182556152,-73.9913787841797",
            "40.7501182556152,-73.9914016723633",
            "40.7501411437988,-73.9913101196289",
            "40.7501411437988,-73.9948883056641",
            "40.7501487731934,-73.9915237426758",
            "40.7501792907715,-73.9948196411133",
            "40.7501983642578,-73.9948501586914",
            "40.7493209838867,-73.9921417236328",
            "40.7499313354492,-73.9914932250977",
            "40.75,-73.9914016723633")
dfLocal8 <- subset(df, coord_dropoff %in% Local8)
dfmaindest8 <- group_by(dfLocal8, hour_dropoff)
dfmaindest8 <- summarise(dfmaindest8, count = n())
plot_ly(x = dfmaindest8$hour_dropoff, y = dfmaindest8$count, type = "scatter", showlegend = TRUE, linetype = "1")


#La Guardia Airport
Local9 <- c("40.7683982849121,-73.8617477416992",
            "40.7683982849121,-73.8617782592773",
            "40.7684097290039,-73.86181640625",
            "40.7694854736328,-73.8633193969727",
            "40.7705917358398,-73.8651275634766",
            "40.7704010009766,-73.8649063110352",
            "40.7704086303711,-73.8648986816406",
            "40.7706413269043,-73.8652191162109",
            "40.7708892822266,-73.8655776977539",
            "40.7739791870117,-73.8707275390625",
            "40.7740783691406,-73.8709030151367",
            "40.7741889953613,-73.8710174560547",
            "40.7684211730957,-73.8617935180664",
            "40.7684211730957,-73.8618011474609",
            "40.7684211730957,-73.8618469238281",
            "40.7684593200684,-73.8619079589844",
            "40.7684783935547,-73.8618392944336",
            "40.7685012817383,-73.8618927001953",
            "40.7685394287109,-73.861930847168",
            "40.7685508728027,-73.8619232177734",
            "40.7740211486816,-73.8707504272461",
            "40.7740516662598,-73.8708419799805",
            "40.7740783691406,-73.8708877563477",
            "40.7740898132324,-73.8709182739258",
            "40.7743110656738,-73.8726501464844",
            "40.7743110656738,-73.8727493286133",
            "40.7743110656738,-73.872802734375",
            "40.7743186950684,-73.8726196289062",
            "40.7743186950684,-73.8727188110352",
            "40.7743301391602,-73.8726806640625",
            "40.774341583252,-73.8726806640625",
            "40.7745018005371,-73.8725509643555",
            "40.774528503418,-73.8724594116211",
            "40.7686004638672,-73.8620376586914",
            "40.7686195373535,-73.8620834350586",
            "40.7702598571777,-73.8647079467773",
            "40.7703552246094,-73.8648147583008",
            "40.7704086303711,-73.864860534668",
            "40.7743110656738,-73.8727874755859",
            "40.7683715820312,-73.8617935180664")
dfLocal9 <- subset(df, coord_dropoff %in% Local9)
dfmaindest9 <- group_by(dfLocal9, hour_dropoff)
dfmaindest9 <- summarise(dfmaindest9, count = n())
plot_ly(x = dfmaindest9$hour_dropoff, y = dfmaindest9$count, type = "scatter", showlegend = TRUE, linetype = "1")

#JFK Airport
Local10 <- c("40.6469383239746,-73.7899398803711",
             "40.6437683105469,-73.7833862304688",
             "40.6453514099121,-73.7766723632812",
             "38.8988494873047,-77.039436340332",
             "40.6441192626953,-73.7825393676758",
             "40.6454086303711,-73.7763366699219",
             "40.6454696655273,-73.7763137817383",
             "40.6456718444824,-73.7763214111328",
             "40.6468505859375,-73.7900924682617",
             "40.6468658447266,-73.7901077270508",
             "40.6468811035156,-73.7901306152344",
             "40.646900177002,-73.7900619506836",
             "40.6469306945801,-73.7899932861328",
             "40.6469917297363,-73.7898406982422",
             "40.6469993591309,-73.7898178100586",
             "40.6446304321289,-73.7819900512695",
             "40.6453399658203,-73.7767333984375")
dfLocal10 <- subset(df, coord_dropoff %in% Local10)
dfmaindest10 <- group_by(dfLocal10, hour_dropoff)
dfmaindest10 <- summarise(dfmaindest10, count = n())
plot_ly(x = dfmaindest10$hour_dropoff, y = dfmaindest10$count, type = "scatter", showlegend = TRUE, linetype = "1")

