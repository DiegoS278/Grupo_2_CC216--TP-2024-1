setwd('C:/Users/sdiaz/Documents/upc/data science/Parcial/')
data<-read.csv("hotel_bookings.csv",header=TRUE,stringsAsFactors = FALSE)

str(data)
summary(data)

head(data)

data$hotel<-as.factor(data$hotel)
data$is_canceled<-as.factor(data$is_canceled)
data$meal<-as.factor(data$meal)
data$country<-as.factor(data$country)
data$is_repeated_guest<-as.factor(data$is_repeated_guest)
data$arrival_date_month<-as.factor(data$arrival_date_month)
data$market_segment<-as.factor(data$market_segment)
data$distribution_channel<-as.factor(data$distribution_channel)
data$reserved_room_type<-as.factor(data$reserved_room_type)
data$assigned_room_type<-as.factor(data$assigned_room_type)
data$deposit_type<-as.factor(data$deposit_type)
data$agent<-as.factor(data$agent)
data$company<-as.factor(data$company)
data$customer_type<-as.factor(data$customer_type)
data$reservation_status<-as.factor(data$reservation_status)
data$reservation_status_date<-as.Date(data$reservation_status_date)

data$meal[1:30]
data[1:20,]


cant_vacios<-function(x){
  for (i in 1:ncol(x)) {
    cat("Cantidad de vacíos en ", colnames(x[i])," es: ",sum(x[i]==''),"\n")
  }
}

cant_na<-function(x){
  for (i in 1:ncol(x)) {
    cat("Cantidad N.A. en", colnames(x[i])," es: ",sum(is.na(x[i])),"\n")
  }
}
cant_null<-function(x){
  for (i in 1:ncol(x)) {
    cat("Cantidad de NULL en :", colnames(x[i])," es: ",sum(x[i]=='NULL'),"\n")
  }
}
cant_vacios(data)
cant_na(data)
cant_null(data)a

#solo hay N.A. en child!
#######################
child_na<-function(x){
  for (i in 1:nrow(x)) {
    if (is.na(x$children[i])) {
      
     cat("En la fila ",i," hay N.A. ","\n")
    }
  }
}
child_na(data)
library(dplyr)



data_sin_na<-data%>%filter(!is.na(data$children))

head(clean_data)


cant_na(data_sin_na)
summary(data_sin_na)
str(data_sin_na)


library(ggplot2)


library(dplyr)
################leadtime, dayswaitngi list, adr
ggplot(data_sin_na, aes(x=is_repeated_guest,y = lead_time,fill=hotel)) +
  geom_boxplot()

ggplot(data_sin_na, aes(x = lead_time)) +
  geom_histogram()
ggplot(data_sin_na, aes(x = days_in_waiting_list, y = lead_time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)




outlier_values <- boxplot.stats(data_sin_na$lead_time)$out  # outlier values.
print(outlier_values)
fix_outliers <- function(x, removeNA = TRUE){
  #Calculamos los quantiles 1) por arriba del 5% y por debajo del 95%
  
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

clean_data<-data_sin_na
clean_data$lead_time<-fix_outliers(clean_data$lead_time)

par(mfrow = c(1,2))
boxplot(data_sin_na$lead_time, main = "Lead time con Outliers")
boxplot(clean_data$lead_time, main = "Lead time sin Outliers")

#########################################################
ggplot(data_sin_na, aes(x=is_canceled,y = adr,fill=hotel)) +
  geom_boxplot()


ggplot(clean_data, aes(x=is_canceled,y = adr,fill=hotel)) +
  geom_boxplot()

outlier_values <- boxplot.stats(clean_data$adr)$out  # outlier values.
clean_data$adr<-fix_outliers(clean_data$adr)

par(mfrow = c(1,2))
boxplot(data_sin_na$adr, main = "adr con Outliers")
boxplot(clean_data$adr, main = "adr sin Outliers")

#####################

#1 Cantidad de reservas por tipo de hotel:

# Filtrar solo las filas no canceladas
reservas_activas <- clean_data[clean_data$is_canceled == 0,]

# Crear un resumen de la cantidad de reservas por tipo de hotel
resumen_reservas <- summarise(group_by(reservas_activas, hotel), 
                              Cantidad_Reservas = n())

# Mostrar el resumen
print(resumen_reservas)
ggplot(resumen_reservas, aes(x = hotel, y = Cantidad_Reservas,fill=hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Reservas por Hotel",
       x = "Hotel",
       y = "Cantidad de Reservas")


#2 Tendencia de demanda con el tiempo:

#Convertir arrival_date_year a formato numérico
clean_data$arrival_date_year <- as.numeric(clean_data$arrival_date_year)

#Calcular la cantidad de reservas por año
reservas_por_anio <- clean_data %>%
  group_by(arrival_date_year) %>%
  summarise(Cantidad_Reservas = n())

#Crear un gráfico de línea para mostrar la tendencia de la demanda con el tiempo (por año)
ggplot(reservas_por_anio, aes(x = arrival_date_year, y = Cantidad_Reservas)) +
  geom_line() +
  labs(title = "Tendencia de Demanda con el Tiempo (por Año)", x = "Año", y = "Cantidad de Reservas") +
  theme_minimal()


#3 Temporadas de reservas (alta, media y baja):

# Calcular la cantidad de reservas por semana del año
reservas_por_semana <- clean_data %>%
  group_by(arrival_date_week_number) %>%
  summarise(Cantidad_Reservas = n())

# Obtener el mínimo y el máximo de la cantidad de reservas por semana
min_reservas <- min(reservas_por_semana$Cantidad_Reservas)
max_reservas <- max(reservas_por_semana$Cantidad_Reservas)

# Calcular los puntos de corte para clasificar las semanas
corte_baja <- min_reservas + (max_reservas - min_reservas) * 0.25
corte_media_baja <- min_reservas + (max_reservas - min_reservas) * 0.5
corte_media_alta <- min_reservas + (max_reservas - min_reservas) * 0.75

# Crear una nueva columna para clasificar las semanas
reservas_por_semana$Temporada <- cut(reservas_por_semana$Cantidad_Reservas, 
                                     breaks = c(min_reservas, corte_baja, corte_media_baja, corte_media_alta, max_reservas),
                                     labels = c("Baja", "Media Baja", "Media Alta", "Alta"),
                                     include.lowest = TRUE,
                                     right = TRUE)

# Crear un gráfico de barras para mostrar las temporadas de reservas por semana
ggplot(data = reservas_por_semana, aes(x = as.factor(arrival_date_week_number), y = Cantidad_Reservas, fill = Temporada)) +
  geom_bar(stat = "identity") +
  labs(title = "Temporadas de Reservas por Semana", x = "Semana del Año", y = "Cantidad de Reservas", fill = "Temporada") +
  theme_minimal()


print((reservas_por_semana))

# Calcular la cantidad de reservas por semana del año
reservas_por_semana <- data %>%
  group_by(arrival_date_week_number) %>%
  summarise(Cantidad_Reservas = n())

# Obtener el mínimo y el máximo de las semanas del año
min_semana_reservas <- min(reservas_por_semana$arrival_date_week_number)
max_semana_reservas <- max(reservas_por_semana$arrival_date_week_number)

# Calcular los puntos de corte para clasificar las semanas
corte_baja_reservas <- min_semana_reservas + ((max_semana_reservas - min_semana_reservas) / 4)  # 25% de las semanas
corte_media_baja_reservas <- min_semana_reservas + ((max_semana_reservas - min_semana_reservas) / 2)  # 50% de las semanas
corte_media_alta_reservas <- min_semana_reservas + ((max_semana_reservas - min_semana_reservas) * 3 / 4)  # 75% de las semanas

# Crear una nueva columna para clasificar las semanas
reservas_por_semana$Temporada_Reservas <- cut(reservas_por_semana$arrival_date_week_number, 
                                              breaks = c(min_semana_reservas, corte_baja_reservas, corte_media_baja_reservas, corte_media_alta_reservas, max_semana_reservas),
                                              labels = c("Baja", "Media Baja", "Media Alta", "Alta"),
                                              include.lowest = TRUE,
                                              right = TRUE)

# Imprimir en la consola los intervalos de cada temporada
cat("Intervalos para las temporadas de reservas:\n")
cat("Baja:", min_semana_reservas, "-", corte_baja_reservas, "\n")
cat("Media Baja:", corte_baja_reservas, "-", corte_media_baja_reservas, "\n")
cat("Media Alta:", corte_media_baja_reservas, "-", corte_media_alta_reservas, "\n")
cat("Alta:", corte_media_alta_reservas, "-", max_semana_reservas, "\n")









#4 ¿Cuándo es menor la demanda de reservas? 

# Calcular la cantidad de reservas por semana del año
reservas_por_semana <- clean_data %>%
  group_by(arrival_date_week_number) %>%
  summarise(Cantidad_Reservas = n())

# Encontrar la semana con la menor cantidad de reservas
semana_menor_reservas <- reservas_por_semana %>%
  filter(Cantidad_Reservas == min(Cantidad_Reservas)) %>%
  pull(arrival_date_week_number)

# Imprimir el resumen de la temporada con la menor cantidad de reservas
cat("La temporada con la menor cantidad de reservas es la semana", semana_menor_reservas, "\n")










### Pregunta 5: ¿Cuántas reservas incluyen niños y/o bebés?
reservas_con_ninos <- sum(clean_data$children > 0) +
  sum(clean_data$babies > 0)

cat("El número de reservas que incluyen niños y/o bebés es:", reservas_con_ninos, "\n")


# Calcula el número de reservas en cada categoría
clean_data1<-clean_data
clean_data1$con_niños <- ifelse(clean_data$children > 0, "Con Niños", "Sin Niños")
clean_data1$con_bebés <- ifelse(clean_data$babies > 0, "Con Bebés", "Sin Bebés")

clean_data1$categoria <- ifelse(clean_data$children > 0 & clean_data$babies > 0, "Con Niños y Bebés",
                             ifelse(clean_data$children > 0 & clean_data$babies == 0, "Solo con Niños",
                                    ifelse(clean_data$children == 0 & clean_data$babies > 0, "Solo con Bebés", "Sin Niños ni Bebés")))

# Crea el gráfico de barras
ggplot(clean_data1, aes(x = categoria, fill = categoria)) +
  geom_bar() +
  labs(x = "Categoría de Reserva", y = "Cantidad de Reservas", title = "Comparación de Reservas por Categoría") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Con Niños" = "blue", "Con Bebés" = "red", "Con Niños y Bebés" = "green", "Sin Niños ni Bebés" = "gray"))










# Pregunta vi: ¿Es importante contar con espacios de estacionamiento?
espacios_estacionamiento <- sum(clean_data$required_car_parking_spaces > 0)
porcentaje_con_estacionamiento <- (espacios_estacionamiento / nrow(clean_data)) * 100
cat("El", porcentaje_con_estacionamiento, "% de las reservas requieren espacios de estacionamiento.\n")

ggplot(clean_data, aes(x = required_car_parking_spaces > 0)) +
  geom_bar() +
  labs(x = "Parking Requerido", y = "Cantidad de Personas", title = "Cantidad de Personas con Parking Requerido") +
  scale_x_discrete(labels = c("0" = "Igual a 0", "1" = "Mayor que 0"))

## Pregunta vii: ¿En qué meses del año se producen más cancelaciones de reservas?
library(dplyr)

# Filtrar las cancelaciones
cancelaciones_por_mes <- clean_data %>%
  filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(cancelaciones = n())

# Ordenando los meses por número de cancelaciones (de mayor a menor)
cancelaciones_por_mes <- cancelaciones_por_mes %>%
  arrange(desc(cancelaciones))

# Mostramos los tres meses con más cancelaciones
cat("Los tres meses con más cancelaciones de reservas son:\n")
print(head(cancelaciones_por_mes, 3))

ggplot(cancelaciones_por_mes, aes(x =arrival_date_month   , y = cancelaciones)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Meses con más cancelaciones", x = "Meses", y = "Cancelaciones") +
  theme_minimal()





save.image(file = "avance.RData")
