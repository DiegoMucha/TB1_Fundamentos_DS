# Instalar librerias
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")

# Importamos librerias
library("dplyr")
library("tidyr")
library("ggplot2")

# Guardamos el dataset en una variable
df <- read.csv("hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)

# Ver la cantidad de filas y columnas en el dataset
dim(df)

# Ahora vemos un resumen de los datos
str(df)

# Con esto podemos filtrar la cantidad exacta de valores null, na o undefined que hay en las filas del dataset
is_missing <- function(x) {
  is.na(x) | x == "NULL" | x == "Undefined"
}
# is.na(x) | x == "NULL" | x == "UNDEFINED" |
missing_logical <- apply(df, 2, is_missing)

na_count_extended <- colSums(missing_logical)
variables_with_missing <- na_count_extended[na_count_extended > 0]

variables_with_missing_sorted <- sort(variables_with_missing, decreasing = TRUE)
print(variables_with_missing_sorted)

# Podemos ver los valores únicos de cada varible
df %>%
  summarise(across(everything(), ~ paste(unique(.), collapse = ", "))) %>%
  pivot_longer(cols = everything(), names_to = "Columna", values_to = "Valores Únicos")
 
# Reemplazamos Valores con 0, None o con la moda(en caso sean repeticiones pequeñas)
df$agent[df$agent == "NULL"] <- 0
df$company[df$company == "NULL"] <- 0
df$meal[df$meal == "Undefined"] <- 'None'
df$children[is.na(df$children)] <- 0

moda_distribution_channel <- names(sort(table(df$distribution_channel), decreasing = TRUE))[1]
df$distribution_channel[df$distribution_channel == 'Undefined'] <- moda_distribution_channel

moda_country <- names(sort(table(df$country), decreasing = TRUE))[1]
df$country[df$country == 'NULL'] <- moda_country

moda_market <- names(sort(table(df$market_segment), decreasing = TRUE))[1]
df$market_segment[df$market_segment == 'Undefined'] <- moda_market

moda_market <- names(sort(table(df$market_segment), decreasing = TRUE))[1]
df$market_segment[df$market_segment == 'Undefined'] <- moda_market

is_missing <- function(x) {
  is.na(x) | x == "NULL" | x == "Undefined"
}
# is.na(x) | x == "NULL" | x == "UNDEFINED" |
missing_logical <- apply(df, 2, is_missing)

na_count_extended <- colSums(missing_logical)

variables_with_missing <- na_count_extended[na_count_extended > 0]

variables_with_missing_sorted <- sort(variables_with_missing, decreasing = TRUE)

print(variables_with_missing_sorted)

### Ahora vamos a analizar y tratar los valores atípicos
# Hacemos un bucle para hacer los boxplots para cada variable de nuestro dataset y así ver si contienen valores atípicos
for (colname in names(df)) {
  if (is.numeric(df[[colname]])) {
    boxplot(df[[colname]],
            main = paste("Boxplot de", colname),
            ylab = colname)
  }
}

# Ahora hacemos un bucle para ver los histogramas de cada variable y así guiarnos para ver si tienen valores atípicos
for (colname in names(df)) {
  if (is.numeric(df[[colname]])) {
    hist(df[[colname]],
         main = paste("Histograma de", colname),
         xlab = colname,
         col = "lightblue",
         border = "white")
  }
}

# Columna con valores atípicos N1# - lead_time
columna <- "lead_time"

# Crear una copia del dataset original
dataset_original <- df

# Aplicar winsorización
percentil <- quantile(df[[columna]], 0.98, na.rm = TRUE)
df[[columna]][df[[columna]] > percentil] <- percentil

# Comparar boxplots antes y después de la winsorización
par(mfrow = c(1, 2)) # Dividir la ventana en 2 columnas

# Boxplot antes de la winsorización
boxplot(dataset_original[[columna]], main = "Boxplot Original")

# Boxplot después de la winsorización
boxplot(df[[columna]], main = "Boxplot Winsorizado")

par(mfrow = c(1, 1)) # Restaurar la vista normal

# Columna con valores atípicos N2 - stays_in_weekend_nights
estancia_noche <- "stays_in_weekend_nights"
dataset_original <- df

percentil <- quantile(df[[estancia_noche]], 0.99, na.rm = TRUE)
df[[estancia_noche]][df[[estancia_noche]] > percentil] <- percentil

par(mfrow = c(1, 2))

boxplot(dataset_original[[estancia_noche]], main = "Stays in Weekend Nights Original")
boxplot(df[[estancia_noche]], main = "Stays in Weekend Nights Winsorizado")

par(mfrow = c(1, 1))
  
# Columna con valores atípicos N3 - stays_in_week_nights
estancia_noche2 <- "stays_in_week_nights"
dataset_original <- df

percentil <- quantile(df[[estancia_noche2]], 0.97, na.rm = TRUE)
df[[estancia_noche2]][df[[estancia_noche2]] > percentil] <- percentil

par(mfrow = c(1, 2))

boxplot(dataset_original[[estancia_noche2]], main = "Stays in Week Nights")
boxplot(df[[estancia_noche2]], main = "Stays in Week Nights Winsorizado")

par(mfrow = c(1, 1))

#Columna con valores atípicos N4 - adults
adultos <- "adults"
dataset_original <- df

percentil_superior <- quantile(df[[adultos]], 0.94, na.rm = TRUE)
percentil_inferior <- quantile(df[[adultos]], 0.2, na.rm = TRUE)

df[[adultos]][df[[adultos]] > percentil_superior] <- percentil_superior
df[[adultos]][df[[adultos]] < percentil_inferior] <- percentil_inferior

par(mfrow = c(1, 2))

boxplot(dataset_original[[adultos]], main = "Adultos")
boxplot(df[[adultos]], main = "Adultos Winsorizado")

par(mfrow = c(1, 1))

# Columna con valores atípicos N5 - children
niñitos <- "children"
dataset_original <- df

percentil_superior <- quantile(df[[niñitos]], 0.92, na.rm = TRUE)
df[[niñitos]][df[[niñitos]] > percentil_superior] <- percentil_superior

par(mfrow = c(1, 2))

boxplot(dataset_original[[niñitos]], main = "Niños")
boxplot(df[[niñitos]], main = "Niños Winsorizado")

par(mfrow = c(1, 1))

# Columna con valores atípicos N6 - babies
bebitos <- "babies"
dataset_original <- df

percentil_superior <- quantile(df[[bebitos]], 0.99, na.rm = TRUE)
df[[bebitos]][df[[bebitos]] > percentil_superior] <- percentil_superior

par(mfrow = c(1, 2))

boxplot(dataset_original[[bebitos]], main = "Bebitos")
boxplot(df[[bebitos]], main = "Bebitos Winsorizado")

par(mfrow = c(1, 1))

# Columna con valores atípicos N7 - previous_cancellations
cancelaciones_p <- "previous_cancellations"
dataset_original <- df

percentil_superior <- quantile(df[[cancelaciones_p]], 0.94, na.rm = TRUE)
df[[cancelaciones_p]][df[[cancelaciones_p]] > percentil_superior] <- percentil_superior

par(mfrow = c(1, 2))

boxplot(dataset_original[[cancelaciones_p]], main = "Cancelaciones Previas")
boxplot(df[[cancelaciones_p]], main = "Cancelaciones Previas Winsorizado")

par(mfrow = c(1, 1))

# Columna con valores atípicos N8 - previous_bookings_not_canceled
no_cancelados <- "previous_bookings_not_canceled"
dataset_original <- df

percentil_superior <- quantile(df[[no_cancelados]], 0.96, na.rm = TRUE)
df[[no_cancelados]][df[[no_cancelados]] > percentil_superior] <- percentil_superior

par(mfrow = c(1, 2))

boxplot(dataset_original[[no_cancelados]], main = "No Cancelados")
boxplot(df[[no_cancelados]], main = "No Cancelados Winsorizado")

par(mfrow = c(1, 1))

# Columna con valores atípicos N9 - total_of_special_requests
requests_especiales <- "total_of_special_requests"
dataset_original <- df

percentil_superior <- quantile(df[[requests_especiales]], 0.97, na.rm = TRUE)
df[[requests_especiales]][df[[requests_especiales]] > percentil_superior] <- percentil_superior

par(mfrow = c(1, 2))

boxplot(dataset_original[[requests_especiales]], main = "Especiales")
boxplot(df[[requests_especiales]], main = "Especiales Winsorizado")

par(mfrow = c(1, 1))

# Por último escribimos el nuevo dataset como .csv
write.csv(df, "hotel_bookings_limpio.csv", row.names = FALSE)