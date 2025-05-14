library(dplyr)
library(ggplot2)

# Asume que el dataset está cargado como "df"
df <- read.csv("hotel_bookings_limpio.csv")

# 1. ANÁLISIS DE CANCELACIONES
# -----------------------------

# Lead Time vs Cancelación
# BoxPlot
ggplot(df, aes(x = factor(is_canceled), y = lead_time, fill = factor(is_canceled))) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "salmon"), labels = c("No Cancelada", "Cancelada")) +
  labs(title = "Lead Time por Estado de Cancelación", 
       x = "Cancelación", 
       y = "Lead Time (días)",
       fill = "Estado") +
  scale_x_discrete(labels = c("No Cancelada", "Cancelada"))

# Histograma de Lead Time por estado de cancelación
ggplot(df, aes(x = lead_time, fill = factor(is_canceled))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "dodge") +
  scale_fill_manual(values = c("lightblue", "salmon"), labels = c("No Cancelada", "Cancelada")) +
  labs(title = "Distribución de Lead Time por Estado de Cancelación", 
       x = "Lead Time (días)", 
       y = "Conteo",
       fill = "Estado")

# Tasa de cancelación por mes
cancelacion_por_mes <- df %>%
  group_by(arrival_date_month) %>%
  summarise(tasa_cancelacion = mean(is_canceled)) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

ggplot(cancelacion_por_mes, aes(x = arrival_date_month, y = tasa_cancelacion)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Tasa de Cancelación por Mes", 
       x = "Mes", 
       y = "Tasa de Cancelación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Líneas: Evolución de cancelaciones a lo largo del año por tipo de hotel
cancelacion_por_mes_hotel <- df %>%
  group_by(arrival_date_month, hotel) %>%
  summarise(tasa_cancelacion = mean(is_canceled)) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

ggplot(cancelacion_por_mes_hotel, aes(x = arrival_date_month, y = tasa_cancelacion, group = hotel, color = hotel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Evolución de Cancelaciones por Tipo de Hotel", 
       x = "Mes", 
       y = "Tasa de Cancelación",
       color = "Tipo de Hotel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cancelaciones por tipo de cliente
cancelacion_por_cliente <- df %>%
  group_by(customer_type) %>%
  summarise(tasa_cancelacion = mean(is_canceled))

ggplot(cancelacion_por_cliente, aes(x = customer_type, y = tasa_cancelacion, fill = customer_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Tasa de Cancelación por Tipo de Cliente", 
       x = "Tipo de Cliente", 
       y = "Tasa de Cancelación") +
  theme(legend.position = "none")

# PieChart: Distribución de cancelaciones por canal de distribución
cancelaciones_por_canal <- df %>%
  filter(is_canceled == 1) %>%
  group_by(distribution_channel) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(cancelaciones_por_canal, aes(x = "", y = percentage, fill = distribution_channel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de Cancelaciones por Canal", 
       fill = "Canal de Distribución") +
  theme_void()

# 2. ANÁLISIS DE RENTABILIDAD
# ---------------------------

# ADR por tipo de hotel
ggplot(df, aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot() +
  labs(title = "ADR por Tipo de Hotel", 
       x = "Tipo de Hotel", 
       y = "ADR") +
  theme(legend.position = "none")

# Histograma: Distribución de ADR para cada tipo de hotel
ggplot(df, aes(x = adr, fill = hotel)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "dodge") +
  labs(title = "Distribución de ADR por Tipo de Hotel", 
       x = "ADR", 
       y = "Conteo",
       fill = "Tipo de Hotel")

# ADR promedio por mes y tipo de hotel
adr_mes_hotel <- df %>%
  group_by(arrival_date_month, hotel) %>%
  summarise(adr_promedio = mean(adr, na.rm = TRUE)) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

ggplot(adr_mes_hotel, aes(x = arrival_date_month, y = adr_promedio, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "ADR Promedio por Mes y Tipo de Hotel", 
       x = "Mes", 
       y = "ADR Promedio",
       fill = "Tipo de Hotel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# BoxPlot: ADR por tipo de habitación reservada
ggplot(df, aes(x = reserved_room_type, y = adr, fill = reserved_room_type)) +
  geom_boxplot() +
  labs(title = "ADR por Tipo de Habitación Reservada", 
       x = "Tipo de Habitación", 
       y = "ADR") +
  theme(legend.position = "none")

# Rentabilidad por duración de estadía
df$total_nights <- df$stays_in_weekend_nights + df$stays_in_week_nights

# Líneas: ADR promedio por duración de estadía total
adr_por_estadia <- df %>%
  group_by(total_nights) %>%
  summarise(adr_promedio = mean(adr, na.rm = TRUE)) %>%
  filter(total_nights <= 14)  # Filtrar estadías extremadamente largas

ggplot(adr_por_estadia, aes(x = total_nights, y = adr_promedio)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  labs(title = "ADR Promedio por Duración de Estadía", 
       x = "Noches Totales", 
       y = "ADR Promedio")

# 3. ANÁLISIS DE COMPORTAMIENTO DEL CLIENTE
# -----------------------------------------

# PieChart: Top 10 países con más huéspedes repetidos
top_paises_repetidos <- df %>%
  filter(is_repeated_guest == 1) %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(top_paises_repetidos, aes(x = "", y = count, fill = country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Top 10 Países con Huéspedes Repetidos", 
       fill = "País") +
  theme_void()

# BarPlot: Tasa de huéspedes repetidos por tipo de hotel
repetidos_por_hotel <- df %>%
  group_by(hotel) %>%
  summarise(tasa_repetidos = mean(is_repeated_guest))

ggplot(repetidos_por_hotel, aes(x = hotel, y = tasa_repetidos, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Tasa de Huéspedes Repetidos por Tipo de Hotel", 
       x = "Tipo de Hotel", 
       y = "Tasa de Repetición") +
  theme(legend.position = "none")

# Histograma: Distribución de solicitudes especiales
ggplot(df, aes(x = total_of_special_requests)) +
  geom_histogram(bins = 10, fill = "lightgreen", color = "black") +
  labs(title = "Distribución de Solicitudes Especiales", 
       x = "Número de Solicitudes Especiales", 
       y = "Conteo")

# BoxPlot: Solicitudes especiales por tipo de cliente
ggplot(df, aes(x = customer_type, y = total_of_special_requests, fill = customer_type)) +
  geom_boxplot() +
  labs(title = "Solicitudes Especiales por Tipo de Cliente", 
       x = "Tipo de Cliente", 
       y = "Número de Solicitudes Especiales") +
  theme(legend.position = "none")

# 4. ANÁLISIS OPERACIONAL
# -----------------------

# BarPlot: Proporción de habitaciones asignadas vs. reservadas
df$cambio_habitacion <- ifelse(df$reserved_room_type == df$assigned_room_type, "Sin Cambio", "Con Cambio")
cambio_habitacion_count <- table(df$cambio_habitacion)

cambio_df <- data.frame(
  estado = names(cambio_habitacion_count),
  conteo = as.numeric(cambio_habitacion_count)
)

ggplot(cambio_df, aes(x = estado, y = conteo, fill = estado)) +
  geom_bar(stat = "identity") +
  labs(title = "Cambios de Habitación", 
       x = "Estado", 
       y = "Conteo") +
  theme(legend.position = "none")

# Líneas: Promedio de días en lista de espera por mes
espera_por_mes <- df %>%
  group_by(arrival_date_month) %>%
  summarise(dias_promedio = mean(days_in_waiting_list, na.rm = TRUE)) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

ggplot(espera_por_mes, aes(x = arrival_date_month, y = dias_promedio, group = 1)) +
  geom_line(size = 1, color = "purple") +
  geom_point(size = 3, color = "purple") +
  labs(title = "Días Promedio en Lista de Espera por Mes", 
       x = "Mes", 
       y = "Días Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# BoxPlot: Días en lista de espera por tipo de hotel
ggplot(df, aes(x = hotel, y = days_in_waiting_list, fill = hotel)) +
  geom_boxplot() +
  labs(title = "Días en Lista de Espera por Tipo de Hotel", 
       x = "Tipo de Hotel", 
       y = "Días en Lista de Espera") +
  theme(legend.position = "none")

# 5. ANÁLISIS DE ESTACIONALIDAD
# ----------------------------

# Líneas: Reservas por mes para cada tipo de hotel
reservas_por_mes_hotel <- df %>%
  group_by(arrival_date_month, hotel) %>%
  summarise(count = n()) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

ggplot(reservas_por_mes_hotel, aes(x = arrival_date_month, y = count, group = hotel, color = hotel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Reservas por Mes y Tipo de Hotel", 
       x = "Mes", 
       y = "Número de Reservas",
       color = "Tipo de Hotel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Líneas: ADR promedio por mes
adr_por_mes <- df %>%
  group_by(arrival_date_month) %>%
  summarise(adr_promedio = mean(adr, na.rm = TRUE)) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

ggplot(adr_por_mes, aes(x = arrival_date_month, y = adr_promedio, group = 1)) +
  geom_line(size = 1, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "ADR Promedio por Mes", 
       x = "Mes", 
       y = "ADR Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# BoxPlot: Variación de ADR por mes
ggplot(df, aes(x = arrival_date_month, y = adr)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Variación de ADR por Mes", 
       x = "Mes", 
       y = "ADR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. ANÁLISIS DE IMPACTO EN INGRESOS
# ---------------------------------

# BoxPlot: ADR por número de solicitudes especiales
ggplot(df, aes(x = factor(total_of_special_requests), y = adr)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "ADR por Número de Solicitudes Especiales", 
       x = "Número de Solicitudes Especiales", 
       y = "ADR")

# Líneas: Relación entre solicitudes especiales y ADR
adr_por_solicitudes <- df %>%
  group_by(total_of_special_requests) %>%
  summarise(adr_promedio = mean(adr, na.rm = TRUE)) %>%
  filter(total_of_special_requests <= 5)  # Filtrar valores extremos

ggplot(adr_por_solicitudes, aes(x = total_of_special_requests, y = adr_promedio)) +
  geom_line(size = 1, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "ADR Promedio por Número de Solicitudes Especiales", 
       x = "Número de Solicitudes Especiales", 
       y = "ADR Promedio")

# BoxPlot: ADR por tipo de depósito
ggplot(df, aes(x = deposit_type, y = adr, fill = deposit_type)) +
  geom_boxplot() +
  labs(title = "ADR por Tipo de Depósito", 
       x = "Tipo de Depósito", 
       y = "ADR") +
  theme(legend.position = "none")

# 7. ANÁLISIS DE SEGMENTACIÓN DE CLIENTES
# --------------------------------------

# BarPlot: Top 10 países por número de reservas
top_paises <- df %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(top_paises, aes(x = reorder(country, -count), y = count, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Países por Número de Reservas", 
       x = "País", 
       y = "Número de Reservas") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# BoxPlot: ADR por país de origen (top 10)
top_paises_adr <- df %>%
  filter(country %in% top_paises$country) %>%
  select(country, adr)

ggplot(top_paises_adr, aes(x = country, y = adr, fill = country)) +
  geom_boxplot() +
  labs(title = "ADR por País de Origen (Top 10)", 
       x = "País", 
       y = "ADR") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# BoxPlot: ADR por tipo de cliente
ggplot(df, aes(x = customer_type, y = adr, fill = customer_type)) +
  geom_boxplot() +
  labs(title = "ADR por Tipo de Cliente", 
       x = "Tipo de Cliente", 
       y = "ADR") +
  theme(legend.position = "none")

# Histograma: Distribución de lead time por segmento de mercado
ggplot(df, aes(x = lead_time, fill = market_segment)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "dodge") +
  labs(title = "Distribución de Lead Time por Segmento de Mercado", 
       x = "Lead Time (días)", 
       y = "Conteo",
       fill = "Segmento de Mercado")

