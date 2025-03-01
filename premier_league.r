library(dplyr)
library(ggplot2)  
library(readr)  

datos <- read_delim("C:/Users/Adriano/Downloads/PremierLeague.csv", delim = ";")
head(datos)
tabla_contingencia <- table(datos$Season, datos$FullTimeResult)
tabla_contingencia_df <- as.data.frame(prop.table(tabla_contingencia, 1) * 100)
print(tabla_contingencia)

ggplot(datos, aes(x = FullTimeResult, fill = FullTimeResult)) +
  geom_bar() +
  labs(title = "Distribución de resultados en la Premier League desde la temporada 2019",
       x = "Resultado del Partido", 
       y = "Cantidad de Partidos") +
  theme_minimal()

partidos_jugados <- datos %>%
  group_by(HomeTeam) %>%
  summarise(PartidosLocal = n()) %>%
  full_join(
    datos %>%
      group_by(AwayTeam) %>%
      summarise(PartidosVisita = n()),
    by = c("HomeTeam" = "AwayTeam")
  ) %>%
  mutate(PartidosTotales = Partidos_Local + Partidos_Visita)

victorias_total <- datos %>%
  mutate(Ganador = case_when(
    FullTimeResult == "Home win" ~ HomeTeam,
    FullTimeResult == "Away win" ~ AwayTeam,
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Ganador)) %>%
  group_by(Ganador) %>%
  summarise(Victorias = n())

tabla_final <- partidos_jugados %>%
  left_join(victorias_total, by = c("HomeTeam" = "Ganador")) %>%
  mutate(PorcentajeVictorias = (Victorias / PartidosTotales) * 100)

ggplot(tabla_final, aes(x = reorder(HomeTeam, PorcentajeVictorias), y = PorcentajeVictorias, fill = HomeTeam)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Porcentaje de victorias por equipo",
       x = "Equipo",
       y = "Porcentaje de Victorias") +
  theme_minimal() +
  geom_text(aes(label = paste0(round(PorcentajeVictorias, 1), "%")), hjust = -0.2, size = 4, color = "black")

victorias_por_temporada <- datos %>%
  mutate(Ganador = case_when(
    FullTimeResult == "Home win" ~ HomeTeam,
    FullTimeResult == "Away win" ~ AwayTeam,
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Ganador)) %>%
  group_by(Season, Ganador) %>%
  summarise(Victorias = n())

ggplot(victorias_por_temporada, aes(x = Season, y = Victorias, group = Ganador, color = Ganador)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Tendencia de victorias por equipo",
       x = "Temporada",
       y = "Número de Victorias") +
  theme_minimal()



