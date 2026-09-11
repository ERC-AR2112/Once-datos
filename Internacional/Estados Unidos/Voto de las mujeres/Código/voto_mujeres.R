library(pacman)
p_load(survey, tidyverse, srvyr)
anes24 <- read_csv("Voto_mujeres/anes_timeseries_2024_csv_20260519.csv")

anes_clean <- anes24 %>%
  mutate(sexo = case_when(
      V241550 == 1 ~ "Hombres",
      V241550 == 2 ~ "Mujeres",
      TRUE ~ NA_character_ 
    ),
    voto_presidencial = case_when(
      V242091x %in% c(10,  30) ~ "Demócrata (Harris)",
      V242091x %in% c(11,  31) ~ "Republicano (Trump)",
      V242091x %in% c(12, 32) ~ "Otro",
      TRUE ~ NA_character_ 
  ) )%>%
  filter(!is.na(sexo), !is.na(voto_presidencial), !is.na(V240108b))

# 2. Declarar el diseño de la encuesta con el ponderador combinado pre-electoral (V240108b)
anes_design <- anes_clean %>%
  as_survey_design(weights = V240108b)

# 3. Calcular las proporciones ponderadas e intervalos de confianza por sexo
plot_data <- anes_design %>%
  group_by(sexo, voto_presidencial) %>%
  summarize(proporcion = survey_prop(vartype = "ci")) %>%
  ungroup()

# 4. Crear la visualización con ggplot2
ggplot(plot_data, aes(x = sexo, y = proporcion, fill = voto_presidencial)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  # Añadir barras de error basadas en el intervalo de confianza (opcional pero recomendado)
  geom_errorbar(aes(ymin = proporcion_low, ymax = proporcion_upp), 
                position = position_dodge(width = 0.8), width = 0.2, alpha = 0.7) +
  # Añadir etiquetas de porcentaje
  geom_text(aes(label = scales::percent(proporcion, accuracy = 0.1)), 
            position = position_dodge(width = 0.8), vjust = -1.5, size = 3.5) +
  scale_fill_manual(values = c("Demócrata (Harris)" = "#2E74C0", 
                               "Republicano (Trump)" = "#CB454A", 
                               "Otro" = "#A0A0A0")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Preferencia Presidencial por Sexo en EE. UU. (ANES 2024)",
    subtitle = "Brecha de género en intención y preferencia de voto pre-electoral",
    x = NULL,
    y = "Proporción de votantes",
    fill = "Candidato / Partido",
    caption = "Fuente: ANES 2024 Time Series Study (Datos Post-electorales)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

anes_clean <- anes24 %>%
  mutate(
    # Variable de sexo pre-electoral
    sexo = case_when(
      V241550 == 1 ~ "Hombres",
      V241550 == 2 ~ "Mujeres",
      TRUE ~ NA_character_
    ),
    
    # Asegurar que la variable de ideología sea numérica para filtrarla fácilmente
    ideologia = as.numeric(V242438)
  ) %>%
  # 2. Filtrado estricto
  filter(
    !is.na(sexo),
    !is.na(V240108b), V240108b > 0, # Conservar solo ponderadores post-electorales válidos
    !is.na(ideologia),
    ideologia >= 0 & ideologia <= 10 # Excluye valores negativos y los códigos 95, 96, 97, 98
  )

# 3. Declarar el diseño muestral
anes_design <- anes_clean %>%
  as_survey_design(weights = V240108b)

# 4. Calcular la proporción de encuestados en cada punto de la escala por sexo
plot_data <- anes_design %>%
  group_by(sexo, ideologia) %>%
  summarize(proporcion = survey_prop()) %>%
  ungroup() %>%
  # Convertir la ideología a factor para que el eje X de la gráfica sea discreto y ordenado
  mutate(ideologia = factor(ideologia, levels = 0:10))

# 5. Generar la visualización
ggplot(plot_data, aes(x = ideologia, y = proporcion, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("Hombres" = "#2c3e50", "Mujeres" = "#e74c3c")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribución Ideológica (Izquierda-Derecha) por Sexo en EE. UU.",
    x = "Autoubicación Ideológica de 0 (Izquierda) a 10 (Derecha)",
    y = "Proporción del grupo",
    fill = "Sexo",
    caption = "Fuente: ANES 2024 Time Series Study (Datos Post-electorales)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

promedios_ideologicos <- anes_design %>%
  group_by(sexo) %>%
  summarize(promedio = survey_mean(ideologia, vartype = "ci"))


####Opinión sobre el aborto

anes_clean <- anes24 %>%
  mutate(
    sexo = case_when(
      V241550 == 1 ~ "Hombres",
      V241550 == 2 ~ "Mujeres",
      TRUE ~ NA_character_
    ),
    aborto_escala = as.numeric(V241730)
  ) %>%
  filter(
    !is.na(sexo),
    !is.na(V240108b), V240108b > 0,      
    !is.na(aborto_escala),
    aborto_escala >= 1 & aborto_escala <= 7  
  )


anes_design <- anes_clean %>%
  as_survey_design(weights = V240108b)


plot_data <- anes_design %>%
  group_by(sexo, aborto_escala) %>%
  summarize(proporcion = survey_prop()) %>%
  ungroup() %>%
  mutate(aborto_escala = factor(aborto_escala, levels = 1:7))

ggplot(plot_data, aes(x = aborto_escala, y = proporcion, fill = sexo)) +
  geom_col(position = position_dodge(width = 0.8), color = "black", alpha = 0.85) +
  # Etiquetas de porcentaje sobre las barras
  geom_text(aes(label = scales::percent(proporcion, accuracy = 1)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Hombres" = "#2c3e50", "Mujeres" = "#e74c3c")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(plot_data$proporcion) + 0.1)) +
  labs(
    title = "Postura sobre el Aborto por Sexo en EE. UU. (ANES 2024)",
    x = "Escala: 1 (Siempre legal por cualquier razón) a 7 (Siempre ilegal sin excepciones)",
    y = "Proporción del grupo",
    fill = "Sexo",
    caption = "Fuente: ANES 2024 Time Series Study"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

