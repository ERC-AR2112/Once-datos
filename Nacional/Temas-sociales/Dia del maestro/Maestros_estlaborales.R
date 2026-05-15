library(readr)
library(tidyverse)
ENOE_COE1T425 <- read_csv("Dia_Maestro/ENOE_COE1T425.csv")
ENOE_SDEMT425 <- read_csv("Dia_Maestro/ENOE_SDEMT425.csv")
enoe_docentes<- ENOE_SDEMT425 %>% 
  inner_join(ENOE_COE1T425, by = c("cd_a", "cve_ent", "con", "v_sel", "n_hog", "h_mud", "n_ren")) %>% 
  filter(
    r_def.x == 0,              
    c_res %in% c("1", "3"),     
    p3 %in% c("2311", "2312", "2321", "2322", "2331", "2332", "2333", "2334", "2334", "2335", "2339", "2341", "2342", "2343", "2399") 
  ) %>%
  mutate(
    sex = factor(sex, levels = c("1", "2"), labels = c("Hombre", "Mujer")), 
    eda = as.numeric(eda.x), 
    hrsocup = as.numeric(hrsocup), 
    rango_edad = case_when(
      eda <= 29 ~ "20-29 años",
      eda <= 39 ~ "30-39 años",
      eda <= 49 ~ "40-49 años",
      TRUE ~ "50 años o más"
    ),
    grupo_horas = case_when(
      hrsocup < 15 ~ "Menos de 15 hrs",
      hrsocup >= 15 & hrsocup <= 24 ~ "15 a 24 hrs",
      hrsocup >= 25 & hrsocup <= 34 ~ "25 a 34 hrs",
      hrsocup >= 35 & hrsocup <= 45 ~ "35 a 45 hrs",
      hrsocup > 45 ~ "Más de 45 hrs"
    ),
    # Aseguramos un orden lógico para las tablas
    grupo_horas = factor(grupo_horas, levels = c(
      "Menos de 15 hrs", 
      "15 a 24 hrs", 
      "25 a 34 hrs", 
      "35 a 45 hrs", 
      "Más de 45 hrs"
    ))
  )


enoe_diseno <- enoe_docentes %>%
  as_survey_design(weights = fac_tri.x)

resumen_sexo <- enoe_diseno %>%
  group_by(sex) %>%
  summarise(
    total = survey_total(na.rm = TRUE),
    porcentaje = survey_mean(na.rm = TRUE) * 100
  )

# B. Distribución por edad
resumen_edad <- enoe_diseno %>%
  group_by(rango_edad) %>%
  summarise(total = survey_total(na.rm = TRUE))

# C. Promedio de horas trabajadas a la semana
tabla_horas <- enoe_diseno %>%
  group_by(grupo_horas) %>%
  summarise(
    maestros_total = survey_total(), # Cantidad total estimada de maestros
    porcentaje = survey_mean() * 100 # Porcentaje sobre el total
  )


##Gráficas
grafica_sexo <- ggplot(resumen_sexo, aes(x = sex, y = total, fill = sex)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(total)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Fuerza Laboral Docente por Sexo en México",
    subtitle = "Primaria, Secundaria y Bachillerato (4T 2025)",
    x = "Sexo",
    y = "Total de Personas (Estimado)",
    caption = "Fuente: Estimaciones propias con microdatos de la ENOE, INEGI."
  ) +
  theme_minimal()


gráfica_edad <- ggplot(resumen_edad, aes(x = rango_edad, y = total, fill = rango_edad)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + # Para mejor lectura de los rangos
  labs(
    title = "Distribución de Maestros por Rangos de Edad",
    x = "Grupo de Edad",
    y = "Total de Docentes",
    caption = "Datos expandidos mediante fac_tri."
  ) +
  theme_light()


ggplot(tabla_horas, aes(x = grupo_horas, y = porcentaje, fill = grupo_horas)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), vjust = -0.5) +
  labs(
    title = "Distribución de la Carga Horaria Semanal",
    subtitle = "Porcentaje de docentes según horas dedicadas a la ocupación principal",
    x = "Horas Trabajadas",
    y = "Porcentaje de la Población Docente",
    caption = "Nota: No incluye horas de preparación en casa (fuente: ENOE)."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Opción treemap
install.packages("treemapify")
library(treemapify)


visualizacion_treemap <- tabla_horas %>%
  mutate(etiqueta = paste0(grupo_horas, "\n", 
                           round(porcentaje, 1), "%"))

grafica_horas <- ggplot(visualizacion_treemap, aes(area = maestros_total, fill = grupo_horas, label = etiqueta)) +
  geom_treemap(color = "black", size = 1) +
  geom_treemap_text(
    colour = "black", 
    place = "centre", 
    reflow = TRUE
  ) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) + # Escala de verde a rojo
  labs(
    title = "Distribución de la Carga Horaria Docente en México",
    subtitle = "Basado en el Censo de Ocupación (4T 2025). Códigos SINCO: 231, 232, 233, 234.",
    caption = "Fuente: Estimaciones con microdatos de la ENOE, INEGI. El tamaño del cuadro representa el volumen de docentes.",
    fill = "Rango de Horas"
  ) +
  theme(legend.position = "none")
