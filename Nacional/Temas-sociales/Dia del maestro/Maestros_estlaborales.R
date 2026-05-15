library(readr)
library(tidyverse)
library(srvyr)
library(flextable)
ENOE_COE1T425 <- read_csv("Dia_Maestro/datos/ENOE_COE1T425.csv")
ENOE_SDEMT425 <- read_csv("Dia_Maestro/datos/ENOE_SDEMT425.csv")
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
    categoria_docente = case_when(
      # Subgrupo 231: Supervisores y Especialistas
      p3 == 2311 ~ "Supervisores e inspectores educativos",
      p3 == 2312 ~ "Pedagogos y orientadores educativos",
      
      # Subgrupo 232: Medio y Superior
      p3 == 2321 ~ "Profesores universitarios y de enseñanza superior",
      p3 == 2322 ~ "Profesores de preparatoria y equivalentes",
      
      # Subgrupo 233: Nivel Básico
      p3 == 2331 ~ "Profesores de enseñanza secundaria",
      p3 == 2332 ~ "Profesores de enseñanza primaria",
      p3 == 2333 ~ "Alfabetizadores",
      p3 == 2334 ~ "Profesores de enseñanza bilingüe (indígena)",
      p3 == 2335 ~ "Profesores de enseñanza preescolar",
      p3 == 2339 ~ "Otros profesores de nivel básico",
      
      # Subgrupo 234: Enseñanza Especial
      p3 %in% c(2341, 2342, 2343) ~ "Profesores de enseñanza especial",
      
      # Subgrupo 239: Otros
      p3 == 2399 ~ "Otros especialistas en docencia",
      TRUE ~ "No clasificados"
    ),
    grupo_horas = case_when(
      hrsocup < 15 ~ "Menos de 15 hrs",
      hrsocup >= 15 & hrsocup <= 24 ~ "15 a 24 hrs",
      hrsocup >= 25 & hrsocup <= 34 ~ "25 a 34 hrs",
      hrsocup >= 35 & hrsocup <= 45 ~ "35 a 45 hrs",
      hrsocup > 45 ~ "Más de 45 hrs"
    ),
    grupo_horas = factor(grupo_horas, levels = c(
      "Menos de 15 hrs", 
      "15 a 24 hrs", 
      "25 a 34 hrs", 
      "35 a 45 hrs", 
      "Más de 45 hrs"
    )),
    grado_academico = case_when(
      cs_p13_1 == 5 ~ "Normal",
      cs_p13_1 == 7 ~ "Licenciatura",
      cs_p13_1 == 8 ~ "Maestría",
      cs_p13_1 == 9 ~ "Doctorado",
      cs_p13_1 %in% c(1, 2, 3, 4, 6) ~ "Otros (Básica/Media/Técnica)",
      TRUE ~ "No especificado"
    ),
    grado_academico = factor(grado_academico, levels = c(
      "Doctorado", "Maestría", "Licenciatura", "Normal", "Otros (Básica/Media/Técnica)", "No especificado"
    )),
    nivel_educativo = case_when(
      p3 == 2335 ~ "Preescolar",
      p3 %in% c(2332, 2334) ~ "Primaria", # Incluye primaria general e indígena
      p3 == 2331 ~ "Secundaria",
      p3 == 2322 ~ "Media Superior",
      p3 == 2321 ~ "Superior",
      TRUE ~ "Otros/No especificado"
    ),
    nivel_educativo = factor(nivel_educativo, 
                             levels = c("Preescolar", "Primaria", "Secundaria", 
                                        "Media Superior", "Superior", "Otros/No especificado"))
  )


enoe_diseno <- enoe_docentes %>%
  as_survey_design(weights = fac_tri.x)

resumen_sexo <- enoe_diseno %>%
  group_by(sex) %>%
  summarise(
    total = survey_total(na.rm = TRUE),
    porcentaje = survey_mean(na.rm = TRUE) * 100
  )

resumen_sexo_nivel <- enoe_diseno %>%
  group_by(nivel_educativo, sex) %>%
  summarise(
    pct_total = survey_mean(na.rm = TRUE) * 100
  ) %>%
  mutate(label_pct = paste0(nivel_educativo, " (", sex, ")\n", round(pct_total, 1), "%"))

tabla_niveles_detalle <- enoe_diseno %>%
  group_by(categoria_docente) %>%
  summarise(
    total_estimado = survey_total(na.rm = TRUE),
    porcentaje = survey_mean(na.rm = TRUE) * 100
  ) %>%
  arrange(desc(total_estimado))

flextable(tabla_niveles_detalle)

resumen_edad <- enoe_diseno %>%
  group_by(rango_edad) %>%
  summarise(total = survey_total(na.rm = TRUE),
            porcentaje = survey_mean(na.rm = TRUE) * 100)

# C. Promedio de horas trabajadas a la semana
tabla_horas <- enoe_diseno %>%
  group_by(grupo_horas) %>%
  summarise(
    maestros_total = survey_total(), # Cantidad total estimada de maestros
    porcentaje = survey_mean() * 100 # Porcentaje sobre el total
  )


tabla_educacion <- enoe_diseno %>%
  group_by(grado_academico) %>%
  summarise(
    total_docentes = survey_total(na.rm = TRUE),
    porcentaje = survey_mean(na.rm = TRUE) * 100
  )


##Gráficas
grafica_sexo <- ggplot(resumen_sexo, aes(x = sex, y = porcentaje, fill = sex)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(porcentaje, 2)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Fuerza Laboral Docente por Sexo en México",
    subtitle = "Primaria, Secundaria y Bachillerato (4T 2025)",
    x = "Sexo",
    y = "Porcentaje (Estimado)",
    caption = "Fuente: Estimaciones propias con microdatos de la ENOE, INEGI."
  ) +
  theme_minimal()




gráfica_edad <- ggplot(resumen_edad, aes(x = rango_edad, y = porcentaje, fill = rango_edad)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + # Para mejor lectura de los rangos
  labs(
    title = "Distribución de Maestros por Rangos de Edad",
    x = "Grupo de Edad",
    y = "Porcentaje (Estimado)",
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

datos_grafico_edu <- tabla_educacion %>%
  mutate(label_edu = paste0(grado_academico, "\n", 
                            round(porcentaje, 1), "%"))

# Generar Treemap
grafica_educ <- ggplot(datos_grafico_edu, aes(area = total_docentes, fill = grado_academico, label = label_edu)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(colour = "black", place = "centre", reflow = TRUE) +
  scale_fill_viridis_d(option = "mako", direction = -1) +
  labs(
    title = "Perfil Académico de los Docentes en México",
    subtitle = "Basado en la ENOE 4T 2025 (Bachillerato, Secundaria y Primaria)",
    caption = "Fuente: Estimaciones propias con microdatos del INEGI usando el factor de expansión fac_tri.",
    fill = "Nivel de Estudios"
  ) +
  theme(legend.position = "none")


ggplot(resumen_sexo_nivel, aes(area = pct_total, fill = nivel_educativo, subgroup = sex, label = label_pct)) +
  geom_treemap(color = "white", size = 1.5) +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  scale_fill_viridis_d(option = "plasma", name = "Nivel Educativo") +
  labs(
    title = "Distribución Porcentual del Magisterio en México",
    subtitle = "Proporción del total de docentes por Nivel y Sexo (4T 2025)",
    caption = "Fuente: Estimaciones propias con microdatos de la ENOE (INEGI). Ponderado con fac_tri."
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



