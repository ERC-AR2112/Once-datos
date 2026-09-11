### Perros en México

library(pacman)

p_load(tidyverse, readr, survey, srvyr, gt)

base_viv <- read_csv("Dia_perro/TVIVIENDA.csv")

base_viv |> 
  count(P1_8_1_1)

base_viv |> 
  count(P1_8_1_2)
base_viv |> count(P1_8_2_2)

base_viv <- base_viv |> 
  mutate(perros = if_else(P1_8_1_1 == 1, 1, 0),
         n_perros = as.numeric(P1_8_1_2),
         gatos = if_else(P1_8_2_1== 1, 1, 0),
         n_gatos = as.numeric(P1_8_2_2),
         estado = factor(CVE_ENT, 
                         levels = c("01","02","03","04","05","06","07","08","09","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30",
                                    "31","32"),
                         labels = c("Aguascalientes", "Baja California", "Baja California Sur", 
                                    "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", 
                                    "Ciudad de México", "Durango", "Guanajuato", "Guerrero", 
                                    "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", 
                                    "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", 
                                    "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
                                    "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
                                    "Yucatán", "Zacatecas")),
         mascota = if_else(P1_8_1_1==1 | P1_8_2_1==1 | P1_8_3_1==1, 1, 0 ))

base_dis <- base_viv |> 
  as_survey_design(strata = EST_DIS,
                   weights = FAC_VIV)

base_dis |> 
  summarise("Porecentaje de hogares con perros" = survey_mean(perros)*100,
            "Porecentaje de hogares con gatos" = survey_mean(gatos)*100)


base_dis |> 
  survey_count(perros)



base_dis |> 
  filter(perros ==1) |> 
  summarise("Número de perros por vivienda" = survey_mean(n_perros))


base_dis |> 
  filter(gatos ==1) |> 
  summarise("Número de gatos por vivienda" = survey_mean(n_gatos, na.rm = T))


tabla_estados <- base_dis |>  
  group_by(estado) |>  
  summarise(
    "Porcentaje de hogares con perros" = survey_mean(perros, na.rm = TRUE) * 100,
    "Porcentaje de hogares con gatos" = survey_mean(gatos, na.rm = TRUE) * 100
  ) |>  
  gt() |>  
  fmt_number(
    columns = c("Porcentaje de hogares con perros", "Porcentaje de hogares con gatos"),
    decimals = 2
  ) |>  
  cols_label(
    estado = "Entidad Federativa"
  )

tabla_estados

datos_grafica <- base_dis |>  
  group_by(estado) |>  
  summarise(
    pct_perros = survey_mean(perros, na.rm = TRUE) * 100,
    pct_gatos = survey_mean(gatos, na.rm = TRUE) * 100
  ) |>  
  mutate(estado = reorder(estado, pct_perros)) # Ordena los estados de menor a mayor según el % de perros

# Gráfica de barras horizontales para el porcentaje de hogares con perros
ggplot(datos_grafica, aes(x = estado, y = pct_perros)) +
  geom_col(fill = "#2b5c8f", width = 0.7) +
  coord_flip() + # Convierte las barras en horizontales
  labs(
    title = "Porcentaje de viviendas con perros por entidad federativa",
    subtitle = "Fuente: ENBIARE",
    x = "Entidad Federativa",
    y = "Porcentaje de viviendas (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )


datos_perros <- base_dis |>  
  group_by(estado) |>  
  summarise(
    pct_perros = survey_mean(perros, na.rm = TRUE) * 100
  ) |>  
  mutate(estado = reorder(estado, pct_perros))

ggplot(datos_perros, aes(x = estado, y = pct_perros)) +
  geom_col(fill = "#fb2447", width = 0.75, alpha = 0.9) + 
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%%", pct_perros)), 
            hjust = -0.15, size = 3, fontface = "bold", color = "#4a4a4a") +
  scale_y_continuous(limits = c(0, max(datos_perros$pct_perros) + 10)) + # Espacio para las etiquetas
  labs(
    title = "🐶 ¡Hogares mexicanos que aman a los lomitos! 🐾",
    subtitle = "Porcentaje de viviendas con perros por entidad federativa (ENBIARE)",
    caption = "¡Feliz Día del Perro!",
    x = "",
    y = "Porcentaje de vivienda con presencia de perros (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
    plot.caption = element_text(face = "italic", size = 10, color = "#fb2447", hjust = 1),
    panel.grid.major.y = element_blank()
    axis.text.y = element_text(face = "bold", color = "#34495e"),
    axis.text.x = element_blank(),       
    axis.ticks.x = element_blank()
  )
