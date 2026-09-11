library(pacman)
p_load(readr, tidyverse, flextable, readxl, scales, ggtext)


unam_res <- read_csv("Admision_UNAM/resultados_todos.csv")


unam_res |> 
  filter(year ==2026) |> 
  count(carrera) |> 
  print(n=Inf) |> 
  flextable()


unam_res |> 
  filter(year ==2026, !is.na(aciertos)) |> 
  count(carrera) |> 
  print(n=Inf) |> 
  flextable()


unam_res |> 
  filter(year ==2021) |> 
  count(carrera) |> 
  print(n=Inf)

unam_res |> 
  count(year) |> 
  print(n=Inf)

unam_res |> 
  filter(year ==2026) |> 
  count(carrera) |> 
  arrange(-n)

unam_res |> 
  group_by(carrera, year) |> 
  summarise(promedio = mean(aciertos, na.rm=T),
            demanda = n()) |>
  filter(demanda >= 5328) |> 
  ggplot(aes(x= year, y = promedio, color = carrera)) +
  geom_point()+
  geom_line()+ 
  theme_bw()+
  labs(x= "Año",
       y= "Promedio de aciertos",
       title = "Evolución en el número de aciertos- Examnen de admisión UNAM",
       caption = "Fuente: Elaboración propia con datos de Manuel Toral")

##Distribución general 



unam_res |> 
  filter(!is.na(aciertos)) |> 
  mutate(Año = as.factor(year)) |> 
  ggplot(aes(x = aciertos, group = Año, fill = Año, colour = Año)) + 
  geom_density(aes(alpha = (Año == "2026")), size = 1) +
  scale_alpha_manual(values = c("TRUE" = 0.3, "FALSE" = 0), guide = "none") +
  theme_bw() +
  labs(title = "Distribución de los aciertos en el examen de admisión UNAM", 
       x = "Número de aciertos",
       y = "Densidad",
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1)) +
  theme(legend.position = "top")


datos_barras <- unam_res |> 
  filter(!is.na(aciertos)) |> 
  mutate("100 o más" = if_else(aciertos >= 100, 1, 0), 
         "110 o más" = if_else(aciertos >= 110, 1, 0),
         Año = as.factor(year)) |> 
  group_by(Año) |> 
  summarise("100 o más" = mean(`100 o más`) * 100,
            "110 o más" = mean(`110 o más`) * 100) |>
  pivot_longer(cols = c("100 o más", "110 o más"), 
               names_to = "Categoria", 
               values_to = "Porcentaje")


ggplot(datos_barras, aes(x = Año, y = Porcentaje, fill = Categoria)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(title = "Porcentaje de aspirantes con más de 100 y 110 aciertos",
       subtitle = "Comparativa por año",
       y = "Porcentaje (%)",
       x = "Año",
       fill= "Aciertos",
       caption = "Fuente: Elaboración propia con datos de Manuel Toral" ) +
  theme(legend.position = "top")




unam_res |> 
  filter(!is.na(aciertos), year==2026) |> 
  mutate(Año = as.factor(year)) |> 
  ggplot(aes(x = aciertos)) + 
  geom_histogram(fill="skyblue", color= "Black", bins = 40) +
  theme_bw() +
  labs(title = "Distribución de los aciertos en el examen de admisión UNAM 2026", 
       x = "Número de aciertos",
       y = "Densidad",
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1)) +
  theme(legend.position = "top")



unam_res |> 
  filter(!is.na(aciertos), year==2026) |> 
  mutate(Año = as.factor(year)) |> 
  ggplot(aes(x = aciertos)) + 
  geom_bar(fill="skyblue", color= "Black") +
  theme_bw() +
  labs(title = "Distribución de los aciertos en el examen de admisión UNAM 2026", 
       x = "Número de aciertos",
       y = "Densidad",
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1)) +
  theme(legend.position = "top")



unam_res |> 
  filter(!is.na(aciertos), year==2025) |> 
  mutate(Año = as.factor(year)) |> 
  ggplot(aes(x = aciertos)) + 
  geom_bar(fill="skyblue", color= "Black") +
  theme_bw() +
  labs(title = "Distribución de los aciertos en el examen de admisión UNAM 2025", 
       x = "Número de aciertos",
       y = "Densidad",
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1)) +
  theme(legend.position = "top")

###Gráfica de cambio

glimpse(unam_res)

top_carreras <- unam_res |> 
  filter(year %in% c(2025, 2026), !is.na(aciertos)) |> 
  group_by(carrera, year) |> 
  summarise(promedio_aciertos = mean(aciertos), .groups = "drop") |> 
  pivot_wider(names_from = year, values_from = promedio_aciertos, names_prefix = "anio_") |> 
  filter(!is.na(anio_2025) & !is.na(anio_2026)) |> 
  mutate(crecimiento = anio_2026 - anio_2025) |> 
  slice_max(order_by = crecimiento, n = 10) |> 
  mutate(carrera = reorder(carrera, crecimiento))


ggplot(top_carreras, aes(x = crecimiento, y = carrera, fill = crecimiento)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  labs(title = "Las 10 carreras con mayor incremento en aciertos",
       subtitle = "Comparativa entre 2025 y 2026",
       x = "Incremento en el promedio de aciertos",
       y = NULL,
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") 



top_carreras_m <- unam_res |> 
  filter(year %in% c(2025, 2026), !is.na(aciertos)) |> 
  group_by(carrera, year) |> 
  summarise(mediana_aciertos = median(aciertos, na.rm = T), .groups = "drop") |> 
  pivot_wider(names_from = year, values_from = mediana_aciertos, names_prefix = "anio_") |> 
  filter(!is.na(anio_2025) & !is.na(anio_2026)) |> 
  mutate(crecimiento = anio_2026 - anio_2025) |> 
  slice_max(order_by = crecimiento, n = 10) |> 
  mutate(carrera = reorder(carrera, crecimiento))


ggplot(top_carreras_m, aes(x = crecimiento, y = carrera, fill = crecimiento)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  labs(title = "Las 10 carreras con mayor incremento en aciertos (mediana)",
       subtitle = "Comparativa entre 2025 y 2026",
       x = "Incremento en la mediana de aciertos",
       y = NULL,
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") 


unam_res |> 
  filter(year %in% c(2025, 2026), !is.na(aciertos)) |> 
  group_by(carrera, year) |> 
  summarise(promedio_aciertos = mean(aciertos), .groups = "drop") |> 
  pivot_wider(names_from = year, values_from = promedio_aciertos, names_prefix = "anio_") |> 
  filter(!is.na(anio_2025) & !is.na(anio_2026)) |> 
  mutate(crecimiento = anio_2026 - anio_2025) |> 
  filter(carrera== "GEOGRAFIA")



top_carreras_m <- unam_res |> 
  filter(year %in% c(2025, 2026), !is.na(aciertos)) |> 
  group_by(carrera, year) |> 
  summarise(mediana_aciertos = median(aciertos, na.rm = T), .groups = "drop") |> 
  pivot_wider(names_from = year, values_from = mediana_aciertos, names_prefix = "anio_") |> 
  filter(!is.na(anio_2025) & !is.na(anio_2026)) |> 
  mutate(crecimiento = anio_2026 - anio_2025) |> 
  slice_min(order_by = crecimiento, n = 10) |> 
  mutate(carrera = reorder(carrera, crecimiento))


ggplot(top_carreras_m, aes(x = crecimiento, y = carrera, fill = crecimiento)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  labs(title = "Las 10 carreras con menor incremento en aciertos (mediana)",
       subtitle = "Comparativa entre 2025 y 2026",
       x = "Incremento en la mediana de aciertos",
       y = NULL,
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") 




top_carreras <- unam_res |> 
  filter(year %in% c(2025, 2026), !is.na(aciertos)) |> 
  group_by(carrera, year) |> 
  summarise(promedio_aciertos = mean(aciertos), .groups = "drop") |> 
  pivot_wider(names_from = year, values_from = promedio_aciertos, names_prefix = "anio_") |> 
  filter(!is.na(anio_2025) & !is.na(anio_2026)) |> 
  mutate(crecimiento = anio_2026 - anio_2025) |> 
  slice_min(order_by = crecimiento, n = 10) |> 
  mutate(carrera = reorder(carrera, crecimiento))


ggplot(top_carreras, aes(x = crecimiento, y = carrera, fill = crecimiento)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  labs(title = "Las 10 carreras con menor incremento en aciertos",
       subtitle = "Comparativa entre 2025 y 2026",
       x = "Incremento en el promedio de aciertos",
       y = NULL,
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") 

unam_res |> 
  filter(!is.na(aciertos)) |> 
  count(year)

admisiones_pct <- read_excel("Admision_UNAM/admisiones_pct.xlsx")

df_plot <- admisiones_pct |> 
  arrange(Tipo, Porcentaje) |> 
  mutate(Universidad = fct_inorder(Universidad))

estilos_eje <- ifelse(levels(df_plot$Universidad) == "UNAM", "bold", "plain")

df_plot |> 
  ggplot(aes(x = Porcentaje, y = Universidad, fill = Tipo)) +
  geom_col() +
  scale_fill_manual(values = c("Privada"="#2980b9", "Pública" = "#fb2447")) +
  theme_bw() +
  # Pasamos el vector de estilos al argumento face
  theme(axis.text.y = element_text(face = estilos_eje)) +
  labs(title = "Tasa de admisión de universidades",
       x = "Porcentaje de admisión")


admisiones_pct |> 
  mutate(Universidad = ifelse(Universidad == "UNAM", "**UNAM**", Universidad)) |> 
  arrange(Tipo, Porcentaje) |> 
  mutate(Universidad = fct_inorder(Universidad)) |> 
  ggplot(aes(x = Porcentaje, y = Universidad, fill = Tipo)) +
  geom_col() +
  geom_text(aes(label = percent(Porcentaje, accuracy = 0.1)), 
            hjust = -0.2, # Empuja el texto ligeramente a la derecha de la barra
            size = 3.5,   # Tamaño de la letra
            color = "black") +
  scale_fill_manual(values = c("Privada"="#2980b9", "Pública" = "#fb2447")) +
  scale_x_continuous(labels = label_percent()) +
  theme_bw() +
  theme(axis.text.y = element_markdown()) +
  labs(title = "Tasa de admisión de universidades",
       x = "Porcentaje de admisión")


df_plot |> 
  ggplot(aes(y= `Costo (en dolares)`, x = Porcentaje, color = Tipo))+
  geom_point( size = 10, alpha = 0.5)+
  geom_text(aes(label = Universidad),
            color="black",
            hjust = -0.4)+
  scale_x_continuous(labels = label_percent())+
  labs(y = "Costo anual en dolares",
       x = "Porcentaje de aceptación",
       title = "Porcentaje de admisión y costos para los estudiantes",
       color = "Gestión:")+
  theme_bw()+
  theme(legend.position = "bottom")


###efectos de medida

unam26 <- unam_res |> 
  filter(year==2026)

metadata_carreras <- read_csv("Admision_UNAM/metadata_carreras.csv")
 metadata_carreras <- metadata_carreras |> 
   filter(year==2025)
 unam26 <- unam26 |>   
   left_join(metadata_carreras, by = c("modalidad", "carrera", "campus"))

unam26 <- unam26 |> 
  mutate(límite = if_else(aciertos >= aciertos_minimos, 1, 0))

unam26 |> summarise(porcentaje = mean(límite, na.rm=T)*100)


unam26 |> 
  mutate(modalidad= case_when(modalidad=="abierta" ~ "Abierta",
                              modalidad== "escolarizado" ~ "Escolarizada",
                              modalidad== "suayed" ~ "SUA y educación a distancia")) |> 
  group_by(modalidad) |> 
  summarise(porcentaje = mean(límite, na.rm=T)*100) |> 
  ggplot(aes(x = porcentaje, y= reorder(modalidad, -porcentaje), fill = porcentaje))+
  geom_col(show.legend = FALSE)+
  theme_bw() +
  labs(title = "Porcentaje de estudiantes que aplicarán control por modalidad",
       x = "Porcentaje",
       y = NULL,
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") 


unam26 |>
  filter(modalidad=="escolarizado") |> 
  group_by(carrera) |> 
  summarise( demanda = n(),
    porcentaje = mean(límite, na.rm=T)*100) |> 
  arrange(-porcentaje) |> 
  filter(demanda >= 4793) |> 
  ggplot(aes(x = porcentaje, y= reorder(carrera, -porcentaje), fill = porcentaje))+
  geom_col(show.legend = FALSE)+
  theme_bw() +
  labs(title = "Porcentaje de estudiantes que aplicarán control - 10 carreras más demandadas",
       x = "Porcentaje",
       y = NULL,
       caption = "Fuente: Elaboración propia con datos de Manuel Toral") 

