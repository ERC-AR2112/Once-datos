library(pacman)
p_load(tidyverse, rnaturalearth,rnaturalearthdata, sf, readxl)

df <- read_excel("Mundial/Selecciones/Francia_jugadores.xlsx")


nacimientos <- df %>%
  select(`Lugar de Nacimiento`) %>%
  drop_na() %>%
  # Corregir el error tipográfico en la base original
  mutate(`Lugar de Nacimiento` = str_replace(`Lugar de Nacimiento`, "Ingraterra", "Inglaterra")) %>%
  count(`Lugar de Nacimiento`, name = "cantidad")

# 2. Homologar los nombres de los países con la cartografía base (en inglés)
nacimientos <- nacimientos %>%
  mutate(admin = case_when(
    `Lugar de Nacimiento` == "Francia" ~ "France",
    `Lugar de Nacimiento` == "Guayana Francesa" ~ "France", # Se agrupa con Francia a nivel de país
    `Lugar de Nacimiento` == "Inglaterra" ~ "United Kingdom",
    `Lugar de Nacimiento` == "Italia" ~ "Italy",
    `Lugar de Nacimiento` == "República del Congo" ~ "Republic of Congo",
    TRUE ~ `Lugar de Nacimiento`
  )) %>%
  group_by(admin) %>%
  summarise(cantidad = sum(cantidad))

# 3. Cargar el mapa mundial y unir los datos
mundo <- ne_countries(scale = "medium", returnclass = "sf")

mapa_datos <- mundo %>%
  left_join(nacimientos, by = "admin") %>%
  mutate(cantidad = replace_na(cantidad, 0))

# 4. Graficar el mapa
ggplot(data = mapa_datos) +
  geom_sf(aes(fill = cantidad), color = "white", size = 0.2) +
  # Se filtran los ceros para que el mapa resalte solo los países de origen
  scale_fill_viridis_c(
    option = "plasma", 
    name = "Número de\njugadores", 
    na.value = "gray90",
    limits = c(1, max(nacimientos$cantidad, na.rm = TRUE))
  ) +
  theme_bw() +
  labs(
    title = "Países de origen de las y los jugadores",
    subtitle = "Selección de Francia",
    caption = "Fuente: Elaboración propia"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )


nacimientos_madres <- df %>%
  select(`Nacimiento de la Madre`) %>%
  drop_na() %>%
  # Separar los casos donde hay doble nacionalidad o múltiples países (ej. "Mauritania / Senegal")
  separate_rows(`Nacimiento de la Madre`, sep = "\\s*/\\s*") %>%
  # Limpiar espacios en blanco
  mutate(`Nacimiento de la Madre` = str_trim(`Nacimiento de la Madre`)) %>%
  count(`Nacimiento de la Madre`, name = "cantidad")

# 2. Homologar los nombres de los países con la cartografía base (en inglés)
nacimientos_madres <- nacimientos_madres %>%
  mutate(admin = case_when(
    `Nacimiento de la Madre` == "Francia" ~ "France",
    `Nacimiento de la Madre` == "Guadalupe" ~ "France", # Se agrupa con Francia en el mapa general
    `Nacimiento de la Madre` == "Camerún" ~ "Cameroon",
    `Nacimiento de la Madre` == "Argelia" ~ "Algeria",
    `Nacimiento de la Madre` == "República del Congo" ~ "Republic of Congo",
    `Nacimiento de la Madre` == "Costa de Marfil" ~ "Ivory Coast",
    `Nacimiento de la Madre` == "Malí" ~ "Mali",
    `Nacimiento de la Madre` == "Haití" ~ "Haiti",
    TRUE ~ `Nacimiento de la Madre`
  )) %>%
  group_by(admin) %>%
  summarise(cantidad = sum(cantidad))

# 3. Cargar el mapa mundial y unir los datos
mundo <- ne_countries(scale = "medium", returnclass = "sf")

mapa_datos <- mundo %>%
  left_join(nacimientos_madres, by = "admin") %>%
  mutate(cantidad = replace_na(cantidad, 0))

# 4. Graficar el mapa
ggplot(data = mapa_datos) +
  geom_sf(aes(fill = cantidad), color = "white", size = 0.2) +
  # Filtrar los ceros para resaltar solo los países de origen materno
  scale_fill_viridis_c(
    option = "plasma", 
    name = "Número de\nmadres", 
    na.value = "gray90",
    limits = c(1, max(nacimientos_madres$cantidad, na.rm = TRUE))
  ) +
  theme_bw() +
  labs(
    title = "Países de origen de las madres de los jugadores",
    subtitle = "Selección de Francia",
    caption = "Fuente: Elaboración propia"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )
