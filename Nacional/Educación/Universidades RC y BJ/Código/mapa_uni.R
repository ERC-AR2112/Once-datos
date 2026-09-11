library(tidyverse)
install.packages("sf")
library(sf)



denue_raw <- read_csv(
  "Universidades_zmcdmx/INEGI_DENUE_02092026.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)


unis_interes <- denue_raw %>%
  filter(
    !is.na(Latitud) & !is.na(Longitud),
    str_detect(`Nombre de la Unidad Económica`, regex("ROSARIO CASTELLANOS|BENITO JUAREZ|UBBJ", ignore_case = TRUE)) |
      str_detect(`Razón social`, regex("ROSARIO CASTELLANOS|BENITO JUAREZ|BIENESTAR", ignore_case = TRUE))
  ) %>%
  mutate(
    subsistema = case_when(
      str_detect(`Nombre de la Unidad Económica`, regex("ROSARIO CASTELLANOS", ignore_case = TRUE)) |
        str_detect(`Razón social`, regex("ROSARIO CASTELLANOS", ignore_case = TRUE)) ~ "Universidad Rosario Castellanos",
      
      str_detect(`Nombre de la Unidad Económica`, regex("BENITO JUAREZ|BIENESTAR|UBBJ", ignore_case = TRUE)) |
        str_detect(`Razón social`, regex("BENITO JUAREZ|BIENESTAR|UBBJ", ignore_case = TRUE)) ~ "Univ. para el Bienestar Benito Juárez",
      
      TRUE ~ "Otro"
    )
  )


unis_sf <- st_as_sf(
  unis_interes,
  coords = c("Longitud", "Latitud"),
  crs = 4326
)

unis_sf <- unis_sf %>%
  mutate(
    entidad_limpia = case_when(
      str_detect(`Entidad federativa`, regex("CIUDAD DE M", ignore_case = TRUE)) ~ "Ciudad de México",
      str_detect(`Entidad federativa`, regex("MÉXICO|MEXICO", ignore_case = TRUE)) ~ "Estado de México",
      str_detect(`Entidad federativa`, regex("HIDALGO", ignore_case = TRUE)) ~ "Hidalgo",
      TRUE ~ `Entidad federativa`
    )
  )


limites_zmvm <- st_read("00mun.shp") %>%
  st_transform(crs = 4326)


mapa_universidades <- ggplot() +
  # Capa base poligonal
  geom_sf(
    data = limites_zmvm,
    aes(fill = CVE_ENT), 
    color = "white",
    linewidth = 0.25,
    alpha = 0.65,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    name = "Entidad Federativa:",
    values = c(
      "09" = "#4361ee",
      "15"           = "#f72585",
      "13"          = "#4cc9f0"
    )
  ) +
  geom_sf(
    data = unis_sf,
    aes(color = subsistema),
    size = 3.2,
    alpha = 0.95
  ) +
  scale_color_manual(
    name = "Subsistema:",
    values = c(
      "Universidad Rosario Castellanos"      = "#2d6a4f", # Verde institucional
      "Univ. para el Bienestar Benito Juárez" = "#b91c1c"  # Rojo institucional
    )
  ) +
  # Encuadre automático centrado en los puntos
  coord_sf(
    xlim = c(st_bbox(unis_sf)["xmin"] - 0.08, st_bbox(unis_sf)["xmax"] + 0.08),
    ylim = c(st_bbox(unis_sf)["ymin"] - 0.08, st_bbox(unis_sf)["ymax"] + 0.08),
    expand = FALSE
  ) +
  labs(
    title = "Distribución de Sedes Universitarias en la ZMVM",
    subtitle = "URC y UBBJ por entidad federativa y demarcación",
    caption = "Fuente: DENUE - INEGI (2026)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.major = element_line(color = "#ebebeb", linetype = "dashed", linewidth = 0.3),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "#555555", size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Desplegar o guardar
print(mapa_universidades)
ggsave("mapa_sedes_universitarias.png", mapa_universidades, width = 8, height = 9, dpi = 300)
