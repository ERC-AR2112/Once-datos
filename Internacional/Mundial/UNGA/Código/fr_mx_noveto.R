
# Mapa: iniciativa franco-mexicana para limitar el derecho de veto
# en el Consejo de Seguridad de la ONU (atrocidades masivas, 2015-2026)
#
# Categorías:
#   1) Proponentes: México y Francia
#   2) Apoyan la declaración política (se sumaron)
#   3) Miembros permanentes que no la respaldan (EUA, Rusia, China)
#   4) Estados miembros de la ONU que NO han suscrito la declaración
#   5) Territorios que no son Estados miembros de la ONU
#      (dependencias, territorios disputados: fuera del universo)

library(sf)
library(rnaturalearth)
library(tidyverse)

proponentes <- c("MEX", "FRA")



p5 <- c("USA", "RUS", "CHN", "FRA", "GBR")

# Total de apoyos que reportan Francia y México al 21/09/2026.

total_reportado <- 128



apoyan_lista_2023 <- c(
  "ALB","AND","ARG","AUS","AUT","BEL","BEN","BIH","BRA","BGR",
  "BFA","KHM","CAN","CPV","CAF","TCD","CHL","COL","COM","CRI",
  "CIV","HRV","CYP","CZE","COD","DNK","DJI","DOM","ECU","SLV",
  "EST","FIN","GAB","GEO","DEU","GHA","GRC","GTM","GIN","GNB",
  "VAT","HND","HUN","ISL","IDN","IRQ","IRL","ITA","JPN","JOR",
  "KWT","LVA","LBN","LBY","LIE","LTU","LUX","MDG","MYS","MDV",
  "MLT","MDA","MCO","MNG","MNE","MAR","NLD","NZL","NER","MKD",
  "NOR","OMN","PLW","PSE","PAN","PNG","PER","PHL","POL","PRT",
  "QAT","KOR","ROU","RWA","WSM","SMR","SAU","SEN","SGP","SVK",
  "SVN","ZAF","ESP","SWE","CHE","THA","TLS","TGO","TUN","TUR",
  "UKR","ARE","URY","VUT"
)


apoyan_confirmados_despues <- c(
  "ARM",  # Armenia, 31 de marzo de 2025 (comunicado del MEAE francés)
  "GBR"   # Reino Unido, 21 de septiembre de 2026 (2º P5 en sumarse)
)


apoyan_por_identificar <- c(
  # "KEN", "NGA", ...
)

apoyan <- unique(c(apoyan_lista_2023, apoyan_confirmados_despues,
                   apoyan_por_identificar))
apoyan <- setdiff(apoyan, proponentes)  
se_opusieron <- c("USA", "RUS", "CHN")

# Los 193 Estados miembros de la ONU. Este es el universo de análisis:

# NOTA: Palestina y el Vaticano son observadores, no miembros, pero SÍ firmaron la declaración; el case_when de abajo los toma de 'apoyan'.
miembros_onu <- c(
  # África (54)
  "DZA","AGO","BEN","BWA","BFA","BDI","CPV","CMR","CAF","TCD","COM","COG",
  "COD","CIV","DJI","EGY","GNQ","ERI","SWZ","ETH","GAB","GMB","GHA","GIN",
  "GNB","KEN","LSO","LBR","LBY","MDG","MWI","MLI","MRT","MUS","MAR","MOZ",
  "NAM","NER","NGA","RWA","STP","SEN","SYC","SLE","SOM","ZAF","SSD","SDN",
  "TZA","TGO","TUN","UGA","ZMB","ZWE",
  # Asia (47)
  "AFG","ARM","AZE","BHR","BGD","BTN","BRN","KHM","CHN","CYP","PRK","GEO",
  "IND","IDN","IRN","IRQ","ISR","JPN","JOR","KAZ","KWT","KGZ","LAO","LBN",
  "MYS","MDV","MNG","MMR","NPL","OMN","PAK","PHL","QAT","KOR","SAU","SGP",
  "LKA","SYR","TJK","THA","TLS","TUR","TKM","ARE","UZB","VNM","YEM",
  # Europa (43)
  "ALB","AND","AUT","BLR","BEL","BIH","BGR","HRV","CZE","DNK","EST","FIN",
  "FRA","DEU","GRC","HUN","ISL","IRL","ITA","LVA","LIE","LTU","LUX","MLT",
  "MDA","MCO","MNE","NLD","MKD","NOR","POL","PRT","ROU","RUS","SMR","SRB",
  "SVK","SVN","ESP","SWE","CHE","UKR","GBR",
  # América (35)
  "ATG","ARG","BHS","BRB","BLZ","BOL","BRA","CAN","CHL","COL","CRI","CUB",
  "DMA","DOM","ECU","SLV","GRD","GTM","GUY","HTI","HND","JAM","MEX","NIC",
  "PAN","PRY","PER","KNA","LCA","VCT","SUR","TTO","USA","URY","VEN",
  # Oceanía (14)
  "AUS","FJI","KIR","MHL","FSM","NRU","NZL","PLW","PNG","WSM","SLB","TON",
  "TUV","VUT"
)
stopifnot(length(miembros_onu) == 193)


mundo <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(admin != "Antarctica")


mundo <- mundo |>
  mutate(
    iso3 = case_when(
      !is.na(iso_a3)    & iso_a3    != "-99" ~ iso_a3,
      !is.na(iso_a3_eh) & iso_a3_eh != "-99" ~ iso_a3_eh,
      TRUE                                   ~ adm0_a3
    ),
    categoria = case_when(
      iso3 %in% proponentes    ~ "Proponentes (México y Francia)",
      iso3 %in% se_opusieron   ~ "Miembros permanentes que no la respaldan",
      iso3 %in% apoyan         ~ "Se sumaron a la iniciativa (identificados)",
      iso3 %in% miembros_onu   ~ "Sin firma registrada en la lista pública",
      TRUE                     ~ "No es Estado miembro de la ONU"
    ),
    categoria = factor(
      categoria,
      levels = c("Proponentes (México y Francia)",
                 "Se sumaron a la iniciativa (identificados)",
                 "Miembros permanentes que no la respaldan",
                 "Sin firma registrada en la lista pública",
                 "No es Estado miembro de la ONU")
    )
  )


faltantes <- setdiff(c(proponentes, apoyan, se_opusieron), mundo$iso3)
if (length(faltantes) > 0) {
  message("Códigos sin correspondencia en el mapa: ",
          paste(faltantes, collapse = ", "))
}

print(count(st_drop_geometry(mundo), categoria))


identificados <- length(unique(c(proponentes, apoyan)))
sin_identificar <- total_reportado - identificados
cat("\nApoyos reportados: ", total_reportado,
    " | identificados: ", identificados,
    " | sin lista pública: ", sin_identificar, "\n", sep = "")


sin_registro <- mundo |>
  st_drop_geometry() |>
  filter(categoria == "Sin firma registrada en la lista pública") |>
  arrange(admin) |>
  pull(admin)
cat("\nSin firma registrada (", length(sin_registro), "):\n", sep = "")
print(sin_registro)

colores <- c(
  "Proponentes (México y Francia)"             = "#4A1486",
  "Se sumaron a la iniciativa (identificados)" = "#3182BD",
  "Miembros permanentes que no la respaldan"   = "#CB181D",
  "Sin firma registrada en la lista pública"   = "#E6B84C",
  "No es Estado miembro de la ONU"             = "grey88"
)


puntos_p5 <- mundo |>
  filter(iso3 %in% p5) |>
  st_transform("+proj=robin") |>
  st_centroid(of_largest_polygon = TRUE) |>
  mutate(marca = "Miembro permanente (con poder de veto)")

mapa <- ggplot(mundo) +
  geom_sf(aes(fill = categoria), color = "white", linewidth = 0.12) +
  # Marca de los P5: punto negro con halo blanco, legible sobre cualquier relleno
  geom_sf(data = puntos_p5, color = "white", size = 3.6, show.legend = FALSE) +
  geom_sf(data = puntos_p5, aes(shape = marca), color = "black", size = 2.4) +
  # Proyección Robinson (quita esta línea si prefieres coordenadas planas)
  coord_sf(crs = "+proj=robin", datum = NA) +
  scale_fill_manual(values = colores, name = NULL) +
  scale_shape_manual(values = c("Miembro permanente (con poder de veto)" = 19),
                     name = NULL) +
  labs(
    title = "Iniciativa franco-mexicana para limitar el veto en el Consejo de Seguridad",
    subtitle = "Declaración política sobre la suspensión del veto en casos de atrocidades masivas (desde 2015)",
    caption = sprintf(paste0(
      "Francia y México reportan %d apoyos al 21/09/2026, pero la última lista nominal es de enero de 2023: ",
      "aquí se identifican %d.\nLos %d restantes se sumaron en 2025-2026 sin que se publicaran sus nombres, ",
      "así que aparecen en ámbar aunque hayan firmado.\n",
      "La declaración es un compromiso voluntario: no hubo votación, y 'no la respaldan' significa que no la han suscrito.\n",
      "Elaboración propia con datos de Francia-México, globalr2p y rnaturalearth."),
      total_reportado, identificados, sin_identificar)
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(
    fill  = guide_legend(nrow = 3, byrow = TRUE, order = 1),
    shape = guide_legend(order = 2,
                         override.aes = list(size = 2.4, color = "black"))
  )

print(mapa)


if (requireNamespace("ggrepel", quietly = TRUE)) {
  etiquetas <- mundo |>
    filter(iso3 %in% proponentes) |>
    st_transform("+proj=robin") |>
    st_centroid(of_largest_polygon = TRUE) |>

    mutate(nombre = recode(iso3, MEX = "México", FRA = "Francia",
                           .default = admin))
  
  mapa <- mapa +
    ggrepel::geom_label_repel(
      data = etiquetas,
      aes(label = nombre, geometry = geometry),
      stat = "sf_coordinates",
      size = 3, fontface = "bold",
      label.size = 0.2, min.segment.length = 0
    )
  print(mapa)
}


dispositivo <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL

ggsave("UNGA/mapa_veto_onu.png", mapa, width = 11, height = 6.5, dpi = 300,
       bg = "white", device = dispositivo)

