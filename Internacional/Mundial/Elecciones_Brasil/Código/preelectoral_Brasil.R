
# =============================================================================
# Gobernadores de Brasil (situación al 18/09/2026) – partido, ideología y alineamiento con Lula / Bolsonaro
# -----------------------------------------------------------------------------
# NOTAS SOBRE LOS DATOS
# * "gobernador" = quien ejerce el cargo hoy. 11 titulares renunciaron en abril
#   de 2026 para competir por otros cargos y asumieron sus vices (o, en RJ y AM,
#   otras autoridades). RJ y RR tienen gobernadores interinos; en SC el titular
#   (Jorginho Mello, PL) está de licencia hasta el 5/10 y ejerce su vice.
# * "ideologia" es una aproximación basada en la ubicación habitual del partido
#   en el espectro brasileño (p. ej., encuestas a expertos como Bolognesi,
#   Ribeiro y Codato 2023) ajustada por la trayectoria de la persona.
# * "alineamiento" es MI clasificación aproximada según alianzas de 2026
#   (a quién apoya para presidente / con quién va la coalición estatal).
#   Revisa la columna "confianza": en varios casos es una lectura discutible.
# =============================================================================

install.packages("geobr")



library(geobr)
library(sf)
library(tidyverse)
library(patchwork)
library(tibble)

# ---- 1. Datos ---------------------------------------------------------------
gob <- tribble(
  ~uf,  ~gobernador,                          ~partido,        ~ideologia,       ~alineamiento,                 ~confianza, ~nota,
  "AC", "Mailza Assis",                       "PP",            "Centroderecha",  "Cercano a Bolsonaro",         "media",    "Asumió tras renuncia de Gladson Cameli; coalición incluye al PL",
  "AL", "Paulo Dantas",                       "MDB",           "Centro",         "Aliado de Lula",              "alta",     "Grupo de Renan Calheiros",
  "AP", "Clécio Luís",                        "União Brasil",  "Centro",         "Aliado de Lula",              "alta",     "Candidato a reelección en alianza con el PT",
  "AM", "Roberto Cidade",                     "União Brasil",  "Centroderecha",  "Pragmático / indefinido",     "baja",     "Elegido indirectamente tras renuncia de gobernador y vice",
  "BA", "Jerônimo Rodrigues",                 "PT",            "Izquierda",      "Aliado de Lula",              "alta",     "",
  "CE", "Elmano de Freitas",                  "PT",            "Izquierda",      "Aliado de Lula",              "alta",     "",
  "DF", "Celina Leão",                        "PP",            "Derecha",        "Cercano a Bolsonaro",         "alta",     "Coalición con PL (Bia Kicis, Michelle Bolsonaro al Senado)",
  "ES", "Ricardo Ferraço",                    "MDB",           "Centro",         "Aliado de Lula",              "media",    "Heredero de Casagrande (PSB)",
  "GO", "Daniel Vilela",                      "MDB",           "Centroderecha",  "Cercano a Bolsonaro",   "alta",     "Heredero de Ronaldo Caiado, candidato presidencial del PSD",
  "MA", "Carlos Brandão",                     "MDB",           "Centro",         "Pragmático / indefinido",     "baja",     "Rompió con el grupo de Flávio Dino; el PT compite contra su candidato",
  "MT", "Otaviano Pivetta",                   "PP",            "Centroderecha",  "Pragmático / indefinido",     "baja",     "Asumió tras renuncia de Mauro Mendes",
  "MS", "Eduardo Riedel",                     "PP",            "Centroderecha",  "Cercano a Bolsonaro",         "media",    "Electo en 2022 con apoyo de Bolsonaro",
  "MG", "Mateus Simões",                      "PSD",           "Derecha",        "Cercano a Bolsonaro",   "media",    "Heredero de Romeu Zema (Novo), candidato presidencial",
  "PA", "Hana Ghassan",                       "MDB",           "Centro",         "Aliado de Lula",              "alta",     "Heredera de Helder Barbalho",
  "PB", "Lucas Ribeiro",                      "PP",            "Centro",         "Aliado de Lula",              "alta",     "Candidato a reelección en alianza con el PT",
  "PR", "Ratinho Junior",                     "PSD",           "Centroderecha",  "Cercano a Bolsonaro",   "media",    "El palanque de Flávio Bolsonaro en PR es Sergio Moro (PL)",
  "PE", "Raquel Lyra",                        "PSD",           "Centro",         "Pragmático / indefinido",     "media",    "Compite contra João Campos (PSB), aliado de Lula",
  "PI", "Rafael Fonteles",                    "PT",            "Izquierda",      "Aliado de Lula",              "alta",     "",
  "RJ", "Ricardo Couto (interino)",           "Sin partido",   "Sin partido",    "Pragmático / indefinido",     "alta",     "Presidente del TJ-RJ; ejerce desde la renuncia de Cláudio Castro",
  "RN", "Fátima Bezerra",                     "PT",            "Izquierda",      "Aliado de Lula",              "alta",     "",
  "RS", "Eduardo Leite",                      "PSD",           "Centroderecha",  "Pragmático / indefinido",     "media",    "Sin apoyo presidencial definido según Poder360 (jul/2026)",
  "RO", "Marcos Rocha",                       "PSD",           "Derecha",        "Cercano a Bolsonaro",         "media",    "Histórico bolsonarista; su candidato compite contra el del PL",
  "RR", "Soldado Sampaio (interino)",         "Republicanos",  "Derecha",        "Cercano a Bolsonaro",         "baja",     "Interino tras la casación de Denarium y Damião",
  "SC", "Jorginho Mello (licencia; ejerce Marilisa Boehm)", "PL", "Derecha",     "Cercano a Bolsonaro",         "alta",     "",
  "SP", "Tarcísio de Freitas",                "Republicanos",  "Derecha",        "Cercano a Bolsonaro",         "alta",     "Exministro de Bolsonaro",
  "SE", "Fábio Mitidieri",                    "PSD",           "Centro",         "Aliado de Lula",              "alta",     "Candidato a reelección en alianza con el PT",
  "TO", "Wanderlei Barbosa",                  "Republicanos",  "Derecha",        "Cercano a Bolsonaro",         "media",    "Apoya a Dorinha (União) en coalición con el PL"
)

# Orden de los factores (para leyendas coherentes)
gob <- gob %>%
  mutate(
    ideologia = factor(ideologia, levels = c("Izquierda", "Centroizquierda", "Centro",
                                             "Centroderecha", "Derecha", "Sin partido")),
    alineamiento = factor(alineamiento, levels = c("Aliado de Lula",
                                                   "Pragmático / indefinido",
                                                   "Cercano a Bolsonaro",
                                                   "Cercano a Bolsonaro"))
  )

# ---- 2. Geometrías de los estados -------------------------------------------
# Descargas lentas o cortadas suelen "corromper" el archivo: ampliamos el timeout
options(timeout = 600)

cargar_estados <- function() {
  # Opción A: geobr, sin usar la caché (evita reutilizar un archivo dañado)
  intento <- tryCatch(
    read_state(year = 2020, simplified = TRUE, showProgress = FALSE, cache = FALSE),
    error = function(e) NULL
  )
  if (!is.null(intento) && inherits(intento, "sf")) return(intento)
  
  # Opción B: otro año de geobr
  intento <- tryCatch(
    read_state(year = 2019, simplified = TRUE, showProgress = FALSE, cache = FALSE),
    error = function(e) NULL
  )
  if (!is.null(intento) && inherits(intento, "sf")) return(intento)
  
  # Opción C: malha oficial del IBGE (descarga directa del shapefile de UFs)
  message("geobr falló; descargando la malha de UFs directamente del IBGE...")
  url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2022/Brasil/BR/BR_UF_2022.zip"
  zip <- tempfile(fileext = ".zip")
  dir <- tempfile()
  download.file(url, zip, mode = "wb")   # mode = "wb" es clave en Windows
  unzip(zip, exdir = dir)
  shp <- list.files(dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
  st_read(shp, quiet = TRUE) %>%
    rename(abbrev_state = SIGLA_UF, name_state = NM_UF) %>%
    st_simplify(dTolerance = 2000, preserveTopology = TRUE)  # aligera el dibujo
}

estados <- cargar_estados()

mapa <- estados %>%
  left_join(gob, by = c("abbrev_state" = "uf"))

# Puntos para etiquetas (point_on_surface evita que caigan fuera del polígono)
etiquetas <- st_point_on_surface(mapa)

# ---- 3. Paletas -------------------------------------------------------------
col_partido <- c(
  "PT"           = "#C8102E",
  "PSD"          = "#F2A900",
  "MDB"          = "#2E8B57",
  "PP"           = "#1F4E9C",
  "União Brasil" = "#5BA3D9",
  "Republicanos" = "#7B3F99",
  "PL"           = "#0B2545",
  "Sin partido"  = "#BDBDBD"
)

col_ideologia <- c(
  "Izquierda"       = "#B2182B",
  "Centroizquierda" = "#EF8A62",
  "Centro"          = "#F7F7F7",
  "Centroderecha"   = "#67A9CF",
  "Derecha"         = "#2166AC",
  "Sin partido"     = "#BDBDBD"
)

col_alineamiento <- c(
  "Aliado de Lula"            = "#D7301F",
  "Pragmático / indefinido"   = "#E0E0E0",
  "Cercano a Bolsonaro"       = "#1B7837"
)

# ---- 4. Función de mapa base ------------------------------------------------
mapa_base <- function(var, paleta, titulo) {
  ggplot(mapa) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.3) +
    geom_sf_text(data = etiquetas, aes(label = abbrev_state),
                 size = 2.6, fontface = "bold", color = "grey15") +
    scale_fill_manual(values = paleta, drop = FALSE, name = NULL) +
    labs(title = titulo) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    ) +
    guides(fill = guide_legend(nrow = 2))
}

m_partido     <- mapa_base("partido", col_partido, "Partido del gobernador")
m_ideologia   <- mapa_base("ideologia", col_ideologia, "Tendencia ideológica (aprox.)")
m_alineamiento <- mapa_base("alineamiento", col_alineamiento, "Cercanía a Lula o Bolsonaro (aprox.)")

# ---- 5. Mostrar y guardar ---------------------------------------------------
print(m_partido)
print(m_ideologia)
print(m_alineamiento)

panel <- (m_partido | m_ideologia | m_alineamiento) +
  plot_annotation(
    title = "Gobernadores de Brasil – septiembre de 2026",
    caption = "Fuente: elaboración propia con base en prensa brasileña (ISTOÉ, Poder360, CNN Brasil, entre otros). Geometrías: geobr/IBGE.\nIdeología y alineamiento son clasificaciones aproximadas."
  )

ggsave("Elecciones_Brasil/gobernadores_partido.png",      m_partido,      width = 7,  height = 7.5, dpi = 300, bg = "white")
ggsave("Elecciones_Brasil/gobernadores_ideologia.png",    m_ideologia,    width = 7,  height = 7.5, dpi = 300, bg = "white")
ggsave("Elecciones_Brasil/gobernadores_alineamiento.png", m_alineamiento, width = 7,  height = 7.5, dpi = 300, bg = "white")
ggsave("Elecciones_Brasil/gobernadores_panel.png",        panel,          width = 18, height = 7.5, dpi = 300, bg = "white")

####Mapa resultados anteriores#####

install.packages(c("data.table", "scales", "stringi"))
library(data.table)
library(scales)


votos <- read_delim("Elecciones_Brasil/votacao_candidato_munzona_2022_BR.csv", 
                                                delim = ";", escape_double = FALSE,                                            trim_ws = TRUE)

seg_vuelta <- votos %>%
  filter(NR_TURNO == 2,
         toupper(DS_CARGO) == "PRESIDENTE",
         NR_CANDIDATO %in% c(13, 22)) %>%            # 13 = Lula, 22 = Bolsonaro
  mutate(candidato = if_else(NR_CANDIDATO == 13, "lula", "bolsonaro"))

# ---- 3. Agregar por estado -------------------------------------------------
res_uf <- seg_vuelta %>%
  filter(SG_UF != "ZZ") %>%                          # ZZ = votos en el exterior
  group_by(SG_UF, candidato) %>%
  summarise(votos = sum(QT_VOTOS_NOMINAIS), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = candidato, values_from = votos) %>%
  mutate(
    validos   = lula + bolsonaro,
    pct_lula  = 100 * lula / validos,
    pct_bolso = 100 * bolsonaro / validos,
    ganador   = if_else(lula > bolsonaro, "Lula (PT)", "Bolsonaro (PL)"),
    margen    = pct_lula - pct_bolso                 # + = ventaja Lula
  )

# Chequeo: total nacional (incluye exterior). Debe dar ~50,9% Lula / ~49,1% Bolsonaro
nacional <- seg_vuelta %>%
  group_by(candidato) %>% summarise(votos = sum(QT_VOTOS_NOMINAIS))
print(nacional %>% mutate(pct = round(100 * votos / sum(votos), 2)))

# ---- 4. Unir con las geometrías --------------------------------------------
mapa_2022 <- estados %>%
  left_join(res_uf, by = c("abbrev_state" = "SG_UF"))

etiq_2022 <- st_point_on_surface(mapa_2022) %>%
  mutate(lab = paste0(abbrev_state, "\n", number(pct_lula, accuracy = 0.1,
                                                 decimal.mark = ","), "%"))

# ---- 5. Mapas por estado ----------------------------------------------------
col_ganador <- c("Lula (PT)" = "#C8102E", "Bolsonaro (PL)" = "#1B4F9C")

m_ganador <- ggplot(mapa_2022) +
  geom_sf(aes(fill = ganador), color = "white", linewidth = 0.3) +
  geom_sf_text(data = etiq_2022, aes(label = abbrev_state),
               size = 2.6, fontface = "bold", color = "white") +
  scale_fill_manual(values = col_ganador, name = NULL) +
  labs(title = "Ganador por estado",
       caption = "Fuente: TSE – Portal de Dados Abertos (votação nominal por município e zona). Excluye votos en el exterior.") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")

m_pct <- ggplot(mapa_2022) +
  geom_sf(aes(fill = pct_lula), color = "white", linewidth = 0.3) +
  geom_sf_text(data = etiq_2022, aes(label = lab),
               size = 2.2, lineheight = 0.85, color = "grey10") +
  scale_fill_gradient2(low = "#1B4F9C", mid = "#F7F7F7", high = "#C8102E",
                       midpoint = 50, limits = c(20, 80), oob = squish,
                       name = "% Lula (votos\nválidos)") +
  labs(title = "Porcentaje de Lula por estado",
       caption = "Fuente: TSE – Portal de Dados Abertos (votação nominal por município e zona). Excluye votos en el exterior."
  ) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"))

panel_2022 <- (m_ganador | m_pct) +
  plot_annotation(
    title = "Brasil 2022 – Segunda vuelta presidencial (30/10/2022)",
    caption = "Fuente: TSE – Portal de Dados Abertos (votação nominal por município e zona). Excluye votos en el exterior."
  )

print(panel_2022)
ggsave("Elecciones_Brasil/segunda_vuelta_2022_estados.png", panel_2022,
       width = 13, height = 7.5, dpi = 300, bg = "white")
ggsave("Elecciones_Brasil/porcentaje_2022_estados.png", m_pct,
       width = 13, height = 7.5, dpi = 300, bg = "white")
ggsave("Elecciones_Brasil/ganador_2022_estados.png", m_ganador,
       width = 13, height = 7.5, dpi = 300, bg = "white")

# Tabla resumen ordenada (útil para revisar o exportar)
res_uf %>% arrange(desc(pct_lula)) %>% print(n = 27)
# write.csv(res_uf, "resultados_2v_2022_por_uf.csv", row.names = FALSE)


# =============================================================================
# 6. (OPCIONAL) Mapa por municipio
# -----------------------------------------------------------------------------
# El TSE usa códigos de municipio propios (no los del IBGE), así que unimos por
# nombre normalizado + UF. Al final se reportan los que no empataron.
# La descarga de ~5.570 municipios es pesada; simplified = TRUE ayuda.
# =============================================================================
hacer_municipios <- TRUE

if (hacer_municipios) {
  
  normalizar <- function(x) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
    x <- toupper(x)
    x <- gsub("[^A-Z ]", " ", x)       # quita guiones, apóstrofos, etc.
    gsub("\\s+", " ", trimws(x))
  }
  
  res_mun <- seg_vuelta %>%
    filter(SG_UF != "ZZ") %>%
    group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, candidato) %>%
    summarise(votos = sum(QT_VOTOS_NOMINAIS), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = candidato, values_from = votos, values_fill = 0) %>%
    mutate(pct_lula = 100 * lula / (lula + bolsonaro),
           clave = paste(SG_UF, normalizar(NM_MUNICIPIO)))
  
  # Geometrías municipales: geobr y, si falla, malha del IBGE
  municipios <- tryCatch(
    geobr::read_municipality(year = 2020, simplified = TRUE,
                             showProgress = FALSE, cache = FALSE),
    error = function(e) NULL
  )
  if (is.null(municipios)) {
    message("geobr falló; descargando malha municipal del IBGE...")
    url_mun <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2022/Brasil/BR/BR_Municipios_2022.zip"
    zip_mun <- file.path(dir_datos, "BR_Municipios_2022.zip")
    if (!file.exists(zip_mun)) download.file(url_mun, zip_mun, mode = "wb")
    unzip(zip_mun, exdir = file.path(dir_datos, "mun"))
    shp <- list.files(file.path(dir_datos, "mun"), "\\.shp$",
                      full.names = TRUE, recursive = TRUE)[1]
    municipios <- st_read(shp, quiet = TRUE) %>%
      rename(name_muni = NM_MUN, abbrev_state = SIGLA_UF) %>%
      st_simplify(dTolerance = 500, preserveTopology = TRUE)
  }
  
  mapa_mun <- municipios %>%
    mutate(clave = paste(abbrev_state, normalizar(name_muni))) %>%
    left_join(res_mun %>% select(clave, pct_lula), by = "clave")
  
  # Diagnóstico de empates (suele haber una decena de nombres con grafías distintas)
  sin_dato <- mapa_mun %>% st_drop_geometry() %>% filter(is.na(pct_lula))
  message("Municipios sin empatar: ", nrow(sin_dato))
  if (nrow(sin_dato) > 0) print(sin_dato %>% select(abbrev_state, name_muni))
  
  m_mun <- ggplot(mapa_mun) +
    geom_sf(aes(fill = pct_lula), color = NA) +
    geom_sf(data = estados, fill = NA, color = "grey20", linewidth = 0.25) +
    scale_fill_gradient2(low = "#1B4F9C", mid = "#F7F7F7", high = "#C8102E",
                         midpoint = 50, limits = c(0, 100),
                         na.value = "grey70", name = "% Lula") +
    labs(title = "Brasil 2022 – 2ª vuelta: % de Lula por municipio",
         caption = "Fuente: TSE. Gris = municipio sin empatar por nombre.") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "right")
  
  print(m_mun)
  ggsave("segunda_vuelta_2022_municipios.png", m_mun,
         width = 9, height = 9, dpi = 300, bg = "white")
}



#####Graffica de resultado de Polling Data primera vuelta

# 1. Datos de la primera vuelta con los intervalos de confianza (al 22/09/2026)
datos_primera_vuelta <- data.frame(
  candidato  = c("Lula", "Flávio Bolsonaro", "Inválido", "Augusto Cury"),
  porcentaje = c(39.1, 36.2, 9.0, 6.4),
  ic_min     = c(34.7, 30.9, 1.8, 1.4),
  ic_max     = c(43.5, 41.5, 16.3, 11.3)
)

# Ordenar de mayor a menor según el porcentaje central
datos_primera_vuelta$candidato <- factor(
  datos_primera_vuelta$candidato,
  levels = datos_primera_vuelta$candidato[order(-datos_primera_vuelta$porcentaje)]
)

# 2. Paleta de colores acorde a la gráfica
colores <- c(
  "Lula"             = "#d93829",
  "Flávio Bolsonaro" = "#1f78b4",
  "Inválido"         = "#808080",
  "Augusto Cury"     = "#f28e2b"
)

# 3. Gráfico de barras con intervalos de confianza
ggplot(datos_primera_vuelta, aes(x = candidato, y = porcentaje, fill = candidato)) +
  geom_col(width = 0.6, show.legend = FALSE, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = ic_min, ymax = ic_max),
    width = 0.2,
    linewidth = 0.8,
    color = "#2b2b2b"
  ) +
  geom_text(
    aes(
      y = ic_max,
      label = paste0(porcentaje, "%\n(", ic_min, "% - ", ic_max, "%)")
    ),
    vjust = -0.3,
    lineheight = 0.85,
    fontface = "bold",
    size = 3.8
  ) +
  scale_fill_manual(values = colores) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 52),
    breaks = seq(0, 50, by = 10)
  ) +
  labs(
    title = "Elección Presidencial 2026 - 1.ª Vuelta",
    subtitle = "Estimación al 22/09/2026 con intervalos de confianza",
    x = NULL,
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1a2a3a"),
    plot.subtitle = element_text(color = "#666666", size = 11, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e5e5e5"),
    axis.text.x = element_text(face = "bold", size = 10.5),
    axis.line.x = element_line(color = "#333333")
  )

### Gráfica polling data segunda vuelta 

# 1. Datos de la segunda vuelta con intervalos de confianza
datos_segunda_vuelta <- data.frame(
  candidato = c("Flávio Bolsonaro", "Lula", "Inválido"),
  porcentaje = c(45.0, 44.4, 10.6),
  ic_min     = c(40.9, 39.7, 6.1),
  ic_max     = c(49.2, 49.0, 15.0)
)

# Ordenar de mayor a menor porcentaje
datos_segunda_vuelta$candidato <- factor(
  datos_segunda_vuelta$candidato,
  levels = datos_segunda_vuelta$candidato[order(-datos_segunda_vuelta$porcentaje)]
)

# 2. Paleta de colores acorde a la gráfica
colores <- c(
  "Flávio Bolsonaro" = "#1f78b4",
  "Lula"             = "#d93829",
  "Inválido"         = "#808080"
)

# 3. Gráfico de barras con barras de error
ggplot(datos_segunda_vuelta, aes(x = candidato, y = porcentaje, fill = candidato)) +
  # Barras principales
  geom_col(width = 0.6, show.legend = FALSE, alpha = 0.9) +
  # Intervalos de confianza (barras de error)
  geom_errorbar(
    aes(ymin = ic_min, ymax = ic_max),
    width = 0.2,
    linewidth = 0.8,
    color = "#2b2b2b"
  ) +
  # Etiqueta de valor y rango de IC sobre cada barra
  geom_text(
    aes(
      y = ic_max,
      label = paste0(porcentaje, "%\n(", ic_min, "% - ", ic_max, "%)")
    ),
    vjust = -0.3,
    lineheight = 0.85,
    fontface = "bold",
    size = 3.8
  ) +
  scale_fill_manual(values = colores) +
  # Eje Y con espacio suficiente para las etiquetas superiores
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 60),
    breaks = seq(0, 50, by = 10)
  ) +
  labs(
    title = "Elección Presidencial 2026 - 2.ª Vuelta",
    subtitle = "Estimación al 24/09/2026 con intervalos de confianza",
    x = NULL,
    y = "Porcentaje", 
    caption = "Fuente: Agregador de encuestas PollingData al 18 de septiembre de 2026"  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1a2a3a"),
    plot.subtitle = element_text(color = "#666666", size = 11, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e5e5e5"),
    axis.text.x = element_text(face = "bold", size = 10.5),
    axis.line.x = element_line(color = "#333333")
  )
