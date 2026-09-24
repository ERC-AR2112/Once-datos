# =============================================================================
# RADIOGRAFÍA COMPARADA DE LOS 193 ESTADOS MIEMBROS DE LA ONU
# Población · Riqueza · Pobreza · Desarrollo humano
#
# Fuentes (se descargan automáticamente al ejecutar el script):
#   1. Banco Mundial - World Development Indicators (WDI), vía paquete {WDI}
#   2. PNUD - Informe de Desarrollo Humano 2025 (serie histórica de índices)


paquetes <- c("WDI", "dplyr", "tidyr", "readr", "ggplot2", "scales", "stringr")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))

for (d in c("datos", "resultados", "figuras")) dir.create(d, showWarnings = FALSE)

ANIO_INICIO <- 2010                                   # ventana de búsqueda
ANIO_FIN    <- as.integer(format(Sys.Date(), "%Y"))   # el año en curso
theme_set(theme_minimal(base_size = 12))


# 1. LOS 193 ESTADOS MIEMBROS (códigos ISO-3) 
miembros_onu <- c(
  "AFG","ALB","DZA","AND","AGO","ATG","ARG","ARM","AUS","AUT","AZE","BHS","BHR",
  "BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL","BIH","BWA","BRA","BRN","BGR",
  "BFA","BDI","CPV","KHM","CMR","CAN","CAF","TCD","CHL","CHN","COL","COM","COG",
  "CRI","CIV","HRV","CUB","CYP","CZE","PRK","COD","DNK","DJI","DMA","DOM","ECU",
  "EGY","SLV","GNQ","ERI","EST","SWZ","ETH","FJI","FIN","FRA","GAB","GMB","GEO",
  "DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND","HUN","ISL","IND",
  "IDN","IRN","IRQ","IRL","ISR","ITA","JAM","JPN","JOR","KAZ","KEN","KIR","KWT",
  "KGZ","LAO","LVA","LBN","LSO","LBR","LBY","LIE","LTU","LUX","MDG","MWI","MYS",
  "MDV","MLI","MLT","MHL","MRT","MUS","MEX","FSM","MDA","MCO","MNG","MNE","MAR",
  "MOZ","MMR","NAM","NRU","NPL","NLD","NZL","NIC","NER","NGA","MKD","NOR","OMN",
  "PAK","PLW","PAN","PNG","PRY","PER","PHL","POL","PRT","QAT","KOR","ROU","RUS",
  "RWA","KNA","LCA","VCT","WSM","SMR","STP","SAU","SEN","SRB","SYC","SLE","SGP",
  "SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","SDN","SUR","SWE","CHE","SYR",
  "TJK","TZA","THA","TLS","TGO","TON","TTO","TUN","TUR","TKM","TUV","UGA","UKR",
  "ARE","GBR","USA","URY","UZB","VUT","VEN","VNM","YEM","ZMB","ZWE"
)
stopifnot(length(miembros_onu) == 193, !anyDuplicated(miembros_onu))


# 2. DESCARGA DE DATOS 

## 2.1 Banco Mundial (WDI) 
# Catálogo completo: https://data.worldbank.org/indicator
indicadores_wdi <- c(
  # Población
  poblacion        = "SP.POP.TOTL",        # población total
  crec_poblacion   = "SP.POP.GROW",        # crecimiento anual (%)
  pob_65mas        = "SP.POP.65UP.TO.ZS",  # % de población de 65 años o más
  urbana           = "SP.URB.TOTL.IN.ZS",  # % población urbana
  fecundidad       = "SP.DYN.TFRT.IN",     # hijos por mujer
  # Riqueza
  pib_usd          = "NY.GDP.MKTP.CD",     # PIB, US$ corrientes
  pib_pc_usd       = "NY.GDP.PCAP.CD",     # PIB per cápita, US$ corrientes
  pib_pc_ppa       = "NY.GDP.PCAP.PP.KD",  # PIB per cápita, PPA (US$ int. 2021)
  # Pobreza y desigualdad
  pobreza_3usd     = "SI.POV.DDAY",        # % bajo línea de US$3.00/día (PPA 2021)
  pobreza_4_2usd   = "SI.POV.LMIC",        # % bajo US$4.20/día (ingreso medio-bajo)
  pobreza_8_3usd   = "SI.POV.UMIC",        # % bajo US$8.30/día (ingreso medio-alto)
  gini             = "SI.POV.GINI",        # índice de Gini (0-100) dentro del país
  pobreza_mpi      = "SI.POV.MPUN",        # pobreza multidimensional (PNUD/OPHI)
  # Bienestar
  esperanza_vida   = "SP.DYN.LE00.IN",     # esperanza de vida al nacer
  mortalidad_5     = "SH.DYN.MORT",        # mortalidad <5 años (por 1.000 nac.)
  alfabetismo      = "SE.ADT.LITR.ZS",     # % alfabetismo adultos
  electricidad     = "EG.ELC.ACCS.ZS"      # % con acceso a electricidad
)

archivo_wdi <- "datos/wdi_crudo.rds"
if (!file.exists(archivo_wdi)) {
  message("Descargando WDI del Banco Mundial (puede tardar 1-2 min)...")
  wdi_crudo <- WDI(
    country   = "all",
    indicator = indicadores_wdi,
    start     = ANIO_INICIO,
    end       = ANIO_FIN,
    extra     = TRUE       # agrega region, income (nivel de ingreso), etc.
  )
  saveRDS(wdi_crudo, archivo_wdi)
}
wdi_crudo <- readRDS(archivo_wdi)

## PNUD: Índice de Desarrollo Humano y componentes
#  https://hdr.undp.org/data-center/documentation-and-downloads
url_hdr     <- "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv"
archivo_hdr <- "datos/hdr_serie_completa.csv"
if (!file.exists(archivo_hdr)) {
  message("Descargando datos del PNUD (IDH)...")
  download.file(url_hdr, archivo_hdr, mode = "wb")
}
# latin1 evita errores con acentos; no usamos los nombres de país del PNUD.
hdr_crudo <- read_csv(archivo_hdr, locale = locale(encoding = "latin1"),
                      show_col_types = FALSE, na = c("", "NA", ".."))


ultimo_valor <- function(df, vars) {
  larga <- df |>
    select(iso3c, year, all_of(vars)) |>
    pivot_longer(all_of(vars), names_to = "indicador", values_to = "valor") |>
    filter(!is.na(valor)) |>
    group_by(iso3c, indicador) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup()
  valores <- larga |>
    select(iso3c, indicador, valor) |>
    pivot_wider(names_from = indicador, values_from = valor)
  anios <- larga |>
    transmute(iso3c, indicador = paste0("anio_", indicador), year) |>
    pivot_wider(names_from = indicador, values_from = year)
  left_join(valores, anios, by = "iso3c")
}

wdi_ultimo <- ultimo_valor(wdi_crudo, names(indicadores_wdi))


metadatos <- wdi_crudo |>
  filter(iso3c %in% miembros_onu) |>
  transmute(iso3c, pais = country, region, income) |>
  distinct(iso3c, .keep_all = TRUE) |>
  mutate(grupo_ingreso = factor(
    income,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"),
    labels = c("Bajo", "Medio-bajo", "Medio-alto", "Alto")
  )) |>
  select(-income)


anio_hdr <- hdr_crudo |>
  names() |>
  str_extract("(?<=^hdi_)\\d{4}$") |>
  as.integer() |>
  max(na.rm = TRUE)
message("Último año del IDH en el archivo del PNUD: ", anio_hdr)


col_hdr <- function(prefijo) {
  nm <- paste0(prefijo, "_", anio_hdr)
  if (nm %in% names(hdr_crudo)) suppressWarnings(as.numeric(hdr_crudo[[nm]]))
  else rep(NA_real_, nrow(hdr_crudo))
}

hdr <- tibble(
  iso3c            = hdr_crudo$iso3,
  categoria_idh    = hdr_crudo$hdicode,
  ranking_idh      = col_hdr("hdi_rank"),
  idh              = col_hdr("hdi"),          # Índice de Desarrollo Humano
  idh_ajustado_des = col_hdr("ihdi"),         # IDH ajustado por desigualdad
  indice_genero_gii = col_hdr("gii"),         # Índice de Desigualdad de Género
  anios_escol_esp  = col_hdr("eys"),          # años esperados de escolaridad
  anios_escol_prom = col_hdr("mys"),          # años promedio de escolaridad
  ingreso_nacional_pc = col_hdr("gnipc")      # INB per cápita (PPA 2021)
) |>
  filter(iso3c %in% miembros_onu) |>
  mutate(categoria_idh = factor(categoria_idh,
                                levels = c("Low", "Medium", "High", "Very High"),
                                labels = c("Bajo", "Medio", "Alto", "Muy alto")))


datos <- tibble(iso3c = miembros_onu) |>
  left_join(metadatos,  by = "iso3c") |>
  left_join(wdi_ultimo, by = "iso3c") |>
  left_join(hdr,        by = "iso3c") |>
  mutate(
    poblacion_mill  = poblacion / 1e6,
    pib_mmill_usd   = pib_usd / 1e9,
    log_pib_pc_ppa  = log10(pib_pc_ppa),
    # Estimación aproximada de personas en pobreza extrema (mezcla años distintos)
    pobres_3usd_mill = poblacion_mill * pobreza_3usd / 100
  )
stopifnot(nrow(datos) == 193)

write_csv(datos, "unga/base_193_paises.csv")


cobertura <- datos |>
  summarise(across(where(is.numeric), ~ sum(!is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "paises_con_dato") |>
  filter(!str_starts(variable, "anio_")) |>
  mutate(pct = round(100 * paises_con_dato / 193, 1)) |>
  arrange(pct)
write_csv(cobertura, "unga/cobertura_por_variable.csv")
print(cobertura, n = Inf)

resumen_ingreso <- datos |>
  filter(!is.na(grupo_ingreso)) |>
  group_by(grupo_ingreso) |>
  summarise(
    n_paises            = n(),
    poblacion_mill      = sum(poblacion_mill, na.rm = TRUE),
    pib_pc_ppa_mediana  = median(pib_pc_ppa, na.rm = TRUE),
    idh_mediana         = median(idh, na.rm = TRUE),
    esperanza_vida_med  = median(esperanza_vida, na.rm = TRUE),
    pobreza_3usd_med    = median(pobreza_3usd, na.rm = TRUE),
    fecundidad_med      = median(fecundidad, na.rm = TRUE),
    .groups = "drop"
  )
print(resumen_ingreso)
write_csv(resumen_ingreso, "resultados/resumen_por_grupo_ingreso.csv")


resumen_region <- datos |>
  filter(!is.na(region)) |>
  group_by(region) |>
  summarise(
    n_paises          = n(),
    poblacion_mill    = sum(poblacion_mill, na.rm = TRUE),
    pib_pc_ppa_mediana = median(pib_pc_ppa, na.rm = TRUE),
    idh_mediana       = median(idh, na.rm = TRUE),
    pobreza_3usd_med  = median(pobreza_3usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(idh_mediana))
print(resumen_region)
write_csv(resumen_region, "resultados/resumen_por_region.csv")


concentracion <- function(var, n = 10) {
  x <- datos |> filter(!is.na(.data[[var]])) |> arrange(desc(.data[[var]]))
  tibble(variable = var, paises_con_dato = nrow(x),
         participacion_top = sum(head(x[[var]], n)) / sum(x[[var]]))
}
conc <- bind_rows(concentracion("poblacion", 10), concentracion("pib_usd", 10))
conc$participacion_top <- percent(conc$participacion_top, accuracy = 0.1)
cat("\nParticipación de los 10 países más grandes (dentro de los 193):\n")
print(conc)

gini_pond <- function(x, w = rep(1, length(x))) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  p  <- w / sum(w)
  F_ <- cumsum(p) - p / 2
  2 * sum(p * x * F_) / sum(p * x) - 1
}

d_gini <- datos |> filter(!is.na(pib_pc_ppa), !is.na(poblacion))
desigualdad_entre_paises <- tibble(
  medida = c("Gini del PIB pc (PPA), cada país cuenta 1",
             "Gini del PIB pc (PPA), ponderado por población",
             "Razón p90/p10 del PIB pc (PPA)",
             "Razón PIB pc del país más rico / más pobre"),
  valor  = c(gini_pond(d_gini$pib_pc_ppa),
             gini_pond(d_gini$pib_pc_ppa, d_gini$poblacion),
             unname(quantile(d_gini$pib_pc_ppa, .9) / quantile(d_gini$pib_pc_ppa, .1)),
             max(d_gini$pib_pc_ppa) / min(d_gini$pib_pc_ppa))
)
print(desigualdad_entre_paises)
write_csv(desigualdad_entre_paises, "resultados/desigualdad_entre_paises.csv")


top_bottom <- function(var, n = 10) {
  x <- datos |> filter(!is.na(.data[[var]])) |> select(pais, iso3c, all_of(var))
  list(mas_alto = slice_max(x, .data[[var]], n = n),
       mas_bajo = slice_min(x, .data[[var]], n = n))
}
print(top_bottom("idh"))
print(top_bottom("pib_pc_ppa"))


print(datos |>
        filter(!is.na(pobres_3usd_mill)) |>
        slice_max(pobres_3usd_mill, n = 10) |>
        select(pais, pobreza_3usd, anio_pobreza_3usd, pobres_3usd_mill))


brecha <- datos |>
  filter(!is.na(pib_pc_ppa), !is.na(idh)) |>
  mutate(rank_pib = rank(-pib_pc_ppa), rank_idh = rank(-idh),
         brecha_rank = rank_pib - rank_idh) |>
  arrange(desc(brecha_rank)) |>
  select(pais, pib_pc_ppa, idh, rank_pib, rank_idh, brecha_rank)
cat("\nDesarrollo por encima de lo que predice la riqueza:\n"); print(head(brecha, 10))
cat("\nRiqueza por encima de lo que predice el desarrollo:\n"); print(tail(brecha, 10))
write_csv(brecha, "resultados/brecha_riqueza_desarrollo.csv")


vars_cor <- c("poblacion", "pib_pc_ppa", "pobreza_3usd", "gini", "idh",
              "esperanza_vida", "fecundidad", "urbana", "mortalidad_5",
              "anios_escol_prom", "indice_genero_gii")
matriz_cor <- datos |>
  select(all_of(vars_cor)) |>
  cor(use = "pairwise.complete.obs", method = "spearman") |>
  round(2)
print(matriz_cor)
write.csv(matriz_cor, "resultados/correlaciones_spearman.csv")

## Propuesta de PCA, puede ser complicada. Se omité en documento
vars_cluster <- c("log_pib_pc_ppa", "esperanza_vida", "idh", "fecundidad",
                  "urbana", "mortalidad_5")
base_cl <- datos |>
  select(iso3c, pais, poblacion_mill, all_of(vars_cluster)) |>
  mutate(mortalidad_5 = log10(mortalidad_5)) |>
  drop_na()
message("Países con datos completos para el clustering: ", nrow(base_cl))

X   <- scale(base_cl[vars_cluster])
pca <- prcomp(X)
cat("\nVarianza explicada por componente:\n")
print(round(summary(pca)$importance[2, ], 3))

set.seed(2026)
K  <- 4
km <- kmeans(X, centers = K, nstart = 50)
base_cl$cluster <- km$cluster
base_cl$PC1 <- pca$x[, 1]
base_cl$PC2 <- pca$x[, 2]


orden <- base_cl |>
  group_by(cluster) |> summarise(m = mean(idh)) |> arrange(m)
base_cl$tipologia <- factor(base_cl$cluster, levels = orden$cluster,
                            labels = c("1 (menor desarrollo)", "2", "3", "4 (mayor desarrollo)"))

tipologia <- base_cl |>
  group_by(tipologia) |>
  summarise(n = n(), across(all_of(vars_cluster), ~ round(mean(.x), 2)), .groups = "drop")
print(tipologia)
write_csv(select(base_cl, iso3c, pais, tipologia, PC1, PC2), "resultados/tipologia_paises.csv")



guardar <- function(g, nombre, w = 9, h = 6)
  ggsave(file.path("figuras", nombre), g, width = w, height = h, dpi = 300, bg = "white")

pal_ingreso <- c(Bajo = "#c0392b", `Medio-bajo` = "#e67e22",
                 `Medio-alto` = "#2e86c1", Alto = "#1e8449")

# Riqueza vs. esperanza de vida (tamaño = población)
g1 <- datos |>
  filter(!is.na(pib_pc_ppa), !is.na(esperanza_vida), !is.na(grupo_ingreso)) |>
  ggplot(aes(pib_pc_ppa, esperanza_vida, size = poblacion_mill, colour = grupo_ingreso)) +
  geom_point(alpha = .7) +
  scale_x_log10(labels = label_dollar()) +
  scale_size_area(max_size = 16, labels = label_comma(), name = "Población (mill.)") +
  scale_colour_manual(values = pal_ingreso, name = "Grupo de ingreso") +
  labs(title = "Más riqueza, más años de vida... con rendimientos decrecientes",
       x = "PIB per cápita, PPA (US$ 2021, escala log)", y = "Esperanza de vida (años)",
       caption = "Fuente: Banco Mundial (WDI). Último dato disponible por país.")
guardar(g1, "01_riqueza_vs_esperanza_vida.png")

# Distribución del IDH por región
g2 <- datos |>
  filter(!is.na(idh), !is.na(region)) |>
  ggplot(aes(reorder(region, idh, median), idh)) +
  geom_boxplot(fill = "#aed6f1", outlier.shape = NA) +
  geom_jitter(width = .15, alpha = .5, size = 1.4) +
  coord_flip() +
  labs(title = "Desarrollo humano por región", x = NULL, y = "IDH",
       caption = "Fuente: PNUD, Informe de Desarrollo Humano 2025.")
guardar(g2, "02_idh_por_region.png")

# Pobreza extrema vs. riqueza
g3 <- datos |>
  filter(!is.na(pobreza_3usd), !is.na(pib_pc_ppa)) |>
  ggplot(aes(pib_pc_ppa, pobreza_3usd)) +
  geom_point(aes(size = poblacion_mill), alpha = .6, colour = "#c0392b") +
  geom_smooth(method = "loess", se = FALSE, colour = "grey30", linewidth = .7) +
  geom_text(data = ~ slice_max(.x, poblacion_mill, n = 8), aes(label = iso3c),
            size = 3, vjust = -1.2) +
  scale_x_log10(labels = label_dollar()) +
  scale_size_area(max_size = 14, name = "Población (mill.)") +
  labs(title = "Pobreza extrema (< US$3 al día) y riqueza nacional",
       x = "PIB per cápita, PPA (US$ 2021, escala log)",
       y = "% de la población bajo US$3.00/día",
       caption = "Fuente: Banco Mundial (WDI/PIP). Años de encuesta distintos por país.")
guardar(g3, "03_pobreza_vs_riqueza.png")

g3_1  <- base_cl|> 
  slice_max(log_pib_pc_ppa, n = 5) |>
  ggplot(aes( x= pais, y =log_pib_pc_ppa)) +
  geom_col(fill = "#2e86c1") +
  geom_text(aes(label = number( log_pib_pc_ppa)), hjust = -.15, size = 3.2) +
  labs(title = "Los 5 Estados miembros más Ricos", y = "PIB per capita", x = "País")
g3_1
guardar(g3_1, "03.1_paises_mas_ricos.png")

# Los 15 países más poblados
g4 <- datos |>
  slice_max(poblacion_mill, n = 15) |>
  ggplot(aes(reorder(pais, poblacion_mill), poblacion_mill)) +
  geom_col(fill = "#2e86c1") +
  geom_text(aes(label = number(poblacion_mill, accuracy = 1)), hjust = -.15, size = 3.2) +
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, .12))) +
  labs(title = "Los 15 Estados miembros más poblados", x = NULL, y = "Millones de habitantes")
guardar(g4, "04_paises_mas_poblados.png")

# Curva de Lorenz entre países (población vs. PIB mundial). Complicada de comunicar, se omite de documento
lor <- datos |>
  filter(!is.na(pib_pc_usd), !is.na(poblacion), !is.na(pib_usd)) |>
  arrange(pib_pc_usd) |>
  mutate(cum_pob = cumsum(poblacion) / sum(poblacion),
         cum_pib = cumsum(pib_usd) / sum(pib_usd))
g5 <- ggplot(lor, aes(cum_pob, cum_pib)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_area(alpha = .15, fill = "#c0392b") +
  geom_line(linewidth = 1, colour = "#c0392b") +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  coord_equal() +
  labs(title = "Curva de Lorenz entre países",
       subtitle = "Países ordenados de menor a mayor PIB per cápita (US$ corrientes)",
       x = "% acumulado de la población de los países", y = "% acumulado del PIB")
guardar(g5, "05_lorenz_entre_paises.png", w = 6.5, h = 6.5)

# Mapa de tipologías en el plano de los dos primeros componentes principales
g6 <- ggplot(base_cl, aes(PC1, PC2, colour = tipologia)) +
  geom_point(aes(size = poblacion_mill), alpha = .75) +
  geom_text(data = ~ slice_max(.x, poblacion_mill, n = 15), aes(label = iso3c),
            size = 3, colour = "black", vjust = -1.1) +
  scale_size_area(max_size = 14, name = "Población (mill.)") +
  labs(title = "Tipología de países según desarrollo, demografía y riqueza",
       x = paste0("Componente 1 (", percent(summary(pca)$importance[2, 1], .1), ")"),
       y = paste0("Componente 2 (", percent(summary(pca)$importance[2, 2], .1), ")"),
       colour = "Tipología")
guardar(g6, "06_tipologia_pca.png", w = 10, h = 6.5)

# Gini interno (desigualdad dentro de cada país) por grupo de ingreso
g7 <- datos |>
  filter(!is.na(gini), !is.na(grupo_ingreso)) |>
  ggplot(aes(grupo_ingreso, gini, fill = grupo_ingreso)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA) +
  geom_jitter(width = .15, alpha = .5, show.legend = FALSE) +
  scale_fill_manual(values = pal_ingreso) +
  labs(title = "Desigualdad de ingreso dentro de los países",
       x = "Grupo de ingreso (Banco Mundial)", y = "Índice de Gini (0-100)")
guardar(g7, "07_gini_por_grupo_ingreso.png")

# =============================================================================
# MAPA MUNDIAL DEL ÍNDICE DE DESARROLLO HUMANO (IDH) - PROYECCIÓN EQUAL EARTH
#
# Proyección: Equal Earth (Šavrič, Patterson y Jenny, 2018), la que la Asamblea General de la ONU recomendó el 4-sep-2026 para cuando "importa el tamaño relativo" (resolución no vinculante). 

paquetes <- c("sf", "dplyr", "readr", "ggplot2", "scales", "stringr")
paquetes_datos_mapa <- c("rnaturalearth", "rnaturalearthdata")   # se usan con ::
faltan <- setdiff(c(paquetes, paquetes_datos_mapa), rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))

dir.create("figuras", showWarnings = FALSE)
ANIO_IDH <- 2023     # año del IDH en el Informe 2025 del PNUD; ajústalo si cambia
theme_set(theme_void(base_size = 12))

# Proyecciones (cadenas PROJ). Equal Earth requiere PROJ >= 5.2 (sf reciente).
PROJ_EQUAL_EARTH <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"
PROJ_MERCATOR    <- "EPSG:3857"

datos <- read_csv("UNGA/base_193_paises.csv", show_col_types = FALSE) |>
  select(iso3c, pais, idh)

# Umbrales oficiales del PNUD para las categorías de desarrollo humano
cortes  <- c(-Inf, 0.550, 0.700, 0.800, Inf)
etiquetas <- c("Bajo (< 0,550)", "Medio (0,550-0,699)",
               "Alto (0,700-0,799)", "Muy alto (≥ 0,800)")
sin_dato <- "Sin dato / no es Estado miembro"

datos <- datos |>
  mutate(idh_cat = cut(idh, breaks = cortes, labels = etiquetas, right = FALSE))


# 2. GEOMETRÍAS ---------------------------------------------------------------
# Natural Earth 1:50 millones. Usamos iso_a3_eh porque iso_a3 viene vacío
# ("-99") para Francia y Noruega.

mundo <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica") |>
  select(iso3c = iso_a3_eh, nombre_mapa = name, geometry)

mapa <- mundo |>
  left_join(datos, by = "iso3c") |>
  mutate(idh_cat = factor(as.character(idh_cat),
                          levels = c(etiquetas, sin_dato)),
         idh_cat = replace(idh_cat, is.na(idh_cat), sin_dato))

# Estados miembros demasiado pequeños para verse a esta escala (Mónaco, Tuvalu,
# Nauru, Maldivas, etc.) o sin polígono propio en Natural Earth.
sin_poligono <- setdiff(datos$iso3c, mundo$iso3c)
message("Estados miembros sin polígono en el mapa (", length(sin_poligono), "): ",
        paste(sin_poligono, collapse = ", "))



pal_cat <- c(viridis_pal(option = "mako", direction = -1, begin = .1, end = .95)(4),
             "grey85")
names(pal_cat) <- levels(mapa$idh_cat)

nota <- paste0("Fuente: PNUD, Informe de Desarrollo Humano 2025 (datos ", ANIO_IDH,
               "). Geometrías: Natural Earth.\n",
               "Gris: sin dato, Estado observador o territorio. ",
               "Los Estados insulares muy pequeños no son visibles a esta escala.")

mapa_categorias <- function(crs, titulo, subtitulo) {
  ggplot(mapa) +
    geom_sf(aes(fill = idh_cat), colour = "white", linewidth = .12) +
    scale_fill_manual(values = pal_cat, name = NULL, drop = FALSE) +
    coord_sf(crs = crs, expand = FALSE) +
    labs(title = titulo, subtitle = subtitulo, caption = nota) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
          plot.margin = margin(10, 10, 10, 10)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

mapa_continuo <- function(crs, titulo, subtitulo) {
  ggplot(mapa) +
    geom_sf(aes(fill = idh), colour = "white", linewidth = .12) +
    scale_fill_viridis_c(option = "mako", direction = -1, begin = .1, end = .95,
                         na.value = "grey85", name = "IDH",
                         breaks = seq(.4, 1, .1),
                         guide = guide_colourbar(barwidth = 18, barheight = .6,
                                                 title.position = "left")) +
    coord_sf(crs = crs, expand = FALSE) +
    labs(title = titulo, subtitle = subtitulo, caption = nota) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_text(hjust = 0, colour = "grey30", size = 8),
          plot.margin = margin(10, 10, 10, 10))
}

titulo <- paste0("Desarrollo humano en el mundo, ", ANIO_IDH)

ggsave("figuras/08_mapa_idh_categorias_equal_earth.png",
       mapa_categorias(PROJ_EQUAL_EARTH, titulo,
                       "Índice de Desarrollo Humano por categorías del PNUD · proyección Equal Earth (áreas proporcionales)"),
       width = 12, height = 7.2, dpi = 300, bg = "white")

ggsave("figuras/09_mapa_idh_continuo_equal_earth.png",
       mapa_continuo(PROJ_EQUAL_EARTH, titulo,
                     "Índice de Desarrollo Humano (0 a 1) · proyección Equal Earth"),
       width = 12, height = 7.2, dpi = 300, bg = "white")

# Para contrastar: los mismos datos en Mercator (Groenlandia y Rusia se inflan,

ggsave("figuras/10_comparacion_mercator.png",
       mapa_categorias(PROJ_MERCATOR, titulo,
                       "Mismo mapa en proyección Mercator: nótese cómo se distorsiona el tamaño relativo"),
       width = 12, height = 9, dpi = 300, bg = "white")



# =============================================================================
# CONTRASTE: LOS 5 PAÍSES MÁS RICOS vs. LOS 5 MÁS POBRES
#

N               <- 5             
MEDIDA_RIQUEZA  <- "pib_pc_ppa" 
ANIO_MIN_DATO   <- 2020        

etiqueta_riqueza <- c(
  pib_pc_ppa = "PIB per cápita, PPA (US$ internacionales de 2021)",
  pib_pc_usd = "PIB per cápita (US$ corrientes)"
)[[MEDIDA_RIQUEZA]]

col_ricos  <- "#2c7fb8"  
col_pobres <- "#d95f02"   




datos <- read_csv("UNGA/base_193_paises.csv", show_col_types = FALSE)
col_anio <- paste0("anio_", MEDIDA_RIQUEZA)

elegibles <- datos |>
  filter(!is.na(.data[[MEDIDA_RIQUEZA]]), .data[[col_anio]] >= ANIO_MIN_DATO)

ricos  <- elegibles |> slice_max(.data[[MEDIDA_RIQUEZA]], n = N, with_ties = FALSE) |>
  mutate(grupo = "Más ricos")
pobres <- elegibles |> slice_min(.data[[MEDIDA_RIQUEZA]], n = N, with_ties = FALSE) |>
  mutate(grupo = "Más pobres")

extremos <- bind_rows(ricos, pobres) |>
  mutate(
    riqueza = .data[[MEDIDA_RIQUEZA]],
    grupo   = factor(grupo, levels = c("Más ricos", "Más pobres")),
   
    pais    = factor(pais, levels = pais[order(riqueza)])
  )

razon_promedios <- mean(ricos[[MEDIDA_RIQUEZA]]) / mean(pobres[[MEDIDA_RIQUEZA]])
razon_extremos  <- max(extremos$riqueza) / min(extremos$riqueza)
print(select(arrange(extremos, desc(riqueza)), pais, grupo, riqueza,
             all_of(col_anio), idh, esperanza_vida, mortalidad_5))

write_csv(select(arrange(extremos, desc(riqueza)), iso3c, pais, grupo, riqueza,
                 all_of(col_anio), idh, esperanza_vida, mortalidad_5, fecundidad),
          "resultados/ricos_vs_pobres.csv")




g_pib <- ggplot(extremos, aes(riqueza, pais, fill = grupo)) +
  geom_col(width = .7) +
  geom_text(aes(label = dollar(riqueza, accuracy = 1)),
            hjust = -.12, size = 3.6, colour = "grey20") +
  scale_fill_manual(values = c("Más ricos" = col_ricos, "Más pobres" = col_pobres),
                    name = NULL) +
  scale_x_continuous(labels = dollar, expand = expansion(mult = c(0, .14))) +
  labs(
    title = paste0("Los ", N, " países más ricos frente a los ", N, " más pobres"),
    subtitle = paste0("Los más ricos tienen en promedio ", round(razon_promedios),
                      " veces el ingreso per cápita de los más pobres\n",
                      "(entre el país más rico y el más pobre: ", round(razon_extremos), " veces)"),
    x = etiqueta_riqueza, y = NULL,
    caption = paste0("Fuente: Banco Mundial (WDI), último dato disponible por país ",
                     "(se excluyen datos anteriores a ", ANIO_MIN_DATO, ").\n",
                     "Entre los países más ricos suelen aparecer economías muy pequeñas, ",
                     "petroleras o paraísos fiscales: el PIB per cápita no equivale al ",
                     "ingreso de una familia típica.")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_text(hjust = 0, colour = "grey35", size = 8.5),
        plot.title.position = "plot", plot.caption.position = "plot")

ggsave("figuras/11_barras_ricos_vs_pobres_pib.png", g_pib,
       width = 10, height = 6.2, dpi = 300, bg = "white")


# GRÁFICO COMPLEMENTARIO: ¿SE TRADUCE LA RIQUEZA EN BIENESTAR? No se incluye en análisis

nombres_ind <- c(
  idh            = "Índice de Desarrollo Humano (0-1)",
  esperanza_vida = "Esperanza de vida (años)",
  mortalidad_5   = "Mortalidad de menores de 5 años\n(por 1.000 nacidos vivos)",
  fecundidad     = "Hijos por mujer"
)

bienestar <- extremos |>
  select(pais, grupo, all_of(names(nombres_ind))) |>
  pivot_longer(all_of(names(nombres_ind)), names_to = "indicador", values_to = "valor") |>
  mutate(etiqueta  = ifelse(indicador == "idh", sprintf("%.3f", valor),
                            sprintf("%.1f", valor)),
         indicador = factor(indicador, levels = names(nombres_ind),
                            labels = nombres_ind))

g_bienestar <- ggplot(bienestar, aes(valor, pais, fill = grupo)) +
  geom_col(width = .7) +
  geom_text(aes(label = etiqueta), hjust = -.12, size = 3) +
  facet_wrap(~ indicador, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = c("Más ricos" = col_ricos, "Más pobres" = col_pobres),
                    name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, .2))) +
  labs(title = "Riqueza y bienestar en los dos extremos",
       subtitle = "Mismos 10 países, ordenados de más a menos rico",
       x = NULL, y = NULL,
       caption = "Fuentes: Banco Mundial (WDI) y PNUD (Informe de Desarrollo Humano 2025).") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title = element_text(face = "bold", size = 15),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey35", size = 8.5))

ggsave("figuras/12_barras_ricos_vs_pobres_bienestar.png", g_bienestar,
       width = 11, height = 7.5, dpi = 300, bg = "white")
