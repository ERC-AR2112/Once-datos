
#  ¿Quién calentó el planeta y quién paga las consecuencias?



paquetes <- c("tidyverse", "countrycode", "scales", "ggrepel")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

dir.create("resultados", showWarnings = FALSE)
dir.create("datos", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))

# Devuelve el valor más reciente no faltante de una variable
ultimo_valor <- function(x, anio) {
  ok <- which(!is.na(x))
  if (length(ok) == 0) return(NA_real_)
  x[ok[which.max(anio[ok])]]
}

# Datos de emisiones (OWID) 
url_owid <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
owid <- read_csv(url_owid, show_col_types = FALSE)

paises <- owid %>%
  filter(!is.na(iso_code), nchar(iso_code) == 3, !str_starts(iso_code, "OWID"))

anio_datos <- max(paises$year[!is.na(paises$share_of_temperature_change_from_ghg)])

responsabilidad <- paises %>%
  arrange(year) %>%
  group_by(iso_code, country) %>%
  summarise(
    co2_acum_fosil_mt  = ultimo_valor(cumulative_co2, year),              # Mt CO2 (fósiles + cemento)
    co2_acum_total_mt  = ultimo_valor(cumulative_co2_including_luc, year),# Mt CO2 incl. uso de suelo
    pct_calentamiento  = ultimo_valor(share_of_temperature_change_from_ghg, year), # % del calentamiento global
    aporte_temp_c      = ultimo_valor(temperature_change_from_ghg, year), # °C atribuibles al país
    co2_pc_actual_t    = ultimo_valor(co2_per_capita, year),              # t CO2 por persona (año reciente)
    poblacion          = ultimo_valor(population, year),
    .groups = "drop"
  ) %>%
  mutate(
    co2_acum_pc_t = co2_acum_total_mt * 1e6 / poblacion,  # t CO2 acumuladas por habitante actual
    continente    = countrycode(iso_code, "iso3c", "continent", warn = FALSE),
    region        = countrycode(iso_code, "iso3c", "region", warn = FALSE),
    pais_es       = countrycode(iso_code, "iso3c", "cldr.short.es", warn = FALSE),
    pais_es       = coalesce(pais_es, country)
  )

# Principales responsables 
top20_total <- responsabilidad %>%
  filter(!is.na(pct_calentamiento)) %>%
  slice_max(pct_calentamiento, n = 20)

g1 <- ggplot(top20_total,
             aes(x = reorder(pais_es, pct_calentamiento), y = pct_calentamiento, fill = continente)) +
  geom_col() +
  geom_text(aes(label = percent(pct_calentamiento / 100, accuracy = 0.1)),
            hjust = -0.1, size = 3.3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Los 20 países que más han contribuido al calentamiento global",
       subtitle = paste0("Participación en el aumento de temperatura por CO2, CH4 y N2O (1850–", anio_datos, ")"),
       x = NULL, y = "% del calentamiento global", fill = "Continente",
       caption = "Fuente: Our World in Data / Jones et al. (2023)")
ggsave("resultados/01_top20_calentamiento.png", g1, width = 10, height = 7, dpi = 300, bg = "white")

# Responsabilidad por habitante (países con más de 1 millón de habitantes)
top20_pc <- responsabilidad %>%
  filter(poblacion > 1e6, !is.na(co2_acum_pc_t)) %>%
  slice_max(co2_acum_pc_t, n = 20)

g2 <- ggplot(top20_pc,
             aes(x = reorder(pais_es, co2_acum_pc_t), y = co2_acum_pc_t, fill = continente)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Emisiones históricas de CO2 por habitante",
       subtitle = "Toneladas acumuladas (incluye uso de suelo) / población actual; países > 1 millón hab.",
       x = NULL, y = "t CO2 por persona", fill = "Continente",
       caption = "Fuente: Our World in Data")
ggsave("resultados/02_top20_per_capita.png", g2, width = 10, height = 7, dpi = 300, bg = "white")

# Participación por continente
por_continente <- responsabilidad %>%
  filter(!is.na(continente)) %>%
  group_by(continente) %>%
  summarise(pct_calentamiento = sum(pct_calentamiento, na.rm = TRUE),
            pct_poblacion     = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_poblacion = 100 * pct_poblacion / sum(pct_poblacion))
print(por_continente)

g3 <- por_continente %>%
  pivot_longer(-continente, names_to = "indicador", values_to = "pct") %>%
  mutate(indicador = recode(indicador,
                            pct_calentamiento = "% del calentamiento causado",
                            pct_poblacion     = "% de la población mundial")) %>%
  ggplot(aes(x = continente, y = pct, fill = indicador)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#d7301f", "#2b8cbe")) +
  labs(title = "Responsabilidad vs. población por continente",
       x = NULL, y = "%", fill = NULL) +
  theme(legend.position = "bottom")
ggsave("resultados/03_continentes.png", g3, width = 9, height = 6, dpi = 300, bg = "white")

# ---- 4. Concentración de la responsabilidad (curva de Lorenz) -------
lorenz <- responsabilidad %>%
  filter(!is.na(co2_acum_total_mt), !is.na(poblacion), co2_acum_total_mt > 0) %>%
  arrange(co2_acum_pc_t) %>%
  mutate(pob_acum = cumsum(poblacion) / sum(poblacion),
         emis_acum = cumsum(co2_acum_total_mt) / sum(co2_acum_total_mt))

x <- c(0, lorenz$pob_acum); y <- c(0, lorenz$emis_acum)
gini <- 1 - sum((x[-1] - x[-length(x)]) * (y[-1] + y[-length(y)]))
cat(sprintf("Índice de Gini de las emisiones históricas entre países: %.2f\n", gini))

g4 <- ggplot(lorenz, aes(pob_acum, emis_acum)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1.2, colour = "#d7301f") +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Desigualdad en las emisiones históricas de CO2",
       subtitle = sprintf("Curva de Lorenz entre países (Gini = %.2f)", gini),
       x = "Población mundial acumulada (de menor a mayor emisor per cápita)",
       y = "Emisiones históricas acumuladas")
ggsave("resultados/04_lorenz.png", g4, width = 8, height = 7, dpi = 300, bg = "white")

# Vulnerabilidad (ND-GAIN)
leer_ndgain <- function(ruta, nombre_var) {
  read_csv(ruta, show_col_types = FALSE) %>%
    pivot_longer(-c(ISO3, Name), names_to = "anio", values_to = "valor") %>%
    mutate(anio = suppressWarnings(as.integer(anio))) %>%
    filter(!is.na(valor), !is.na(anio)) %>%
    group_by(ISO3) %>%
    slice_max(anio, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(iso_code = ISO3, !!nombre_var := valor)
}

ruta_vul <- "datos/vulnerability.csv"
ruta_rea <- "datos/readiness.csv"

if (!file.exists(ruta_vul)) {
  stop("No se encontró ", ruta_vul,
       ". Descarga ND-GAIN (ver encabezado) y copia vulnerability.csv a ./datos/")
}

vulnerabilidad <- leer_ndgain(ruta_vul, "vulnerabilidad")
if (file.exists(ruta_rea)) {
  vulnerabilidad <- vulnerabilidad %>% left_join(leer_ndgain(ruta_rea, "preparacion"), by = "iso_code")
}

datos <- responsabilidad %>%
  inner_join(vulnerabilidad, by = "iso_code") %>%
  filter(!is.na(pct_calentamiento), pct_calentamiento > 0, !is.na(poblacion))

# Los 20 países más vulnerables y cuánto aportaron
top20_vul <- datos %>%
  slice_max(vulnerabilidad, n = 20) %>%
  select(pais_es, continente, vulnerabilidad, pct_calentamiento, co2_acum_pc_t, poblacion)
print(top20_vul, n = 20)
cat(sprintf("Los 20 países más vulnerables causaron en conjunto el %.2f%% del calentamiento\n",
            sum(top20_vul$pct_calentamiento)))

g5 <- ggplot(top20_vul, aes(x = reorder(pais_es, vulnerabilidad), y = vulnerabilidad, fill = continente)) +
  geom_col() + coord_flip() +
  labs(title = "Los 20 países más vulnerables al cambio climático",
       subtitle = "Índice de vulnerabilidad ND-GAIN (0 a 1)", x = NULL,
       y = "Vulnerabilidad", fill = "Continente", caption = "Fuente: ND-GAIN")
ggsave("resultados/05_top20_vulnerables.png", g5, width = 10, height = 7, dpi = 300, bg = "white")

# ---- 6. Responsabilidad vs. vulnerabilidad ---------------------------
med_vul  <- median(datos$vulnerabilidad)
med_resp <- median(datos$co2_acum_pc_t, na.rm = TRUE)

datos <- datos %>%
  mutate(cuadrante = case_when(
    co2_acum_pc_t >= med_resp & vulnerabilidad <  med_vul ~ "Alta responsabilidad, baja vulnerabilidad",
    co2_acum_pc_t <  med_resp & vulnerabilidad >= med_vul ~ "Baja responsabilidad, alta vulnerabilidad",
    co2_acum_pc_t >= med_resp & vulnerabilidad >= med_vul ~ "Alta responsabilidad, alta vulnerabilidad",
    TRUE                                                   ~ "Baja responsabilidad, baja vulnerabilidad"))

print(count(datos, cuadrante))

# Etiquetas: principales emisores, más vulnerables y México
etiquetar <- datos %>%
  filter(iso_code %in% c(top20_total$iso_code[1:10]) |
           vulnerabilidad >= sort(vulnerabilidad, decreasing = TRUE)[8] |
           iso_code == "MEX")

g6 <- ggplot(datos, aes(x = co2_acum_pc_t, y = vulnerabilidad)) +
  geom_hline(yintercept = med_vul, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = med_resp, linetype = "dashed", colour = "grey60") +
  geom_point(aes(size = poblacion, colour = cuadrante), alpha = 0.7) +
  geom_text_repel(data = etiquetar, aes(label = pais_es), size = 3, max.overlaps = 30) +
  scale_x_log10(labels = label_comma()) +
  scale_size_area(max_size = 14, labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_colour_manual(values = c("#fdae61", "#2c7bb6", "#d7191c", "grey60")) +
  labs(title = "Injusticia climática: quién emite y quién es vulnerable",
       subtitle = "Emisiones históricas por habitante vs. vulnerabilidad ND-GAIN (líneas = medianas)",
       x = "t CO2 acumuladas por habitante (escala log)", y = "Vulnerabilidad (ND-GAIN)",
       size = "Población", colour = NULL,
       caption = "Fuentes: Our World in Data; ND-GAIN") +
  guides(colour = guide_legend(ncol = 2), size = "none") +
  theme(legend.position = "bottom")
ggsave("resultados/06_responsabilidad_vs_vulnerabilidad.png", g6, width = 12, height = 9, dpi = 300, bg = "white")

# Correlación 
print(cor.test(datos$co2_acum_pc_t, datos$vulnerabilidad, method = "spearman", exact = FALSE))

# Quintiles de vulnerabilidad:
quintiles <- datos %>%
  mutate(quintil = ntile(vulnerabilidad, 5)) %>%
  group_by(quintil) %>%
  summarise(n_paises = n(),
            pct_poblacion = sum(poblacion),
            pct_calentamiento = sum(pct_calentamiento), .groups = "drop") %>%
  mutate(pct_poblacion = 100 * pct_poblacion / sum(pct_poblacion),
         pct_calentamiento = 100 * pct_calentamiento / sum(pct_calentamiento),
         quintil = factor(quintil, labels = c("Q1\n(menos vulnerable)", "Q2", "Q3", "Q4",
                                              "Q5\n(más vulnerable)")))
print(quintiles)

g7 <- quintiles %>%
  pivot_longer(c(pct_poblacion, pct_calentamiento), names_to = "indicador", values_to = "pct") %>%
  mutate(indicador = recode(indicador,
                            pct_calentamiento = "% del calentamiento causado",
                            pct_poblacion     = "% de la población")) %>%
  ggplot(aes(quintil, pct, fill = indicador)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(pct, 1)), position = position_dodge(0.9), vjust = -0.3, size = 3.3) +
  scale_fill_manual(values = c("#d7301f", "#2b8cbe")) +
  labs(title = "Los más vulnerables son los que menos han contribuido",
       subtitle = "Países agrupados por quintil de vulnerabilidad ND-GAIN",
       x = NULL, y = "%", fill = NULL,
       caption = "Fuentes: Our World in Data; ND-GAIN") +
  theme(legend.position = "bottom")
ggsave("resultados/07_quintiles_vulnerabilidad.png", g7, width = 9, height = 6, dpi = 300, bg = "white")

write_csv(responsabilidad, "resultados/tabla_responsabilidad.csv")
write_csv(datos,           "resultados/tabla_responsabilidad_vulnerabilidad.csv")
write_csv(quintiles,       "resultados/tabla_quintiles.csv")

