

paquetes <- c("dplyr", "readr", "ggplot2", "stringr", "purrr", "tibble")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))
dir.create("figuras", showWarnings = FALSE)


N_FILAS        <- 8     
RADIO_INTERNO  <- 0.35   
# Miembros permanentes del Consejo de Seguridad 
permanentes <- c(CHN = "China", USA = "EE. UU.", FRA = "Francia",
                 GBR = "Reino Unido", RUS = "Rusia")

# Regiones del Banco Mundial en español
regiones_es <- c(
  "North America"                    = "Norteamérica",
  "Latin America & Caribbean"        = "América Latina y el Caribe",
  "Europe & Central Asia"            = "Europa y Asia Central",
  "Middle East & North Africa"       = "Medio Oriente y Norte de África",
  "Middle East, North Africa, Afghanistan & Pakistan" =
    "Medio Oriente, N. África, Afganistán y Pakistán",
  "Sub-Saharan Africa"               = "África Subsahariana",
  "South Asia"                       = "Asia Meridional",
  "East Asia & Pacific"              = "Asia Oriental y Pacífico"
)

# Paleta Okabe-Ito: distinguible por personas con daltonismo
paleta <- c("#56B4E9", "#009E73", "#0072B2", "#D55E00",
            "#F0E442", "#CC79A7", "#E69F00", "#999999")


datos <- read_csv("UNGA/base_193_paises.csv", show_col_types = FALSE) |>
  select(iso3c, pais, region, poblacion) |>
  mutate(
    region_es = coalesce(unname(regiones_es[region]), region, "Sin región asignada"),
    permanente = iso3c %in% names(permanentes)
  )
stopifnot(nrow(datos) == 193, all(names(permanentes) %in% datos$iso3c))


orden_regiones <- c(intersect(unname(regiones_es), unique(datos$region_es)),
                    setdiff(unique(datos$region_es), unname(regiones_es)))
datos <- datos |>
  mutate(region_es = factor(region_es, levels = orden_regiones)) |>
  arrange(region_es, desc(poblacion))   # dentro de cada región: de más a menos poblado

conteo <- count(datos, region_es)
message("Países por región:"); print(conteo)



n <- nrow(datos)
radios <- seq(RADIO_INTERNO, 1, length.out = N_FILAS)

por_fila <- radios / sum(radios) * n
asientos_fila <- floor(por_fila)
faltantes <- n - sum(asientos_fila)
asientos_fila[order(por_fila - asientos_fila, decreasing = TRUE)[seq_len(faltantes)]] <-
  asientos_fila[order(por_fila - asientos_fila, decreasing = TRUE)[seq_len(faltantes)]] + 1
stopifnot(sum(asientos_fila) == n)

asientos <- map2_dfr(seq_len(N_FILAS), asientos_fila, function(i, k) {
  tibble(fila = i, radio = radios[i], angulo = seq(pi, 0, length.out = k))
}) |>
  arrange(desc(angulo), desc(radio)) |>    
  mutate(x = radio * cos(angulo), y = radio * sin(angulo),
         asiento = row_number())


mapa <- bind_cols(asientos, datos)
for (codigo in names(permanentes)) {
  i_pais <- which(mapa$iso3c == codigo)
  if (mapa$fila[i_pais] == N_FILAS) next          # ya está en el arco exterior
  reg    <- mapa$region_es[i_pais]
  candidatos <- which(mapa$region_es == reg & mapa$fila == N_FILAS &
                        !mapa$iso3c %in% names(permanentes))
  if (!length(candidatos)) next
  # el asiento exterior de la región más cercano en ángulo al que le tocó
  destino <- candidatos[which.min(abs(mapa$angulo[candidatos] - mapa$angulo[i_pais]))]
  campos <- c("iso3c", "pais", "region", "poblacion", "region_es", "permanente")
  tmp <- mapa[destino, campos]
  mapa[destino, campos] <- mapa[i_pais, campos]
  mapa[i_pais, campos]  <- tmp
}

mapa <- mapa |>
  mutate(tipo = ifelse(permanente, "Miembro permanente del Consejo de Seguridad",
                       "Otro Estado miembro"))


etiquetas <- mapa |>
  filter(permanente) |>
  mutate(nombre = unname(permanentes[iso3c]),
         grados = angulo * 180 / pi,
         angulo_texto = ifelse(grados > 90, grados + 180, grados),
         hjust = ifelse(grados > 90, 1, 0),
         xl = 1.09 * cos(angulo), yl = 1.09 * sin(angulo))




nombres_leyenda <- paste0(conteo$region_es, " (", conteo$n, ")")
names(nombres_leyenda) <- conteo$region_es

g <- ggplot(mapa, aes(x, y)) +
  geom_point(aes(fill = region_es, shape = tipo, size = tipo, colour = tipo,
                 stroke = ifelse(permanente, 1.3, .4))) +
  geom_text(data = etiquetas,
            aes(xl, yl, label = nombre, angle = angulo_texto, hjust = hjust),
            size = 3.6, fontface = "bold", colour = "grey15") +
  annotate("text", x = 0, y = .16, label = "193", size = 16, fontface = "bold",
           colour = "grey15") +
  annotate("text", x = 0, y = .03, label = "Estados miembros", size = 4.2,
           colour = "grey30") +
  annotate("text", x = 0, y = -.05, label = "de la ONU", size = 4.2,
           colour = "grey30") +
  scale_fill_manual(values = setNames(paleta[seq_along(orden_regiones)], orden_regiones),
                    labels = nombres_leyenda, name = "Región (Banco Mundial)") +
  scale_shape_manual(values = c("Otro Estado miembro" = 21,
                                "Miembro permanente del Consejo de Seguridad" = 23),
                     name = NULL) +
  scale_size_manual(values = c("Otro Estado miembro" = 4.6,
                               "Miembro permanente del Consejo de Seguridad" = 6.4),
                    guide = "none") +
  scale_colour_manual(values = c("Otro Estado miembro" = "white",
                                 "Miembro permanente del Consejo de Seguridad" = "black"),
                      guide = "none") +
  scale_x_continuous(expand = expansion(mult = .05)) +
  guides(fill  = guide_legend(order = 1, ncol = 2, title.position = "top",
                              override.aes = list(shape = 21, size = 5, colour = "white")),
         shape = guide_legend(order = 2,
                              override.aes = list(fill = "grey75",
                                                  colour = c("white", "black"),
                                                  size = c(4.6, 6.4)))) +
  coord_fixed(clip = "off", xlim = c(-1.3, 1.3), ylim = c(-.08, 1.25)) +
  labs(title = "Los 193 Estados miembros de la Organización de las Naciones Unidas",
       subtitle = "Cada punto es un país, agrupado por región. Los rombos son los cinco miembros permanentes del Consejo de Seguridad.",
       caption = "Regiones: clasificación del Banco Mundial. Dentro de cada región, los países van de más a menos poblados (en el arco exterior se colocan los miembros permanentes).") +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom", legend.box = "horizontal",
        legend.box.just = "top",
        plot.title = element_text(face = "bold", size = 17, hjust = .5),
        plot.subtitle = element_text(size = 10.5, hjust = .5, colour = "grey30",
                                     margin = margin(b = 4)),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = .5,
                                    margin = margin(t = 8)),
        plot.margin = margin(12, 12, 12, 12))

ggsave("figuras/13_herradura_193_paises.png", g, width = 12, height = 7.5,
       dpi = 300, bg = "white")

message("Listo: figuras/13_herradura_193_paises.png")


# ALINEACIÓN DE VOTOS: ¿QUIÉN VOTA CON QUIÉN?
# Cuatro resoluciones controvertidas de la Asamblea General de la ONU


paquetes <- c("dplyr", "tidyr", "readr", "ggplot2", "stringr", "tibble", "purrr")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan)) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))
dir.create("figuras", showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)

permanentes <- c("CHN", "FRA", "GBR", "RUS", "USA")

colores_voto <- c("Sí" = "#1a9850", "Abstención" = "#bdbdbd",
                  "No" = "#d73027", "Sin dato" = "white")



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

resoluciones <- list(
  ucrania = list(
    simbolo = "A/ES-11/2",
    titulo  = "Consecuencias humanitarias\nde la agresión contra Ucrania",
    fecha   = "24-mar-2022",
    no   = c("BLR","ERI","PRK","RUS","SYR"),
    abst = c("DZA","AGO","ARM","BOL","BWA","BRN","BDI","CAF","CHN","CUB","SLV",
             "GNQ","SWZ","ETH","GNB","IND","IRN","KAZ","KGZ","LAO","MDG","MLI",
             "MNG","MOZ","NAM","NIC","PAK","COG","ZAF","LKA","SDN","TJK","TGO",
             "TZA","UGA","UZB","VNM","ZWE")
  ),
  asentamientos = list(
    simbolo = "A/RES/79/91",
    titulo  = "Asentamientos israelíes en el\nTerritorio Palestino Ocupado",
    fecha   = "4-dic-2024",
    no   = c("ARG","FJI","HUN","ISR","FSM","NRU","PNG","TON","USA"),
    abst = c("CMR","CAF","CIV","ECU","GEO","GTM","HTI","KIR","LBR","MDG","MWI",
             "PLW","PAN","PRY","RWA","TGO","TUV","URY")
  ),
  cuba = list(
    simbolo = "A/RES/79/7",
    titulo  = "Fin del embargo de EE. UU.\ncontra Cuba",
    fecha   = "29-oct-2024",
    no   = c("ISR","USA"),
    abst = c("MDA")
  ),
  golan = list(
    simbolo = "A/RES/79/90",
    titulo  = "El Golán sirio ocupado",
    fecha   = "4-dic-2024",
    no   = c("ISR","PNG","TON","USA"),
    abst = c("ARG","AUS","CMR","CAN","CAF","CIV","ECU","FJI","GEO","GTM","HTI",
             "KIR","LBR","MDG","MWI","FSM","NRU","PLW","PAN","PRY","RWA","TGO",
             "TUV","URY","VUT")
  )
)

votos_largo <- imap_dfr(resoluciones, function(r, clave) {
  tibble(
    iso3c      = miembros_onu,
    resolucion = clave,
    titulo     = r$titulo,
    voto = case_when(
      miembros_onu %in% r$no   ~ "No",
      miembros_onu %in% r$abst ~ "Abstención",
      TRUE                     ~ "Sí"
    )
  )
})

# Comprobación rápida contra los totales oficiales (sí/no/abstención)
message("Comprobación de conteos (sí incluye posibles ausentes no listados):")
print(votos_largo |> count(titulo, voto) |> pivot_wider(names_from = voto, values_from = n))

paises <- read_csv("UNGA/base_193_paises.csv", show_col_types = FALSE) |>
  select(iso3c, pais, region)

votos <- votos_largo |>
  left_join(paises, by = "iso3c") |>
  mutate(pais = coalesce(pais, iso3c),
         permanente = iso3c %in% permanentes,
         etiqueta_pais = ifelse(permanente, paste0("★ ", pais), pais))

write_csv(
  votos |> select(iso3c, pais, resolucion, titulo, voto) |>
    pivot_wider(names_from = titulo, values_from = voto),
  "resultados/votos_4_resoluciones.csv"
)




ancho <- votos |>
  select(iso3c, etiqueta_pais, resolucion, voto) |>
  pivot_wider(names_from = resolucion, values_from = voto)

disidentes <- ancho |>
  filter(if_any(c(ucrania, asentamientos, cuba, golan), ~ . != "Sí"))

message(nrow(disidentes), " de 193 países no votaron 'Sí' en alguna de las cuatro; ",
        193 - nrow(disidentes), " votaron 'Sí' en las cuatro (no se muestran).")

cod_voto <- c("Sí" = 1, "Abstención" = 0, "No" = -1)
matriz_num <- disidentes |>
  select(ucrania, asentamientos, cuba, golan) |>
  mutate(across(everything(), ~ cod_voto[.x])) |>
  as.matrix()
rownames(matriz_num) <- disidentes$etiqueta_pais

orden_cluster <- hclust(dist(matriz_num), method = "complete")$order
orden_paises  <- rownames(matriz_num)[orden_cluster]

titulos_cortos <- c(ucrania = "Ucrania\n(A/ES-11/2)",
                    asentamientos = "Asentamientos en\nTerr. Palestino (79/91)",
                    cuba = "Embargo a Cuba\n(A/RES/79/7)",
                    golan = "Golán sirio\n(A/RES/79/90)")
orden_resoluciones <- names(resoluciones)

datos_matriz <- disidentes |>
  pivot_longer(c(ucrania, asentamientos, cuba, golan),
               names_to = "resolucion", values_to = "voto") |>
  mutate(etiqueta_pais = factor(etiqueta_pais, levels = orden_paises),
         titulo = factor(titulos_cortos[resolucion], levels = titulos_cortos[orden_resoluciones]))

alto_fig <- max(6, 0.185 * nrow(disidentes) + 1.7)

g_matriz <- ggplot(datos_matriz, aes(titulo, etiqueta_pais, fill = voto)) +
  geom_tile(colour = "white", linewidth = .5) +
  scale_fill_manual(values = colores_voto, name = NULL) +
  scale_x_discrete(position = "top") +
  labs(
    title = "¿Quién se aparta del consenso?",
    subtitle = str_wrap(paste0(
      "Países que no votaron \"Sí\" en alguna de las cuatro resoluciones. De 193 Estados miembros, ",
      nrow(disidentes), " se apartaron al menos una vez; los ", 193 - nrow(disidentes),
      " restantes votaron \"Sí\" en las cuatro y no se muestran. ",
      "★ = miembro permanente del Consejo de Seguridad."), width = 95),
    x = NULL, y = NULL,
    caption = paste0("Fuentes: UN Watch Database / Biblioteca Digital de la ONU. Resoluciones: ",
                     paste(map_chr(resoluciones, "simbolo"), collapse = ", "), ". ",
                     "Países ordenados por parecido de voto (agrupamiento jerárquico). ",
                     "Los ausentes no identificados por país en las fuentes se contabilizan como \"Sí\" (ver notas del script).")
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(face = "bold", size = 9, lineheight = .9),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 9.5, colour = "grey30"),
        plot.caption = element_text(hjust = 0, size = 7.5, colour = "grey40"),
        plot.title.position = "plot", plot.caption.position = "plot")

ggsave("figuras/14_matriz_votos_disidentes.png", g_matriz,
       width = 8, height = alto_fig, dpi = 300, bg = "white", limitsize = FALSE)




protagonistas <- c("USA","GBR","FRA","RUS","CHN",             # P5
                   "UKR","ISR",                                # partes directas
                   "CUB","VEN","NIC","PRK","BLR","IRN","SYR",  # bloque afín a Rusia/Cuba
                   "BRA","IND","ZAF","IDN","TUR","MEX",        # potencias emergentes
                   "DEU","JPN")                                 # aliados occidentales

matriz_prot <- ancho |>
  filter(iso3c %in% protagonistas) |>
  select(iso3c, ucrania, asentamientos, cuba, golan) |>
  mutate(across(c(ucrania, asentamientos, cuba, golan), ~ cod_voto[.x]))

m <- as.matrix(matriz_prot[ , -1]); rownames(m) <- matriz_prot$iso3c

acuerdo <- outer(seq_len(nrow(m)), seq_len(nrow(m)),
                 Vectorize(function(i, j) mean(m[i, ] == m[j, ]) * 100))
dimnames(acuerdo) <- list(rownames(m), rownames(m))

orden_prot <- rownames(m)[hclust(as.dist(100 - acuerdo), method = "complete")$order]

datos_acuerdo <- as_tibble(acuerdo, rownames = "pais1") |>
  pivot_longer(-pais1, names_to = "pais2", values_to = "acuerdo") |>
  mutate(pais1 = factor(pais1, levels = orden_prot),
         pais2 = factor(pais2, levels = orden_prot))

g_acuerdo <- ggplot(datos_acuerdo, aes(pais2, pais1, fill = acuerdo)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(acuerdo)), size = 2.7,
            colour = ifelse(datos_acuerdo$acuerdo > 50, "white", "grey20")) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, limits = c(0, 100),
                       name = "% de coincidencia\nen las 4 votaciones") +
  coord_fixed() +
  labs(
    title = "Quién vota con quién: % de coincidencia entre países protagonistas",
    subtitle = "Sobre las mismas 4 resoluciones. 100% = votaron siempre igual (Sí/No/Abstención); 0% = siempre distinto.",
    x = NULL, y = NULL,
    caption = "Con solo 4 votaciones, este porcentaje solo puede tomar los valores 0, 25, 50, 75 o 100%: es una guía de bloques, no una medida fina.
    Fuente: UN Watch Database"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 13.5),
        plot.subtitle = element_text(size = 9, colour = "grey30"),
        plot.caption = element_text(hjust = 0, size = 7.5, colour = "grey40"),
        plot.title.position = "plot", plot.caption.position = "plot")

ggsave("figuras/15_acuerdo_entre_protagonistas.png", g_acuerdo,
       width = 9.5, height = 8.5, dpi = 300, bg = "white")




