## ENCIG
library(pacman)
p_load(readr, tidyverse, srvyr, survey, flextable, scales, sf)


#Carga de bases de datos
encig1 <- read_csv("Encig/encig2025_01_sec1_A_3_4_5_8_9_10.csv")
encig2 <- read_csv("Encig/encig2025_02_residentes_sec_2.csv")
encig3 <- read_csv("Encig/encig2025_03_sec_6.csv")
encig4 <- read_csv("Encig/encig2025_04_sec_7.csv")
encig5 <- read_csv("Encig/encig2025_05_sec_8.csv")
encig_11 <- read_csv("Encig/encig2025_01_sec_11.csv")

encig_11 %>% glimpse()
encig2 %>% glimpse()



##Análisis de la confianza en instituciones 
# Pegado de bases
encig_conf <- left_join(encig_11, encig2, by = c("ID_PER", "ID_VIV", "CVE_ENT", "UPM", "V_SEL"))





## Preparando base de datos a forma adecuada para compara la confianza en distintas instituciones
confianza_larga <- encig_conf %>%
  select(UPM, EST_DIS, FAC_P18, CVE_ENT, starts_with("P11_1_", ignore.case = TRUE)) %>% 
  pivot_longer(
    cols = starts_with("P11_1_", ignore.case = TRUE),
    names_to = "Institucion_cod",
    values_to = "Respuesta"
  ) %>%
  mutate(Institucion_cod = toupper(Institucion_cod)) %>% 
  filter(!is.na(Respuesta), Respuesta != "") %>% 
  mutate(
    Categoria = case_when(
      Respuesta == 1 ~ "1. Mucha confianza",
      Respuesta == 2 ~ "2. Algo de confianza",
      Respuesta == 3 ~ "3. Poco de confianza",
      Respuesta == 4 ~ "4. Nada de confianza",
      Respuesta == 9 ~ "9. Ns/Nc",
      TRUE ~ NA_character_
    ),
    Institucion = case_when(
      Institucion_cod == "P11_1_01"  ~ "Universidades Públicas",
      Institucion_cod == "P11_1_02"  ~ "Policías",
      Institucion_cod == "P11_1_03"  ~ "Hospitales públicos",
      Institucion_cod == "P11_1_04"  ~ "Presidencia de la República",
      Institucion_cod == "P11_1_05"  ~ "Empresarios",
      Institucion_cod == "P11_1_06"  ~ "Gobernaturas de los estados",
      Institucion_cod == "P11_1_07"  ~ "Compañeros (as) de trabajo",
      Institucion_cod == "P11_1_08"  ~ "Presidentes municipales",
      Institucion_cod == "P11_1_09"  ~ "Familiares (primos, tios, etc.",
      Institucion_cod == "P11_1_10" ~ "Sindicatos",
      Institucion_cod == "P11_1_11" ~ "Vecinos (as)",
      Institucion_cod == "P11_1_12" ~ "Cámaras de Diputados y Senadores",
      Institucion_cod == "P11_1_13" ~ "Medios de comunicación",
      Institucion_cod == "P11_1_14" ~ "Institutos electorales",
      Institucion_cod == "P11_1_15" ~ "Comisiones de Derechos Humanos",
      Institucion_cod == "P11_1_16" ~ "Escuelas Públicas a nivel básico",
      Institucion_cod == "P11_1_17" ~ "Jueces (ezas) y Magistrados (as)",
      Institucion_cod == "P11_1_18" ~ "Instituciones religiosas",
      Institucion_cod == "P11_1_19" ~ "Partidos Políticos",
      Institucion_cod == "P11_1_20" ~ "Guardia Nacional",
      Institucion_cod == "P11_1_21" ~ "Ejercito y marina",
      Institucion_cod == "P11_1_22" ~ "Ministerio público",
      Institucion_cod == "P11_1_23" ~ "Servidores públicos",
      Institucion_cod == "P11_1_24" ~ "Comisiones de Derechos Humanos",
      Institucion_cod == "P11_1_25" ~ "Organismos Públicos Autónomos")
  ) %>%
  filter(!is.na(Categoria))


## Estableciendo el diseño muestral
encig_design <- confianza_larga %>%
  as_survey_design(
    ids = UPM,
    strata = EST_DIS,
    weights = FAC_P18,
    nest = TRUE
  )

##Sacando la proporción de la población con confianza por cada institución 
confianza_res <- encig_design %>%
  group_by(Institucion, Categoria) %>%
  summarise(
    proporcion = survey_mean(na.rm = TRUE)
  ) %>%
  ungroup()

##Preparacxión de las gráficas
orden_instituciones <- confianza_res %>%
  filter(Categoria %in% c("1. Mucha confianza", "2. Algo de confianza")) %>%
  group_by(Institucion) %>%
  summarise(confianza_positiva = sum(proporcion)) %>%
  arrange(confianza_positiva) %>%
  pull(Institucion)

confianza_res <- confianza_res %>%
  mutate(Institucion = factor(Institucion, levels = orden_instituciones))

confianza_res %>% 
ggplot(aes(x = proporcion, y = Institucion, fill = Categoria)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(
    values = c(
      "1. Mucha confianza" = "#1a9641",
      "2. Algo de confianza" = "#a6d96a",
      "3. Poco de confianza" = "#fdae61",
      "4. Nada de confianza" = "#d7191c",
      "9. Ns/Nc" = "#d9d9d9"
    )
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Nivel de confianza en instituciones y actores sociales",
    subtitle = "Proporción de la población urbana de 18 años y más",
    x = "Porcentaje poblacional",
    y = NULL,
    fill = NULL,
    caption = "Fuente: Elaboración propia con datos de ENCIG 2025 (INEGI)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank(),
    plot.title.position = "plot"
  )

### Mapa, confianza en los gobernadorxs

confianza_estatal <- encig_design %>%
  filter(Institucion_cod == "P11_1_06") %>%
  mutate(
    Confianza_Positiva = if_else(
      Categoria %in% c("1. Mucha confianza", "2. Algo de confianza"),
      1, 0
    )
  ) %>%
  group_by(CVE_ENT) %>%
  summarise(
    prop_confianza = survey_mean(Confianza_Positiva, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(CVE_ENT = str_pad(CVE_ENT, width = 2, side = "left", pad = "0"))


##Preparando el marco geoestadistico
mapa_mexico <- st_read("D:/Canal11/2025_1_00_ENT.shp")

mapa_confianza <- mapa_mexico %>%
  left_join(confianza_estatal, by = "CVE_ENT")


ggplot(mapa_confianza) +
  # aes(fill) mapea nuestra variable calculada al color de relleno del estado
  geom_sf(aes(fill = prop_confianza), color = "white", size = 0.3) +
  
  # Escala de colores continua (viridis es amigable para daltonismo y se imprime bien)
  scale_fill_viridis_c(
    option = "mako", # Puedes probar "viridis", "magma" o "plasma"
    direction = -1,  # Invertir para que colores oscuros = mayor confianza
    labels = percent_format(accuracy = 1),
    name = "Confianza\n(Mucha + Algo)"
  ) +
  
  labs(
    title = "Confianza en el Gobierno Estatal por Entidad Federativa",
    subtitle = "Proporción de población de 18 años y más con mucha o algo de confianza",
    caption = "Fuente: Elaboración propia con datos de ENCIG 2025 (INEGI)."
  ) +
  
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 1, color = "grey40")
  )

## Promedio de niveles de gobierno

calificaciones_gob <- encig_conf %>%
  select(UPM, EST_DIS, FAC_P18, P11_1A_04, P11_1A_06, P11_1A_08) %>%
  pivot_longer(
    cols = starts_with("P11_1A_"),
    names_to = "Nivel_Gobierno",
    values_to = "Calificacion"
  ) %>%
  mutate(Calificacion = as.numeric(Calificacion),
    Nivel_Gobierno = case_when(
      Nivel_Gobierno == "P11_1A_04" ~ "Presidencia de la República",
      Nivel_Gobierno == "P11_1A_06" ~ "Gubernaturas",
      Nivel_Gobierno == "P11_1A_08" ~ "Presidencias Municipales"
    )
  ) %>%
  filter(!is.na(Calificacion)) %>%
  mutate(Nivel_Gobierno = factor(Nivel_Gobierno, levels = c(
    "Presidencia de la República", "Gubernaturas", "Presidencias Municipales"
  )))

promedios_calculados <- calificaciones_gob %>%
  as_survey_design(
    ids = UPM,
    strata = EST_DIS,
    weights = FAC_P18,
    nest = TRUE
  ) %>%
  group_by(Nivel_Gobierno) %>%
  summarise(
    Promedio = survey_mean(Calificacion, na.rm = TRUE, vartype = "ci")
  ) %>%
  ungroup()

promedios_calculados %>% 
ggplot(aes(x = Nivel_Gobierno, y = Promedio)) +
  geom_col(fill = "#2b8cbe", width = 0.5) +
  geom_errorbar(
    aes(ymin = Promedio_low, ymax = Promedio_upp), 
    width = 0.15, 
    color = "gray40",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = round(Promedio, 2)), 
    vjust = -2, 
    fontface = "bold",
    size = 4.5
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(
    title = "Calificación promedio del desempeño gubernamental",
    subtitle = "Escala de 0 a 10. Las líneas de error a 95% de confianza.",
    x = NULL,
    y = "Calificación Promedio",
    caption = "Fuente: Elaboración propia con datos de ENCIG 2025 (INEGI)."
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(), #
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 10))
  )
