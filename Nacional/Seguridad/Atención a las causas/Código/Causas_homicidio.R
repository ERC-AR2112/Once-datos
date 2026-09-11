library(pacman)
p_load(tidyverse, readr, sf, scales, srvyr, janitor, survey)

Mpal_15_25 <- read_csv("Atencion_causas/Municipal-Delitos-2015-2025_jul2026.csv", locale = locale(encoding = "Latin1"))

Mpal_26 <- read_csv("Atencion_causas/RNID-Delitos_Municipal-2026-jul2026.csv", locale = locale(encoding = "Latin1"))

Mpal_15_25 |> count(`Subtipo de delito`) |>  print(n=Inf)

Mpal_15_25 <- Mpal_15_25 |> filter(`Subtipo de delito` %in% c("Homicidio doloso", "Feminicidio")) |> 
  mutate(`Cve. Municipio` = as.numeric(`Cve. Municipio`),
        Clave_Ent = as.numeric(Clave_Ent) )
Mpal_26 <- Mpal_26 |> 
  filter(`Subtipo de delito` %in% c("Homicidio doloso", "Feminicidio")) |> 
  mutate(`Cve. Municipio` = as.numeric(`Cve. Municipio`),
         Clave_Ent = as.numeric(Clave_Ent) )
Mpal <- bind_rows(Mpal_26, Mpal_15_25)

Mpal <- Mpal %>%
  mutate(
    Entidad = case_match(
      Entidad,
      "Ciudad de MÃ©xico"     ~ "Ciudad de México",
      "MichoacÃ¡n de Ocampo"  ~ "Michoacán de Ocampo",
      "MÃ©xico"               ~ "México",
      "Nuevo LeÃ³n"           ~ "Nuevo León",
      "QuerÃ©taro"            ~ "Querétaro",
      "San Luis PotosÃ­"      ~ "San Luis Potosí",
      "YucatÃ¡n"              ~ "Yucatán",
      .default = Entidad
    )
  )

Mpal |> count(Entidad)


##Base con homicidio desagregados homicidio doloso y feminicidio
Mpal_an <- Mpal |> 
  pivot_longer( cols = Enero:Diciembre, names_to = "Mes", values_to = "Total") |> 
  group_by(Año, Mes, Entidad, Clave_Ent, `Cve. Municipio`, Municipio, `Subtipo de delito` ) |> 
  summarise(Total= sum(Total)) |> 
  ungroup()
## Base homicidios dolosos agregados

Mpal_an2 <- Mpal_an |> 
  group_by(Año, Entidad, Clave_Ent, `Cve. Municipio`, Municipio ) |> 
  summarise(Total= sum(Total)) |> 
  ungroup()



library(readxl)
mun_ap <- read_excel("Atencion_causas/mun_ap.xlsx")

base_analisis <- left_join(Mpal_an2, mun_ap, by = "Cve. Municipio")
base_analisis <- base_analisis |> 
  mutate(Priorizado = if_else(is.na(Priorizado), 0, 1))

##Mapa de municipios priorizados

mpios_shp <- read_sf("00mun.shp")

# 2. Preparar datos y unir
datos_mapa <- base_analisis %>%
  distinct(`Cve. Municipio`, .keep_all = TRUE) %>%
  mutate(
    CVEGEO = str_pad(`Cve. Municipio`, width = 5, side = "left", pad = "0"),
    Priorizado_cat = if_else(Priorizado == 1, "Priorizado", "No priorizado")
  )

mapa_sf <- mpios_shp %>%
  left_join(datos_mapa, by = "CVEGEO")

estados_sf <- mapa_sf %>%
  mutate(cve_ent = substr(CVEGEO, 1, 2)) %>%
  group_by(cve_ent) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

ggplot() +
  geom_sf(
    data = mapa_sf,
    aes(fill = Priorizado_cat),
    color = "grey85",
    linewidth = 0.05
  ) +
  geom_sf(
    data = estados_sf,
    fill = NA,
    color = "#2b2b2b",
    linewidth = 0.35
  ) +
  scale_fill_manual(
    values = c("No priorizado" = "#f7f7f7", "Priorizado" = "#b2182b"),
    na.value = "#d9d9d9",
    name = "¿Priorizado?"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30")
  ) +
  labs(
    title = "Municipios Priorizados",
  )


resumen_tendencia_2018 <- base_analisis %>%
  group_by(Año) %>%
  summarise(
    Priorizados    = sum(Total[Priorizado == 1], na.rm = TRUE),
    No_Priorizados = sum(Total[Priorizado == 0], na.rm = TRUE),
    Nacional       = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Participación porcentual anual
    Pct_Priorizados = (Priorizados / Nacional) * 100,
    
    # Índice fijado en 2018 = 100
    Index_Priorizados    = ((Priorizados - Priorizados[Año == 2018])  / Priorizados[Año == 2018]) * 100,
    Index_No_Priorizados = ((No_Priorizados - No_Priorizados[Año == 2018]) / No_Priorizados[Año == 2018]) * 100,
    Index_Nacional       = ((Nacional  - Nacional[Año == 2018])/ Nacional[Año == 2018]) * 100
  )

# Vista previa de los datos
resumen_tendencia_2018 %>%
  select(Año, Priorizados, Nacional, Pct_Priorizados, starts_with("Index_"))


resumen_tendencia_2018 %>%
  filter(Año >= 2018, Año < 2026) |> 
  select(
    Año,
    `Municipios Priorizados` = Index_Priorizados,
    `Resto de Municipios`    = Index_No_Priorizados,
    `Total Nacional`         = Index_Nacional
  ) %>%
  pivot_longer(
    cols = -Año,
    names_to = "Grupo",
    values_to = "Indice"
  ) %>%
  ggplot(aes(x = Año, y = Indice, color = Grupo, linetype = Grupo)) +
  # Línea de referencia en el 100 (año base)
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50", linewidth = 0.6) +
  # Marca vertical en 2018
  geom_vline(xintercept = 2018, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 1.15) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c(
      "Municipios Priorizados" = "#b2182b",
      "Resto de Municipios"    = "#2166ac",
      "Total Nacional"         = "#252525"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Municipios Priorizados" = "solid",
      "Resto de Municipios"    = "dashed",
      "Total Nacional"         = "dotdash"
    )
  ) +
  scale_x_continuous(breaks = seq(min(resumen_tendencia_2018$Año), max(resumen_tendencia_2018$Año), by = 1)) +
  labs(
    title = "Evolución Relativa de Homicidios Dolosos (2018 = 100)",
    subtitle = "Comparativa entre municipios prioritarios, resto del país y promedio nacional",
    y = "Índice (2018 = 100)",
    x = "Año",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10)
  )


###Para 2026 

base_analisis <- left_join(Mpal_an, mun_ap, by = "Cve. Municipio")
base_analisis <- base_analisis |> 
  mutate(Priorizado = if_else(is.na(Priorizado), 0, 1))

meses_corte <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio")

resumen_tendencia <- base_analisis %>%
  group_by(Año) %>%
  summarise(
    # Homicidios en municipios priorizados
    Homicidios_Priorizados = sum(Total[Priorizado == 1], na.rm = TRUE),
    # Homicidios en el resto de municipios
    Homicidios_No_Priorizados = sum(Total[Priorizado == 0], na.rm = TRUE),
    # Total país (todos los municipios)
    Homicidios_Nacional = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Var_Pct_Priorizados = ((Homicidios_Priorizados-lag(Homicidios_Priorizados)) / lag(Homicidios_Priorizados)) * 100,
    Var_Pct_Nacional = ((Homicidios_Nacional-lag(Homicidios_Nacional)) / lag(Homicidios_Nacional)) * 100,
    Var_pct_NoPriorizados = ((Homicidios_No_Priorizados- lag(Homicidios_No_Priorizados))/ lag(Homicidios_No_Priorizados))*100)

print(resumen_tendencia)




base_jul_2025 <- base_analisis %>%
  filter(Año == 2025, Mes %in% meses_corte) %>%
  summarise(
    Prio_Jul25   = sum(Total[Priorizado == 1], na.rm = TRUE),
    NoPrio_Jul25 = sum(Total[Priorizado == 0], na.rm = TRUE),
    Nac_Jul25    = sum(Total, na.rm = TRUE)
  )


val_prio_jul25   <- base_jul_2025$Prio_Jul25
val_noprio_jul25 <- base_jul_2025$NoPrio_Jul25
val_nac_jul25    <- base_jul_2025$Nac_Jul25


resumen_tendencia <- base_analisis %>%
  group_by(Año) %>%
  summarise(
    Homicidios_Priorizados    = sum(Total[Priorizado == 1], na.rm = TRUE),
    Homicidios_No_Priorizados = sum(Total[Priorizado == 0], na.rm = TRUE),
    Homicidios_Nacional       = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Var_Pct_Priorizados = if_else(
      Año == 2026,
      ((Homicidios_Priorizados - val_prio_jul25) / val_prio_jul25) * 100,
      ((Homicidios_Priorizados - lag(Homicidios_Priorizados)) / lag(Homicidios_Priorizados)) * 100
    ),
    Var_Pct_NoPriorizados = if_else(
      Año == 2026,
      ((Homicidios_No_Priorizados - val_noprio_jul25) / val_noprio_jul25) * 100,
      ((Homicidios_No_Priorizados - lag(Homicidios_No_Priorizados)) / lag(Homicidios_No_Priorizados)) * 100
    ),
    Var_Pct_Nacional = if_else(
      Año == 2026,
      ((Homicidios_Nacional - val_nac_jul25) / val_nac_jul25) * 100,
      ((Homicidios_Nacional - lag(Homicidios_Nacional)) / lag(Homicidios_Nacional)) * 100
    ),
    Periodo_Comparado = if_else(
      Año == 2026,
      "Ene-Jul 2026 vs Ene-Jul 2025",
      "Año completo vs año previo"
    )
  )

print(resumen_tendencia)

resumen_tendencia %>%
  select(Año, `Municipios Priorizados` = Var_Pct_Priorizados, 
         `Resto de Municipios` = Var_Pct_NoPriorizados, 
         `Total Nacional` = Var_Pct_Nacional) %>%
  pivot_longer(cols = -Año, names_to = "Grupo", values_to = "Indice") %>%
  ggplot(aes(x = Año, y = Indice, color = Grupo, linetype = Grupo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey40") +
  scale_color_manual(values = c("Municipios Priorizados" = "#b2182b", 
                                "Resto de Municipios" = "#4393c3", 
                                "Total Nacional" = "#252525")) +
  scale_linetype_manual(values = c("Municipios Priorizados" = "solid", 
                                   "Resto de Municipios" = "dashed", 
                                   "Total Nacional" = "dotdash")) +
  scale_x_continuous(breaks = seq(min(resumen_tendencia$Año), max(resumen_tendencia$Año), by = 1)) +
  theme_minimal(base_family = "sans") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  labs(
    title = "Cambio interanual de Homicidios",
    subtitle = "Comparación entre municipios priorizados, resto del país y total nacional",
    y = "Porcentaje",
    x = "Año",
    color = NULL, linetype = NULL,
    caption = "Ene-Jul 2026 vs Ene-Jul 2025
    Año completo vs año previo
    Elaboración propia con datos de incidencia delictiva del SESNSP")

### """""""""""""""""""""""""""""Percepción"""""""""""""""""""""""""""""""""""""""

#Percepción estatal, segurida en su colonia####################################

library(readxl)

Envipe_col_percep <- read_excel("Atencion_causas/Envipe_Percepcion_seg_col_estatal.xlsx", 
                                                range = "a7:h1591")

##Grafica nacional
Envipe_col_percep |> 
  filter(Sexo=="Total", `Entidad federativa` == "Estados Unidos Mexicanos") |> 
  mutate(Año = as.numeric(Año)) |> 
  ggplot(aes(x=Año, y= `Estimaciones puntuales`, ymin = `Límite inferior`, ymax = `Límite superior`))+
  geom_line(linewidth = 1)+
  geom_ribbon( alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Tendencia de Percepción de Inseguridad en la colonia (ENVIPE)",
       y = "Estimación puntual",
       x = "Año",
       caption = "Elaboración propia con base en tabulados de ENVIPE 2026-INEGI")



##Grafica estatal
Envipe_col_percep |> 
  filter(Sexo=="Total", `Entidad federativa` != "Estados Unidos Mexicanos") |> 
  mutate(Año = as.numeric(Año)) |> 
  ggplot(aes(x=Año, y= `Estimaciones puntuales`,colour = `Entidad federativa`, ymin = `Límite inferior`, ymax = `Límite superior`))+
  geom_line(linewidth = 1)+
  geom_ribbon( aes( fill=`Entidad federativa` ), alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Tendencia de Percepción de Inseguridad en la colonia por Estado (ENVIPE)",
       y = "Estimación puntual",
       x = "Año",
       caption = "Elaboración propia con base en tabulados de ENVIPE 2026-INEGI")+
  facet_wrap(~ `Entidad federativa`, ncol=4)

##### Percepción estatal, segurida en su municipio################################

Envipe_Percep_seg_mun <- read_excel("Atencion_causas/Envipe_Percep_seg_mun_dem_estatal.xlsx", 
                                range = "a7:h1591")

##Grafica nacional
Envipe_Percep_seg_mun |> 
  filter(Sexo=="Total", `Entidad federativa` == "Estados Unidos Mexicanos") |> 
  mutate(Año = as.numeric(Año)) |> 
  ggplot(aes(x=Año, y= `Estimaciones puntuales`, ymin = `Límite inferior`, ymax = `Límite superior`))+
  geom_line(linewidth = 1)+
  geom_ribbon( alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Tendencia de Percepción de Inseguridad en el municipio (ENVIPE)",
       y = "Estimación puntual",
       x = "Año",
       caption = "Elaboración propia con base en tabulados de ENVIPE 2026-INEGI")



##Grafica estatal
Envipe_Percep_seg_mun|> 
  filter(Sexo=="Total", `Entidad federativa` != "Estados Unidos Mexicanos") |> 
  mutate(Año = as.numeric(Año)) |> 
  ggplot(aes(x=Año, y= `Estimaciones puntuales`,colour = `Entidad federativa`, ymin = `Límite inferior`, ymax = `Límite superior`))+
  geom_line(linewidth = 1)+
  geom_ribbon( aes( fill=`Entidad federativa` ), alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Tendencia de Percepción de Inseguridad en el municipio por Estado (ENVIPE)",
       y = "Estimación puntual",
       x = "Año",
       caption = "Elaboración propia con base en tabulados de ENVIPE 2026-INEGI")+
  facet_wrap(~ `Entidad federativa`, ncol=4)



##Percepción de inseguridad en la entidfad por entidad####################

Envipe_ent <- read_excel("Atencion_causas/Percepcion_ent_ent.xlsx", 
                                    range = "a7:h1591")

##Grafica nacional
Envipe_ent |> 
  filter(Sexo=="Total", `Entidad federativa` == "Estados Unidos Mexicanos") |> 
  mutate(Año = as.numeric(Año)) |> 
  ggplot(aes(x=Año, y= `Estimaciones puntuales`, ymin = `Límite inferior`, ymax = `Límite superior`))+
  geom_line(linewidth = 1)+
  geom_ribbon( alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Tendencia de Percepción de Inseguridad en la entidad federativa (ENVIPE)",
       y = "Estimación puntual",
       x = "Año",
       caption = "Elaboración propia con base en tabulados de ENVIPE 2026-INEGI")



##Grafica estatal
Envipe_ent |> 
  filter(Sexo=="Total", `Entidad federativa` != "Estados Unidos Mexicanos") |> 
  mutate(Año = as.numeric(Año)) |> 
  ggplot(aes(x=Año, y= `Estimaciones puntuales`,colour = `Entidad federativa`, ymin = `Límite inferior`, ymax = `Límite superior`))+
  geom_line(linewidth = 1)+
  geom_ribbon( aes( fill=`Entidad federativa` ), alpha = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Tendencia de Percepción de Inseguridad en cada entidad por Entidad Federativa (ENVIPE)",
       y = "Estimación puntual",
       x = "Año",
       caption = "Elaboración propia con base en tabulados de ENVIPE 2026-INEGI")+
  facet_wrap(~ `Entidad federativa`, ncol=4)


#### percepción municipal##################################################

library(haven)

#"""""""""""2026"""""""""""""""""""""""""""""""""""""""""""""""""""""""
tper_vic1 <- read_dta("Atencion_causas/Envipe2026/TPer_Vic1.dta")
tmod_vic <- read_dta("Atencion_causas/Envipe2026/TMod_Vic.dta")

victimas <- tmod_vic |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic1 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2026
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2026 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2026)

#"""""""""""""""2025""""""""""""""""""""""""""""""""""""""""""""""""""""""""

tper_vic125 <- read_dta("Atencion_causas/Envipe2025/TPer_Vic1.dta")
tmod_vic25 <- read_dta("Atencion_causas/Envipe2025/TMod_Vic.dta")

victimas <- tmod_vic25 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic125 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2025
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2025 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2025)

#""""""""""""""""""""""""2024"""""""""""""""""""""""""""""""""""""""""""""""
tper_vic124 <- read_dta("Atencion_causas/Envipe2024/TPer_Vic1.dta")
tmod_vic24 <- read_dta("Atencion_causas/Envipe2024/TMod_Vic.dta")

victimas <- tmod_vic24 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic124 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2024
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2024 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2024)


#"""""""""""""""""""""""""""""""""2023""""""""""""""""""""""""""""""""""""""""

tper_vic123 <- read_dta("Atencion_causas/Envipe2023/TPer_Vic1.dta")
tmod_vic23 <- read_dta("Atencion_causas/Envipe2023/TMod_Vic.dta")

victimas <- tmod_vic23 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic123 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2023
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2023 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2023)


#"""""""""""""""""""""""""""""""""""""""2022""""""""""""""""""""""""""""""""""""

tper_vic122 <- read_dta("Atencion_causas/Envipe2022/TPer_Vic1.dta")
tmod_vic22 <- read_dta("Atencion_causas/Envipe2022/TMod_Vic.dta")

victimas <- tmod_vic22 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic122 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2022
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2022 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2022)


#"""""""""""""""""""""""""""""""""""2021"""""""""""""""""""""""""""""""""""""""
tper_vic121 <- read_dta("Atencion_causas/Envipe2021/TPer_Vic1.dta")
tmod_vic21 <- read_dta("Atencion_causas/Envipe2021/TMod_Vic.dta")

victimas <- tmod_vic21 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic121 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2021
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2021 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2021)


#""""""""""""""""""""""""""""""""2020""""""""""""""""""""""""""""""""""""""""
library(foreign)
tper_vic120 <- read.dbf("Atencion_causas/Envipe2020/TPer_Vic1.dbf", as.is = TRUE)
tmod_vic20 <- read.dbf("Atencion_causas/Envipe2020/TMod_Vic.dbf", as.is = TRUE)

tper_vic120 <- tper_vic120 |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))

tmod_vic20 <- tmod_vic20 |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))

victimas <- tmod_vic20 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic120 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2020
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2020 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2020)

#""""""""""""""""""""""""""""""""""""""2019""""""""""""""""""""""""""""""""""
tper_vic119 <- read.dbf("Atencion_causas/Envipe2019/TPer_Vic1.dbf", as.is = TRUE)
tmod_vic19 <- read.dbf("Atencion_causas/Envipe2019/TMod_Vic.dbf", as.is = TRUE)

tper_vic119 <- tper_vic119 |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))

tmod_vic19 <- tmod_vic19 |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))

victimas <- tmod_vic19 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic119 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2019
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2019 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2019)

#"""""""""""""""""""""""""2018"""""""""""""""""""""""""""""""""""""""""""""""
tper_vic118 <- read.dbf("Atencion_causas/Envipe2018/TPer_Vic1.dbf", as.is = TRUE)
tmod_vic18 <- read.dbf("Atencion_causas/Envipe2018/TMod_Vic.dbf", as.is = TRUE)

tper_vic118 <- tper_vic118 |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))

tmod_vic18 <- tmod_vic18 |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "latin1", to = "UTF-8")))

victimas <- tmod_vic18 |> 
  select(UPM, VIV_SEL, HOGAR, R_SEL) |> 
  distinct() |> 
  mutate(es_victima = 1)


datos_analisis <- tper_vic118 |> 
  left_join(victimas, by = c("UPM", "VIV_SEL", "HOGAR", "R_SEL")) |> 
  mutate(
    es_victima = ifelse(is.na(es_victima), 0, es_victima),
    inseg_colonia   = ifelse(AP4_3_1 == 2, 1, 0),
    inseg_municipio = ifelse(AP4_3_2 == 2, 1, 0),
    inseg_estado    = ifelse(AP4_3_3 == 2, 1, 0),
    FAC_ELE_AM      = as.numeric(FAC_ELE_AM),
    Año = 2018
  ) |> 
  filter(!is.na(AREAM), AREAM != "b", AREAM != " ", AREAM != "") 


diseno_envipe <- datos_analisis |> 
  as_survey_design(
    ids = UPM_DIS,         # Unidades Primarias de Muestreo (Clusters)
    strata = EST_DIS,      # Estratos de diseño
    weights = FAC_ELE_AM,  # Factor de expansión de área metropolitana
    nest = TRUE            # Indica que las UPM están anidadas en estratos
  )

# 5. Calcular los porcentajes y sus intervalos de confianza (al 95%)
tabla_municipios_ic2018 <- diseno_envipe |> 
  group_by(Año, AREAM, CVE_ENT, NOM_ENT, CVE_MUN, NOM_MUN) |> 
  summarise(
    # survey_mean con vartype = "ci" calcula el valor puntual y los límites inferior y superior
    Inseg_Colonia = survey_mean(inseg_colonia, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Municipio = survey_mean(inseg_municipio, vartype = "ci", na.rm = TRUE) * 100,
    Inseg_Estado = survey_mean(inseg_estado, vartype = "ci", na.rm = TRUE) * 100,
    Victimas = survey_mean(es_victima, vartype = "ci", na.rm = TRUE) * 100
  )


# Ver el resultado
View(tabla_municipios_ic2018)


tabla_municipios_ic <- rbind(tabla_municipios_ic2018, tabla_municipios_ic2019, tabla_municipios_ic2020, tabla_municipios_ic2021, tabla_municipios_ic2022, tabla_municipios_ic2023, tabla_municipios_ic2024, tabla_municipios_ic2025, tabla_municipios_ic2026)

