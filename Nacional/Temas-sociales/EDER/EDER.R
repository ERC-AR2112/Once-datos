library(pacman)
p_load(readr, tidyverse, srvyr)

df_informante <- read_csv("EDER/informante.csv")
df_antecedentes <- read_csv("EDER/antecedentes.csv")
df_historia <- read_csv("EDER/historiavida.csv")

df_completa <- df_informante %>%
  inner_join(df_antecedentes, by = c("folioviv", "foliohog", "id_pobla"))

df_analisis <- df_completa %>%
  mutate(
    # Crear cohortes generacionales basadas en la edad actual (edad_act)
    cohorte = case_when(
      edad_act >= 18 & edad_act <= 29 ~ "Generación Z / Millenials Jóvenes (18-29)",
      edad_act >= 30 & edad_act <= 44 ~ "Millenials / Gen X (30-44)",
      edad_act >= 45 & edad_act <= 64 ~ "Gen X / Boomers (45-64)",
      TRUE ~ NA_character_
    ),
    fecundidad_madre = as.numeric(hijos_m),
    fecundidad_informante = as.numeric(hij_vivos),
    edad_inicio_sexual = ifelse(edad_rsex %in% c(88, 99), NA, as.numeric(edad_rsex)),
    num_uniones = as.numeric(matrimonio),
    uso_anticonceptivo = case_when(
      anticoncep == 1 ~ 1,
      anticoncep == 2 ~ 0,
      TRUE ~ NA_real_)
  ) %>%
  filter(!is.na(cohorte) & !is.na(factor_per.x) & !is.na(upm.x) & !is.na(est_dis.x)) 


eder_diseno <- df_analisis %>%
  as_survey_design(
    strata = est_dis.x,    
    weights = factor_per.x,
    nest = TRUE          
  )

###  Familia y fecundidad

# Inicio de viuda sexual 
estimacion_inicio_sexual <- eder_diseno %>%
  group_by(cohorte) %>%
  summarise(
    edad_media = survey_mean(edad_inicio_sexual, na.rm = TRUE, vartype = "ci") # Incluye Intervalos de Confianza
  )

ggplot(estimacion_inicio_sexual, aes(x = cohorte, y = edad_media, color = cohorte)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = edad_media_low, ymax = edad_media_upp), width = 0.2) +
  theme_minimal() +
  labs(title = "Edad Media de la Primera Relación Sexual por Cohorte",
       x = "Cohorte", y = "Edad Media (Años)") +
  theme(legend.position = "none")


#Cambios en pnúmero de hijos

estimacion_fecundidad <- eder_diseno %>%
  group_by(cohorte) %>%
  summarise(
    prom_hijos_madre = survey_mean(fecundidad_madre, na.rm = TRUE),
    prom_hijos_info = survey_mean(fecundidad_informante, na.rm = TRUE)
  )

estimacion_fecundidad %>%
  select(cohorte, prom_hijos_madre, prom_hijos_info) %>%
  pivot_longer(-cohorte, names_to = "generacion", values_to = "promedio") %>%
  mutate(generacion = ifelse(generacion == "prom_hijos_madre", "Madre", "Informante")) %>%
  ggplot(aes(x = cohorte, y = promedio, fill = generacion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  labs(title = "Transición de la Fecundidad Intergeneracional (Estimación Nacional)",
       x = "Cohorte del Informante", y = "Promedio de hijos", fill = "Generación")

### Uso de anticonceptivos 

estimacion_anticoncepcion <- eder_diseno %>%
  filter(!is.na(edad_inicio_sexual)) %>% 
  group_by(cohorte) %>%
  summarise(
    prev_uso = survey_mean(uso_anticonceptivo, na.rm = TRUE)
  ) %>%
  mutate(porcentaje = prev_uso * 100)

ggplot(estimacion_anticoncepcion, aes(x = cohorte, y = porcentaje)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), vjust = -0.8, fontface = "bold") +
  theme_minimal() +
  labs(title = "Prevalencia de Uso Histórico de Anticonceptivos",
       x = "Cohorte", y = "Porcentaje de la población (%)")


######## Traajo y educación


df_analisis_socioecon <- df_completa %>%
  mutate(
    cohorte = case_when(
      edad_act >= 18 & edad_act <= 29 ~ "Generación Z / Millenials Jóvenes (18-29)",
      edad_act >= 30 & edad_act <= 44 ~ "Millenials / Gen X (30-44)",
      edad_act >= 45 & edad_act <= 64 ~ "Gen X / Boomers (45-64)",
      TRUE ~ NA_character_
    ),
    edu_informante = case_when(
      nivela_pe %in% c("00", "01", "02", "03", "04", "05", "06", "07", "08") ~ "Básica o Menos",
      nivela_pe %in% c("09", "10") ~ "Media Superior",
      nivela_pe %in% c("11", "12", "13") ~ "Superior",
      TRUE ~ NA_character_
    ),
    edu_padre = case_when(
      nivel_p %in% c("00", "05", "06", "07", "08", "09", "10", "11") ~ "Básica o Menos",
      nivel_p %in% c("12", "13") ~ "Media Superior",
      nivel_p %in% c("14", "15", "16") ~ "Superior",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cohorte) & !is.na(factor_per.x) & !is.na(upm.x) & !is.na(est_dis.x))

# ---------------------------------------------------------
# 2. DECLARACIÓN DEL DISEÑO MUESTRAL CON SRVYR
# ---------------------------------------------------------
eder_diseno_se <- df_analisis_socioecon %>%
  as_survey_design(
    strata = est_dis.x,
    weights = factor_per.x,
    nest = TRUE
  )


estimacion_educacion <- eder_diseno_se %>%
  group_by(cohorte) %>%
  summarise(
    pct_superior_info = survey_mean(edu_informante == "Superior", na.rm = TRUE) * 100,
    pct_superior_padre = survey_mean(edu_padre == "Superior", na.rm = TRUE) * 100
  ) %>%
  select(cohorte, pct_superior_info, pct_superior_padre) %>%
  pivot_longer(cols = c(pct_superior_info, pct_superior_padre), 
               names_to = "generacion", 
               values_to = "porcentaje") %>%
  mutate(generacion = ifelse(generacion == "pct_superior_info", "Informante", "Padre"))



ggplot(estimacion_educacion, aes(x = cohorte, y = porcentaje, fill = generacion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(porcentaje, 2)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Informante" = "#2980b9", "Padre" = "#7f8c8d")) +
  theme_minimal() +
  labs(title = "Movilidad Educativa Intergeneracional",
       subtitle = "Porcentaje de la población con Educación Superior (Informante vs. Padre)",
       x = "Cohorte del Informante",
       y = "Porcentaje (%)",
       fill = "Generación")

# --- GRÁFICO 2: Transición Ocupacional (De lo Agrícola a lo Profesional) ---
# Comparamos la ocupación del informante vs la del padre en la misma cohorte
estimacion_ocupacion <- eder_diseno_se %>%
  group_by(cohorte) %>%
  summarise(
    # Porcentaje en sector agrícola
    pct_agricola_info = survey_mean(sector_info == "Agrícola", na.rm = TRUE) * 100,
    pct_agricola_padre = survey_mean(sector_padre == "Agrícola", na.rm = TRUE) * 100,
    
    # Porcentaje en sector profesional
    pct_prof_info = survey_mean(sector_info == "Profesionales/Directivos", na.rm = TRUE) * 100,
    pct_prof_padre = survey_mean(sector_padre == "Profesionales/Directivos", na.rm = TRUE) * 100
  )

# Preparamos los datos para un gráfico facetado
df_plot_occ <- estimacion_ocupacion %>%
  select(cohorte, pct_agricola_info, pct_agricola_padre, pct_prof_info, pct_prof_padre) %>%
  pivot_longer(-cohorte, names_to = "variable", values_to = "porcentaje") %>%
  mutate(
    Sector = ifelse(str_detect(variable, "agricola"), "Agrícola", "Profesional/Directivo"),
    Generacion = ifelse(str_detect(variable, "info"), "Informante", "Padre")
  )

ggplot(df_plot_occ, aes(x = cohorte, y = porcentaje, fill = Generacion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~Sector, scales = "free_y") +
  scale_fill_manual(values = c("Informante" = "#27ae60", "Padre" = "#95a5a6")) +
  theme_minimal() +
  labs(title = "Cambio Estructural en el Empleo por Cohortes",
       subtitle = "Comparación de la ocupación principal: Padres vs. Informantes",
       x = "Cohorte del Informante",
       y = "Porcentaje (%)",
       fill = "Generación") +
  theme(strip.text = element_text(face = "bold", size = 12))



df_ocupacion_actual <- df_historia %>%
  filter(!is.na(sinco) & sinco != "0000") %>% 
  group_by(folioviv, foliohog, id_pobla) %>%
  arrange(desc(anio_retro)) %>%
  slice(1) %>%
  select(folioviv, foliohog, id_pobla, sinco) %>%
  ungroup()

df_completa <- df_informante %>%
  inner_join(df_antecedentes, by = c("folioviv", "foliohog", "id_pobla")) %>%
  left_join(df_ocupacion_actual, by = c("folioviv", "foliohog", "id_pobla"))


df_analisis_socioecon <- df_completa %>%
  mutate(
      cohorte = case_when(
        edad_act >= 18 & edad_act <= 29 ~ "Generación Z / Millenials Jóvenes (18-29)",
        edad_act >= 30 & edad_act <= 44 ~ "Millenials / Gen X (30-44)",
        edad_act >= 45 & edad_act <= 64 ~ "Gen X / Boomers (45-64)",
        TRUE ~ NA_character_
      ),
    grupo_occ_info = substr(as.character(sinco), 1, 1),
    grupo_occ_padre = substr(as.character(sinco_p), 1, 1),
    
    sector_info = case_when(
      grupo_occ_info %in% c("1", "2") ~ "Profesionales/Directivos",
      grupo_occ_info == "6" ~ "Agrícola",
      !is.na(grupo_occ_info) & grupo_occ_info != "9" ~ "Otros Sectores",
      TRUE ~ NA_character_
    ),
    
    sector_padre = case_when(
      grupo_occ_padre %in% c("1", "2") ~ "Profesionales/Directivos",
      grupo_occ_padre == "6" ~ "Agrícola",
      !is.na(grupo_occ_padre) & grupo_occ_padre != "9" ~ "Otros Sectores",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cohorte) & !is.na(factor_per.x) & !is.na(upm.x) & !is.na(est_dis.x))


eder_diseno_se <- df_analisis_socioecon %>%
  as_survey_design(
    strata = est_dis.x,
    weights = factor_per.x,
    nest = TRUE
  )

estimacion_ocupacion <- eder_diseno_se %>%
  group_by(cohorte) %>%
  summarise(
    pct_agricola_info = survey_mean(sector_info == "Agrícola", na.rm = TRUE) * 100,
    pct_agricola_padre = survey_mean(sector_padre == "Agrícola", na.rm = TRUE) * 100,
    pct_prof_info = survey_mean(sector_info == "Profesionales/Directivos", na.rm = TRUE) * 100,
    pct_prof_padre = survey_mean(sector_padre == "Profesionales/Directivos", na.rm = TRUE) * 100
  )

df_plot_occ <- estimacion_ocupacion %>%
  select(cohorte, pct_agricola_info, pct_agricola_padre, pct_prof_info, pct_prof_padre) %>%
  pivot_longer(-cohorte, names_to = "variable", values_to = "porcentaje") %>%
  mutate(
    Sector = ifelse(str_detect(variable, "agricola"), "Agrícola", "Profesional/Directivo"),
    Generacion = ifelse(str_detect(variable, "info"), "Informante", "Padre")
  )

ggplot(df_plot_occ, aes(x = cohorte, y = porcentaje, fill = Generacion)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~Sector, scales = "free_y") +
  scale_fill_manual(values = c("Informante" = "#27ae60", "Padre" = "#95a5a6")) +
  theme_minimal() +
  labs(title = "Cambio Estructural en el Empleo por Cohortes",
       subtitle = "Comparación de la ocupación principal: Padres vs. Informantes",
       x = "Cohorte del Informante",
       y = "Porcentaje (%)",
       fill = "Generación") +
  theme(strip.text = element_text(face = "bold", size = 12))


