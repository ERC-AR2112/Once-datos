### CAF

library(haven)
library(forcats)
library(srvyr)
library(tidyverse)
library(scales)
usuarios <- read_sav("consultorios farmacias/ENSANUT/utilizadores_2025_w.sav")
integrantes <- read_sav("consultorios farmacias/ENSANUT/integrantes_2025_w.sav")
hogar <- read_sav("consultorios farmacias/ENSANUT/hogar_2025_w.sav")

usuarios <- usuarios |> filter(!is.na(u0201), !u0201 %in% c(23, 99)) %>%
  mutate(
    servicio_salud = as_factor(u0201),
    servicio_salud = fct_lump_min(servicio_salud, min = 30, other_level = "Otras instituciones")
  )

diseno_ensanut <- usuarios %>%
  as_survey_design(
    ids = mi_upm,           
    strata = estrato,     
    weights = ponde_f,     
    nest = TRUE
  )  

tabla_servicios <- diseno_ensanut %>%
  group_by(servicio_salud) %>%
  summarize(
    total_pob = survey_total(vartype = "ci"), 
    proporcion = survey_prop(vartype = "ci")    
  ) %>%
  filter(!is.na(servicio_salud))

ggplot(tabla_servicios, aes(x = proporcion, y = fct_reorder(servicio_salud, proporcion))) +
  geom_col(fill = "#1b4965", alpha = 0.9, width = 0.7) +
  geom_errorbar(
    aes(xmin = proporcion_low, xmax = proporcion_upp),
    width = 0.25,
    color = "#2b2d42"
  ) +
  geom_text(
    aes(label = percent(proporcion, accuracy = 0.1)),
    hjust = -0.3,
    size = 3.5,
    color = "black"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Distribución de la atención por servicio de salud",
    subtitle = "Población que utilizó servicios de salud en los últimos 3 meses (ENSANUT 2025)",
    x = "Porcentaje de personas atendidas (ponderado)",
    y = NULL,
    caption = "Fuente: ENSANUT Continua 2025. Estimaciones ponderadas con IC del 95%."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 14)
  )



nombres_motivos <- c(
  "u0202_01" = "Tiene afiliación",
  "u0202_02" = "Está cerca",
  "u0202_03" = "Es barato / No cuesta",
  "u0202_04" = "El horario de atención es amplio",
  "u0202_05" = "No es necesario agendar cita",
  "u0202_06" = "Se tardan poco en dar cita",
  "u0202_07" = "Ofrece el servicio que necesito",
  "u0202_08" = "Es fácil agendar una cita",
  "u0202_09" = "No tuve otra opción",
  "u0202_10" = "Le atienden rápido",
  "u0202_11" = "Le gusta cómo lo(a) atienden",
  "u0202_12" = "Ya tiene cita",
  "u0202_13" = "Conoce al prestador de servicios",
  "u0202_14" = "Vi publicidad de este sitio",
  "u0202_15" = "Me recomendaron este lugar/prestador",
  "u0202_16" = "Otro motivo"
)

# 2. Filtrar subpoblación de CAF (u0201 == 12)
df_caf <- usuarios %>%
  filter(u0201 == 12)

# 3. Identificar cuáles variables del diccionario existen realmente en tu base
vars_existentes <- intersect(names(nombres_motivos), names(df_caf))

# Si no encuentra ninguna, revisa con grep cómo vienen nombradas:
# grep("^u0202", names(df_raw), value = TRUE)

# 4. Asegurar que las variables existentes sean binarias (1 = Sí, 0 = No)
df_caf <- df_caf %>%
  mutate(across(all_of(vars_existentes), ~ ifelse(!is.na(.) & . == 1, 1, 0)))

# 5. Declarar diseño muestral
diseno_caf <- df_caf %>%
  as_survey_design(
    ids = mi_upm,           
    strata = estrato,     
    weights = ponde_f,     
    nest = TRUE
  )  

# 6. Calcular estimaciones iterativamente (sin pivoteos propensos a error)
tabla_motivos <- map_dfr(vars_existentes, function(var) {
  res <- diseno_caf %>%
    summarize(
      prop = survey_mean(.data[[var]], vartype = "ci", na.rm = TRUE)
    )
  
  tibble(
    variable = var,
    motivo   = nombres_motivos[var],
    prop     = res$prop,
    low      = res$prop_low,
    upp      = res$prop_upp
  )
})

# 7. Gráfica
ggplot(tabla_motivos, aes(x = prop, y = fct_reorder(motivo, prop))) +
  geom_col(fill = "#2a9d8f", alpha = 0.9, width = 0.7) +
  geom_errorbar(
    aes(xmin = low, xmax = upp),
    width = 0.25,
    color = "#264653"
  ) +
  geom_text(
    aes(label = percent(prop, accuracy = 0.1)),
    hjust = -0.2,
    size = 3.6,
    color = "black"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Motivos para acudir a Consultorios Adyacentes a Farmacias (CAF)",
    subtitle = "Estimación ponderada en usuarios de CAF (ENSANUT 2025)",
    x = "Porcentaje de usuarios que mencionaron el motivo",
    y = NULL,
    caption = "Fuente: ENSANUT Continua 2025. Pregunta 2.2. Estimaciones con IC del 95%."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 10),
    plot.title = element_text(face = "bold", size = 13)
  )



# 1. Cargar y limpiar montos
df_gastos <- usuarios %>%
  filter(!is.na(u0201), !u0201 %in% c(23, 99)) %>% # Excluir NS/NR de institución
  mutate(
    # Recodificar etiquetas de institución
    servicio_salud = as_factor(u0201),
    servicio_salud = fct_lump_min(servicio_salud, min = 30, other_level = "Otras instituciones"),
    
    # Limpieza de costo de consulta (u0208):
    # - Si u0207a == 2 (No cobraron) -> 0
    # - Códigos especiales 7777 / 77777 (No pagó) -> 0
    # - Códigos 9999 / 99999 (NS/NR) o valores negativos -> NA
    costo_consulta = case_when(
      u0207a == 2 ~ 0,
      u0208 %in% c(7777, 77777, 777777) ~ 0,
      u0208 %in% c(9999, 99999, 999999) ~ NA_real_,
      u0208 < 0 ~ NA_real_,
      TRUE ~ as.numeric(u0208)
    ),
    
    # Limpieza de costo de medicamentos (u0306) si deseas gasto de bolsillo total:
    costo_medicamentos = case_when(
      u0306 %in% c(7777, 77777, 777777) ~ 0,
      u0306 %in% c(9999, 99999, 999999) ~ NA_real_,
      u0306 < 0 ~ NA_real_,
      is.na(u0306) ~ 0,
      TRUE ~ as.numeric(u0306)
    ),
    
    # Gasto combinado (consulta + medicamentos)
    gasto_bolsillo = costo_consulta + costo_medicamentos
  )

# 2. Declarar el diseño muestral
diseno_gastos <- df_gastos %>%
  as_survey_design(
    ids = mi_upm,           
    strata = estrato,     
    weights = ponde_f,     
    nest = TRUE
  )  

# 3. Estimar costo promedio ponderado de la consulta por institución
tabla_costos <- diseno_gastos %>%
  filter(!is.na(gasto_bolsillo)) %>%
  group_by(servicio_salud) %>%
  summarize(
    media_costo = survey_mean(gasto_bolsillo, vartype = "ci", na.rm = TRUE),
    n_muestral  = unweighted(n())
  ) %>%
  filter(!is.na(servicio_salud), n_muestral >= 10)

# 4. Gráfica de costo promedio
ggplot(tabla_costos, aes(x = media_costo, y = fct_reorder(servicio_salud, media_costo))) +
  geom_col(fill = "#e76f51", alpha = 0.9, width = 0.7) +
  geom_errorbar(
    aes(xmin = media_costo_low, xmax = media_costo_upp),
    width = 0.25,
    color = "#264653"
  ) +
  geom_text(
    aes(label = dollar(media_costo, accuracy = 1, prefix = "$")),
    hjust = -0.2,
    size = 3.6,
    color = "black"
  ) +
  scale_x_continuous(
    labels = dollar_format(prefix = "$"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Costo promedio de la consulta por servicio de salud",
    subtitle = "Gasto de bolsillo en consulta médica reportado en ENSANUT 2025 (Pesos Mexicanos)",
    x = "Costo promedio ponderado (MXN)",
    y = NULL,
    caption = "Fuente: ENSANUT Continua 2025. Preguntas 2.7a y 2.8. Estimaciones con IC del 95%."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 13)
  )



catalogo_entidades <- c(
  "01" = "Aguascalientes", "02" = "Baja California", "03" = "Baja California Sur",
  "04" = "Campeche", "05" = "Coahuila", "06" = "Colima", "07" = "Chiapas",
  "08" = "Chihuahua", "09" = "Ciudad de México", "10" = "Durango",
  "11" = "Guanajuato", "12" = "Guerrero", "13" = "Hidalgo", "14" = "Jalisco",
  "15" = "México", "16" = "Michoacán", "17" = "Morelos", "18" = "Nayarit",
  "19" = "Nuevo León", "20" = "Oaxaca", "21" = "Puebla", "22" = "Querétaro",
  "23" = "Quintana Roo", "24" = "San Luis Potosí", "25" = "Sinaloa",
  "26" = "Sonora", "27" = "Tabasco", "28" = "Tamaulipas", "29" = "Tlaxcala",
  "30" = "Veracruz", "31" = "Yucatán", "32" = "Zacatecas"
)

# 2. Preparar los datos
df_entidades <- usuarios %>%
  # Excluir no respuestas de institución de atención
  filter(!is.na(u0201), !u0201 %in% c(23, 99)) %>%
  mutate(
    # Crear variable dicotómica: 1 si fue en farmacia, 0 en otro lugar
    atencion_caf = ifelse(u0201 == 12, 1, 0),
    
    # Asegurar formato de clave de entidad a 2 dígitos y etiquetar
    cve_ent = stringr::str_pad(as.character(entidad), width = 2, pad = "0"),
    nombre_entidad = recode(cve_ent, !!!catalogo_entidades)
  )

# 3. Declarar el diseño muestral
diseno_entidades <- df_entidades %>%
  as_survey_design(
    ids = mi_upm,           
    strata = estrato,     
    weights = ponde_f,     
    nest = TRUE
  ) 

# 4. Estimar proporción de uso de CAF por entidad federativa
tabla_entidades <- diseno_entidades %>%
  group_by(nombre_entidad) %>%
  summarize(
    prop_caf = survey_mean(atencion_caf, vartype = "ci", na.rm = TRUE),
    n_muestral = unweighted(n())
  ) %>%
  filter(!is.na(nombre_entidad))

# 5. Gráfica de barras horizontales ordenada
ggplot(tabla_entidades, aes(x = prop_caf, y = fct_reorder(nombre_entidad, prop_caf))) +
  geom_col(fill = "#2a9d8f", alpha = 0.85, width = 0.7) +
  geom_errorbar(
    aes(xmin = prop_caf_low, xmax = prop_caf_upp),
    width = 0.3,
    color = "#264653"
  ) +
  geom_text(
    aes(label = percent(prop_caf, accuracy = 0.1)),
    hjust = -0.2,
    size = 3.2,
    color = "black"
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 5),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Uso de Consultorios Adyacentes a Farmacias (CAF) por Entidad Federativa",
    subtitle = "Porcentaje de personas que se atendieron en CAF respecto al total de utilizadores (ENSANUT 2025)",
    x = "Porcentaje de atención en consultorios de farmacia",
    y = NULL,
    caption = "Fuente: ENSANUT Continua 2025. Pregunta 2.1 (código 12). Estimaciones ponderadas con IC del 95%."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "#4a4e69")
  )

df_integrantes <- integrantes %>%
  mutate(
    # Si viene como dummy h0310_10 = 1 (Ninguno), o variable categórica h0310 = 10
    sin_afiliacion = case_when(
      !is.na(h0310_10) & h0310_10 == 1 ~ 1,
      TRUE                              ~ 0
    )
  ) %>%
  # Seleccionar identificadores únicos y variable de no afiliación
  select(mi_upm, estrato,  FOLIO_INT, sin_afiliacion, ponde_f)

df_utilizadores_join <- usuarios %>%
  # Excluir no respuestas de lugar de atención
  filter(!is.na(u0201), !u0201 %in% c(23, 99)) %>%
  mutate(
    atencion_caf = ifelse(u0201 == 12, 1, 0)
  ) %>%
  left_join(
    df_integrantes,
    by = c( "mi_upm", "estrato",  "FOLIO_INT")
  )

diseno_utilizadores <- df_utilizadores_join %>%
  as_survey_design(
    ids = mi_upm,
    strata = estrato,
    weights = ponde_f.x,
    nest = TRUE
  )

tabla_estimacion <- diseno_utilizadores %>%
  # Comparar personas SIN afiliación vs CON afiliación
  mutate(
    grupo_afiliacion = ifelse(sin_afiliacion == 1, "Sin servicio / afiliación médica", "Con derechohabiencia / afiliación")
  ) %>%
  group_by(grupo_afiliacion) %>%
  summarize(
    pct_caf    = survey_mean(atencion_caf, vartype = "ci", na.rm = TRUE),
    n_muestral = unweighted(n())
  )

# Ver resultados en consola
print(tabla_estimacion)

ggplot(tabla_estimacion, aes(x = grupo_afiliacion, y = pct_caf, fill = grupo_afiliacion)) +
  geom_col(width = 0.55, alpha = 0.9, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = pct_caf_low, ymax = pct_caf_upp),
    width = 0.18,
    color = "#1f2421",
    linewidth = 0.7
  ) +
  geom_text(
    aes(label = percent(pct_caf, accuracy = 0.1)),
    vjust = -1.2,
    size = 4.2,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 5),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(values = c("Sin servicio / afiliación médica" = "#e76f51", 
                               "Con derechohabiencia / afiliación" = "#2a9d8f")) +
  labs(
    title = "Porcentaje de atención en Consultorios Adyacentes a Farmacias (CAF)",
    subtitle = "Comparación según condición de derechohabiencia reportada en Hogar (ENSANUT 2025)",
    x = NULL,
    y = "Porcentaje que se atendió en CAF",
    caption = "Fuente: ENSANUT Continua 2025. Cruce de Hogar (H0310) y Utilizadores (U0201). Estimaciones con IC del 95%."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 13)
  )
