library(tidyverse)
library(srvyr)
library(survey)
library(mitools)

library(haven)
# 1. Cargar y filtrar los microdatos para México
# Se asume que el marco de datos 'pisa2025_data' ya está cargado en el entorno
pisa2022_data <- read_sav("PISA/CY08MSP_STU_QQQ.SAV")

pisa_mex <- pisa2022_data %>% 
  filter(CNT == "MEX") %>%
  select(
    CNTSTUID,              # ID Internacional del estudiante
    CNTSCHID,              # ID Internacional de la escuela
    ESCS,                  # Índice Socioeconómico y Cultural
    W_FSTUWT,              # Peso final del estudiante
    starts_with("W_FSTURWT"), # Los 80 pesos replicados (BRR-Fay)
    starts_with("PV")      # Todos los Valores Plausibles (MATH, READ, SCIE)
  )

pisa_design <- pisa_mex %>%
  as_survey_rep(
    weights = W_FSTUWT,
    repweights = starts_with("W_FSTURWT"), 
    type = "Fay",
    rho = 0.5
  )

pv_math <- paste0("PV", 1:10, "MATH")
pv_read <- paste0("PV", 1:10, "READ")
pv_scie <- paste0("PV", 1:10, "SCIE")

calcular_promedio_pisa <- function(pv_vars, design) {
  medias <- lapply(pv_vars, function(pv) {
    fórmula <- as.formula(paste("~", pv))
    svymean(fórmula, design, na.rm = TRUE) # na.rm por si hay valores faltantes
  })
  MIcombine(medias)
}

promedio_math <- calcular_promedio_pisa(pv_math, pisa_design)
promedio_read <- calcular_promedio_pisa(pv_read, pisa_design)
promedio_scie <- calcular_promedio_pisa(pv_scie, pisa_design)

extraer_resumen <- function(modelo_mi, nombre_materia) {
  resumen <- summary(modelo_mi)
  tibble(
    Materia = nombre_materia,
    Puntaje_Promedio = round(resumen$results, 2),
    Error_Estandar = round(resumen$se, 2)
  )
}

resultados_promedios22 <- bind_rows(
  extraer_resumen(promedio_math, "Matemáticas"),
  extraer_resumen(promedio_read, "Lectura"),
  extraer_resumen(promedio_scie, "Ciencias")
)


print(resultados_promedios22)

resultados_promedios22 <- resultados_promedios22 |> 
  mutate(Año = 2022)

gt::gt(resultados_promedios)

resultados_grafica <- resultados_promedios %>%
  mutate(
    # Z = 1.96 para un nivel de confianza del 95%
    IC_Inferior = Puntaje_Promedio - (1.96 * Error_Estandar),
    IC_Superior = Puntaje_Promedio + (1.96 * Error_Estandar)
  )


# 2. Construir el gráfico con ggplot2
grafica_pisa <- ggplot(resultados_grafica, aes(x = Materia, y = Puntaje_Promedio, fill = Materia)) +
  # Crear las barras
  geom_col(width = 0.5, alpha = 0.85) +
  
  # Añadir las barras de error (intervalos de confianza)
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior), 
                width = 0.15, color = "#555555", size = 0.8) +
  
  # Añadir las etiquetas de texto con el puntaje exacto sobre cada barra
  geom_text(aes(label = sprintf("%.1f", Puntaje_Promedio)), 
            vjust = -2.5, size = 4.5, fontface = "bold") +
  
  # Ajustar el eje Y. Los puntajes de PISA suelen visualizarse mejor 
  # recortando el eje para apreciar las diferencias (ej. entre 350 y 500)
  coord_cartesian(ylim = c(350, 500)) +
  
  # Paleta de colores sobria
  scale_fill_manual(values = c(
    "Matemáticas" = "#4E79A7", 
    "Lectura" = "#F28E2B", 
    "Ciencias" = "#59A14F"
  )) +
  
  # Aplicar un tema minimalista y personalizar textos
  theme_minimal(base_size = 12) +
  labs(
    title = "Resultados Nacionales PISA",
    subtitle = "Puntaje promedio por dominio evaluado (con intervalos de confianza al 95%)",
    x = "Dominio de Evaluación",
    y = "Puntaje Promedio",
    caption = "Elaboración propia con microdatos de PISA."
  ) +
  theme(
    legend.position = "none", # Ocultar leyenda ya que el eje X tiene la materia
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "#444444", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_blank() # Quitar líneas verticales del fondo
  )

# 3. Visualizar el gráfico
print(grafica_pisa)


###Efecto del capital economico 
modelos_escs_math <- lapply(pv_math, function(pv) {
  fórmula <- as.formula(paste(pv, "~ ESCS"))
  svyglm(fórmula, design = pisa_design)
})

# Combinar la varianza de imputación y muestral
resultados_escs_math <- MIcombine(modelos_escs_math)

print("--- Efecto del ESCS sobre Matemáticas ---")
summary(resultados_escs_math)


modelos_escs_read <- lapply(pv_read, function(pv) {
  fórmula <- as.formula(paste(pv, "~ ESCS"))
  svyglm(fórmula, design = pisa_design)
})

# Combinar la varianza de imputación y muestral
resultados_escs_read <- MIcombine(modelos_escs_read)


summary(resultados_escs_read)



modelos_escs_scie <- lapply(pv_scie, function(pv) {
  fórmula <- as.formula(paste(pv, "~ ESCS"))
  svyglm(fórmula, design = pisa_design)
})

# Combinar la varianza de imputación y muestral
resultados_escs_scie <- MIcombine(modelos_escs_scie)


summary(resultados_escs_scie)

