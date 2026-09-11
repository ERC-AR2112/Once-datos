## suicidios

library(pacman)
p_load(tidyverse, gt, foreign)

lista_causas<-read.dbf("Prevencion_suicidio/LISTA1.dbf")



df_mortalidad <- read.dbf("Prevencion_suicidio/DEFUN24.dbf")
df_mortalidad |> glimpse()

#Codigo de suicidio 101

df_suicidio <- df_mortalidad |> 
  filter(LISTA1 == 101) |> 
  mutate(sexo_etiqueta = case_when(SEXO ==1 ~ "Hombre",
                                   SEXO ==2 ~ "Mujer",
                                   TRUE ~ "No especificado"))
resumen_casos <- df_suicidio |>
  count(sexo_etiqueta,  name = "total_defunciones") |>
  arrange(desc(total_defunciones))

resumen_casos |>
  gt() |>
  tab_header(
    title = "Defunciones por lesiones autoinfligidas",
    subtitle = "Estadísticas de Defunciones Registradas (EDR) - INEGI 2024"
  ) |>
  cols_label(
    sexo_etiqueta = "Sexo",
    total_defunciones = "Total de Casos"
  ) |>
  fmt_number(
    columns = total_defunciones,
    decimals = 0
  ) |>
  opt_stylize(style = 1, color = "red")

df_perfil_suicidios <- df_suicidio |>
  mutate(
    sexo_cat = case_match(
      SEXO,
      1 ~ "Hombre", 
      2 ~ "Mujer", 
      9 ~ "No especificado"
    ),
    estado_civil_cat = case_match(
      EDO_CIVIL,
      1 ~ "Soltero(a)", 
      2 ~ "Divorciado(a)", 
      3 ~ "Viudo(a)", 
      4 ~ "Unión libre", 
      5 ~ "Casado(a)", 
      6 ~ "Separado(a)", 
      .default = "No especificado/No aplica"
    ),
    escolaridad_cat = case_match(
      ESCOLARIDA,
      1 ~ "Sin escolaridad",
      2:4 ~ "Primaria (Incompleta/Completa)",
      5:6 ~ "Secundaria (Incompleta/Completa)",
      7:8 ~ "Media Superior (Incompleta/Completa)",
      9 ~ "Profesional",
      10 ~ "Posgrado",
      .default = "No especificado"
    ),
    tenia_empleo = case_match(
      COND_ACT,
      1 ~ "Sí (Trabajaba)",
      2 ~ "No (No trabajaba)",
      .default = "Se ignora/No aplica"
    )
  )
df_perfil_suicidios |>
  count(sexo_cat, tenia_empleo, name = "total_defunciones") |>
  mutate(porcentaje = (total_defunciones / sum(total_defunciones)) * 100) |>
  ggplot(aes(x = tenia_empleo, y = porcentaje, fill = sexo_cat)) +
  geom_col( )+
  theme_bw()+ 
  labs(title = "Porcentaje de casos de suicidio en función a la situación laboral de la persona fallecida",
       x="Empleo",
       y = "Porcentaje",
       fill = "Sexo",
       caption = "Elaboración propia con base en Estadísticas de Defunciones Registradas (EDR) - INEGI 2024")



df_perfil_suicidios |>
  count(escolaridad_cat, name = "total_defunciones") |>
  mutate(porcentaje = (total_defunciones / sum(total_defunciones)) * 100) |>
  ggplot(aes(x =  escolaridad_cat, y = porcentaje)) +
  geom_col( fill = "#750946" )+
  theme_bw()+ 
  labs(title = "Porcentaje de casos de suicidio en función al grado de estudios de la persona fallecida",
       x="Nivel educativo",
       y = "Porcentaje",
       caption = "Elaboración propia con base en Estadísticas de Defunciones Registradas (EDR) - INEGI 2024")

df_perfil_suicidios |>
  count(sexo_cat, estado_civil_cat, name = "total_defunciones") |>
  mutate(porcentaje = (total_defunciones / sum(total_defunciones)) * 100) |>
  ggplot(aes(x =  estado_civil_cat, y = porcentaje)) +
  geom_col( fill = "#750946" )+
  theme_bw()+ 
  labs(title = "Porcentaje de casos de suicidio por estado civil de la persona fallecida",
       x="Estado civil",
       y = "Porcentaje",
       caption = "Elaboración propia con base en Estadísticas de Defunciones Registradas (EDR) - INEGI 2024")


