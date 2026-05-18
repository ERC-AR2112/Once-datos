###Día del museo
library(foreign)
library(tidyverse)
base_museos <- read.dbf("Día_Museo/Tablas de microdatos/museos25.dbf")
base_visita <- read.dbf("Día_Museo/Tablas de microdatos/visita25.dbf")
base_museos %>% 
  glimpse()


base_visita %>% glimpse()

# Piramide de vistantes por sexo

visita_piramide <- base_visita %>% 
  filter(EDAD < 99, SEXO %in% c(1, 2)) %>% 
  mutate(
    Grupo_Edad = cut(EDAD, breaks = seq(10, 100, by = 5), right = FALSE),
    Sexo_cat = case_when(SEXO == 1 ~ "Hombres", SEXO == 2 ~ "Mujeres")
  ) %>% 
  count(Grupo_Edad, Sexo_cat) |>
  mutate(n = if_else(Sexo_cat == "Hombres", -n, n)) %>% 
  drop_na(Grupo_Edad)

visita_piramide %>% 
ggplot(aes(x = Grupo_Edad, y = n, fill = Sexo_cat)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs) + 
  scale_fill_manual(values = c("Hombres" = "#0072B2", "Mujeres" = "red3")) +
  theme_minimal() +
  labs(
    title = "Pirámide Poblacional de las Personas Visitantes",
    subtitle = "Día Internacional de los Museos",
    x = "Grupo de Edad", 
    y = "Volumen de Asistencia", 
    fill = "Género"
  )

# Gráfica motivaciones
motivaciones <- base_visita %>% 
  select(starts_with("MV_")) %>% 
  # Sumar menciones donde el valor es 1 (Con motivo)
  summarise(across(everything(), ~sum(.x == 1, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Motivo", values_to = "Frecuencia") |>
  mutate(Motivo = recode(Motivo,
                         "MV_ACOMP"   = "ACOMPAÑAR A ALGUIEN",
                         "MV_CULTURA" = "CULTURA GENERAL",
                         "MV_APREND"  = "APRENDER",
                         "MV_ESCOLAR" = "MOTIVOS ESCOLARES",
                         "MV_LABORAL" = "MOTIVOS LABORALES",
                         "MV_CONOCER" = "CONOCER LA EXPOSICIÓN",
                         "MV_ENTRETE" = "ENTRETENIMIENTO",
                         "MV_EDIFICI" = "VER EL EDIFICIO",
                         "MV_TALLER"  = "TALLERES O CURSOS",
                         "MV_OTRO"    = "OTROS MOTIVOS"
  )) %>% 
  arrange(Frecuencia) %>% 
  mutate(Motivo = factor(Motivo, levels = Motivo))
motivaciones %>% 
ggplot(aes(x = Frecuencia, y = Motivo)) +
  geom_segment(aes(x = 0, xend = Frecuencia, y = Motivo, yend = Motivo), color = "gray60") +
  geom_point(size = 4, color = "#009E73") +
  theme_minimal() +
  labs(
    title = "Principales Motivaciones de Visita",
    x = "Frecuencia de mención", 
    y = NULL
  )


# Gráfica inclusión
accesibilidad <- base_museos %>% 
  select(ACC_AUDIT, ACC_MOTRIZ, ACC_COGNI, ACC_VISUAL) |>
  summarise(across(everything(), ~mean(.x == 1, na.rm = TRUE) * 100)) %>% 
  pivot_longer(everything(), names_to = "Tipo", values_to = "Porcentaje") %>% 
  mutate(Tipo = case_when(
    Tipo == "ACC_AUDIT" ~ "Auditiva",
    Tipo == "ACC_MOTRIZ" ~ "Motriz",
    Tipo == "ACC_COGNI" ~ "Cognitiva",
    Tipo == "ACC_VISUAL" ~ "Visual"
  ))

ggplot(accesibilidad, aes(x = reorder(Tipo, -Porcentaje), y = Porcentaje, fill = Tipo)) +
  geom_col(show.legend = FALSE, width = 1, color = "white") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Equidad en la Infraestructura",
    subtitle = "% de recintos con accesibilidad específica",
    x = NULL, 
    y = NULL
  )

## Gráfica de apertura de museos
base_museos %>% count(as.numeric(A_APERTURA))

base_museos %>% 
  ggplot(aes(x = A_APERTURA)) +
  geom_bar(fill = "#CC79A7", color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(
    title = "Crecimiento de la Infraestructura Museística",
    subtitle = "Aperturas registradas por quinquenio",
    x = "Año de Apertura", 
    y = "Número de Recintos"
  )
  
base_museos %>%  
  filter(A_APERTURA == "1785") %>% 
  select(NOM_MUSEO, ENTIDAD, A_APERTURA)

##Gráficas por tematica del museo
  tematicas <- base_museos %>% 
    count(TEMA_PRINC) %>% 
    drop_na(TEMA_PRINC) %>% 
    mutate(
      Tematica = case_when(
        TEMA_PRINC == "1" ~ "Arqueología",
        TEMA_PRINC == "2" ~ "Arte",
        TEMA_PRINC == "3" ~ "Paleontología",
        TEMA_PRINC == "4" ~ "Historia",
        TEMA_PRINC == "5" ~ "Industria",
        TEMA_PRINC == "6" ~ "Ciencia",
        TEMA_PRINC == "7" ~ "Tecnología",
        TEMA_PRINC == "8" ~ "Ambiental/Ecología",
        TRUE ~ "No especificado" 
      )
    ) %>% 
    filter(Tematica != "No especificado") %>% 
    mutate(Tematica = fct_reorder(Tematica, n))
  
  ggplot(tematicas, aes(x = n, y = Tematica)) +
    geom_col(fill = "#CC79A7", width = 0.7) +
    geom_text(aes(label = n), hjust = -0.2, size = 3.5, fontface = "bold", color = "black") +
    theme_minimal() +
    labs(
      title = "Vocación de las Instituciones Museísticas en México",
      subtitle = "Distribución por temática principal",
      x = "Número de recintos",
      y = NULL
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10, color = "black")
    )
