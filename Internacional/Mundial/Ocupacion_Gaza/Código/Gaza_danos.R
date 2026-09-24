

library(pacman)

p_load(tidyverse, scales, patchwork)


# 1. Datos


# Evaluaciones de UNOSAT (Hechas a mano con datos de ONUSAT)
gaza <- tribble(
  ~fecha,        ~destruido, ~grave, ~moderado, ~posible, ~pct_danado,
  "2024-07-06",       46223,  18478,     55954,    35754,          63,
  "2024-09-06",       52564,  18913,     56710,    35591,          66,
  "2024-12-01",       60368,  20050,     56292,    34102,          69,
  "2025-10-11",      123464,  17116,     33857,    23836,          81
) |>
  mutate(fecha = as.Date(fecha),
         total = destruido + grave + moderado + posible)

# Actualización de agosto 2026: solo se publicó el resumen

gaza_pct <- gaza |>
  select(fecha, pct_danado) |>
  mutate(tipo = "Evaluación completa") |>
  add_row(fecha = as.Date("2026-08-13"), pct_danado = 82, tipo = "Aproximado")

# Estructuras afectadas por gobernación (UNOSAT, 11 oct 2025)
gobernaciones <- tribble(
  ~gobernacion,     ~estructuras,
  "Khan Yunis",            51885,
  "Gaza",                  51530,
  "Gaza Norte",            41454,
  "Rafah",                 35307,
  "Deir al-Balah",         18097
)

# Cisjordania y Jerusalén Este: enero-abril, 2025 vs 2026
cisjordania <- tribble(
  ~anio,  ~indicador,                ~valor,
  "2025", "Estructuras demolidas",      662,
  "2026", "Estructuras demolidas",      447,
  "2025", "Personas desplazadas",       888,
  "2026", "Personas desplazadas",       776
)


tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "grey45", hjust = 0),
        panel.grid.minor = element_blank(),
        legend.position = "top")

colores_dano <- c("Destruido"          = "#7f0000",
                  "Gravemente dañado"  = "#d7301f",
                  "Moderadamente dañado" = "#fc8d59",
                  "Posiblemente dañado"  = "#fdcc8a")


# Estructuras afectadas por nivel de daño


gaza_largo <- gaza |>
  pivot_longer(destruido:posible, names_to = "nivel", values_to = "n") |>
  mutate(nivel = recode(nivel,
                        destruido = "Destruido",
                        grave     = "Gravemente dañado",
                        moderado  = "Moderadamente dañado",
                        posible   = "Posiblemente dañado"),
         nivel = factor(nivel, levels = rev(names(colores_dano))),
         etiqueta = format(fecha, "%b %Y"))

g1 <- ggplot(gaza_largo, aes(x = reorder(etiqueta, fecha), y = n, fill = nivel)) +
  geom_col(width = 0.7) +
  geom_text(data = gaza, inherit.aes = FALSE,
            aes(x = format(fecha, "%b %Y"), y = total,
                label = number(total, big.mark = ".", decimal.mark = ",")),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = colores_dano, breaks = names(colores_dano)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Gaza: estructuras afectadas según nivel de daño",
       subtitle = "Los edificios destruidos pasan de ~46.000 a ~123.000 entre 2024 y 2025",
       x = NULL, y = "Estructuras", fill = NULL,
       caption = "Fuente: UNOSAT (evaluaciones por satélite, preliminares).") +
  tema
g1
ggsave( "Gaza/Graficas/afectadas.png", g1, width = 16, height = 11, dpi = 200, bg = "white")

g2 <- ggplot(gaza_pct, aes(fecha, pct_danado)) +
  geom_line(colour = "#7f0000", linewidth = 1) +
  geom_point(aes(shape = tipo), size = 3, colour = "#7f0000") +
  geom_text(aes(label = paste0(pct_danado, "%")), vjust = -1, size = 3.5) +
  annotate("segment", x = as.Date("2025-10-10"), xend = as.Date("2025-10-10"),
           y = 55, yend = 90, linetype = "dashed", colour = "grey50") +
  annotate("text", x = as.Date("2025-10-10"), y = 57, label = "Alto el fuego",
           hjust = 1.05, size = 3.3, colour = "grey30") +
  scale_shape_manual(values = c("Evaluación completa" = 16, "Aproximado" = 1)) +
  scale_y_continuous(limits = c(55, 90), labels = label_percent(scale = 1)) +
  scale_x_date(date_labels = "%b %Y") +
  labs(title = "Gaza: porcentaje de estructuras dañadas",
       x = NULL, y = NULL, shape = NULL,
       caption = "Fuente: UNOSAT; ago. 2026 según UNRWA/ONU Noticias (aprox.).\nLa línea une solo las evaluaciones incluidas; hubo otras intermedias en 2025.") +
  tema
g2

ggsave("Gaza/Graficas/evol_estructuradan.png", g2, width = 16, height = 11, dpi = 200, bg = "white")

# Estructuras afectadas por gobernación


g3 <- ggplot(gobernaciones,
             aes(x = estructuras, y = reorder(gobernacion, estructuras))) +
  geom_col(fill = "#d7301f", width = 0.65) +
  geom_text(aes(label = number(estructuras, big.mark = ".", decimal.mark = ",")),
            hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Gaza: estructuras afectadas por gobernación",
       subtitle = "Octubre de 2025 (total: 198.273)",
       x = NULL, y = NULL,
       caption = "Fuente: UNOSAT, imágenes del 11 oct 2025.") +
  tema


# Cisjordania, enero-abril 2025 vs 2026


g4 <- ggplot(cisjordania, aes(x = anio, y = valor, fill = anio)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = valor), vjust = -0.4, size = 3.5) +
  facet_wrap(~ indicador) +
  scale_fill_manual(values = c("2025" = "grey60", "2026" = "#2b6c8f")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Cisjordania y Jerusalén Este: demoliciones (enero-abril)",
       x = NULL, y = NULL,
       caption = "Fuente: Good Shepherd Collective a partir de datos de OCHA (hasta 28 abr).") +
  tema

g4
ggsave("Gaza/Graficas/daños_palestina.png", g4, width = 16, height = 11, dpi = 200, bg = "white")

#Gráficas juntas

panel <- (g1 | g2) / (g3 | g4)
print(panel)

ggsave("destruccion_gaza_cisjordania.png", panel,
       width = 16, height = 11, dpi = 200, bg = "white")

