# Cargar la librería
library(tidyverse)

# 1. Crear el data frame con los ingresos estimados
datos_taquilla <- data.frame(
  franquicia = c("Marvel Cinematic Universe", "Spider-Man", "Batman", 
                 "Universo Extendido DC", "X-Men"),
  recaudacion = c(34.7, 13.3, 6.8, 6.5, 6.0)
)

# 2. Generar la visualización
grafico_superheroes <- datos_taquilla %>%
  # Ordenar la variable categórica según la recaudación (de mayor a menor)
  mutate(franquicia = fct_reorder(franquicia, recaudacion)) %>%
  ggplot(aes(x = recaudacion, y = franquicia, fill = franquicia)) +
  # Crear las barras sin mostrar la leyenda (redundante por los nombres en el eje Y)
  geom_col(show.legend = FALSE, alpha = 0.85) +
  # Añadir las etiquetas de texto al final de cada barra
  geom_text(aes(label = paste0("$", recaudacion, " mil M")), 
            hjust = -0.15, size = 4.5, color = "black", fontface = "bold") +
  # Usar una paleta de colores profesional
  scale_fill_brewer(palette = "Set2") +
  # Ajustar etiquetas y títulos
  labs(
    title = "Comparativa de Taquilla Global por Franquicia de Superhéroes",
    subtitle = "Recaudación total estimada en miles de millones de dólares",
    x = "Recaudación (Miles de millones de USD)",
    y = NULL,
    caption = "Fuente: Estimaciones propias con base en The Numbers y Box Office Mojo"
  ) +
  # Aplicar un tema minimalista y personalizar elementos
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(), # Quitar líneas horizontales de fondo
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(face = "bold")
  ) +
  # Expandir el límite X para que las etiquetas de texto no se corten
  coord_cartesian(xlim = c(0, 42))

# 3. Mostrar el gráfico
print(grafico_superheroes)

######
install.packages("patchwork")
library(tidyverse)
library(patchwork) # Para ensamblar múltiples gráficos

# 1. Preparar los datos
df_licencias <- data.frame(
  categoria = "Licencias Globales\n(Todas las marcas)",
  valor = 307000,
  etiqueta = "$307,000 mil M"
)

df_turismo <- data.frame(
  categoria = "Comic-Con San Diego\n(Impacto Anual)",
  valor = 160,
  etiqueta = "$160 M"
)

# 2. Construir el panel de Licencias (Merchandising)
grafico_licencias <- ggplot(df_licencias, aes(x = categoria, y = valor)) +
  geom_col(fill = "#2c3e50", width = 0.4, alpha = 0.9) +
  geom_text(aes(label = etiqueta), vjust = -1, size = 5, fontface = "bold", color = "#2c3e50") +
  scale_y_continuous(limits = c(0, 360000), expand = c(0,0)) +
  labs(
    title = "El Monopolio del Merchandising",
    subtitle = "Ventas minoristas (Crecimiento interanual del 10%)",
    y = "Millones de USD", x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(), # Se oculta el eje Y para limpiar el diseño
    axis.ticks.y = element_blank()
  )

# 3. Construir el panel de Turismo (Eventos)
grafico_turismo <- ggplot(df_turismo, aes(x = categoria, y = valor)) +
  geom_col(fill = "#e74c3c", width = 0.4, alpha = 0.9) +
  geom_text(aes(label = etiqueta), vjust = -1, size = 5, fontface = "bold", color = "#e74c3c") +
  scale_y_continuous(limits = c(0, 200), expand = c(0,0)) +
  labs(
    title = "El Impacto del Turismo Geek",
    subtitle = "Basado en la atracción de 553,000 visitantes anuales",
    y = NULL, x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# 4. Ensamblar el gráfico final con patchwork
grafico_compuesto <- grafico_licencias + grafico_turismo + 
  plot_annotation(
    title = "Más Allá de la Taquilla: El Ecosistema Económico de los Superhéroes",
    caption = "Fuentes: Daswani (2025) | Jennewein (2026)",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 10, color = "gray40")
    )
  )

# 5. Mostrar la visualización
print(grafico_compuesto)
