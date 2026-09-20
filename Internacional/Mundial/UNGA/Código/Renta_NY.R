## Rentas en New York

library(tidyverse)
library(scales)


renta_data <- tibble(
  categoria = c(
    "Mediana nacional (ref.)",
    "1 recámara (NYC)",
    "General (NYC)",
    "2 recámaras (NYC)",
    "Manhattan (NYC)"
  ),
  renta = c(
    round(2585 / 1.862), # Derivado del +86.2% sobre el nivel nacional
    2467,
    2585,
    2602,
    3281
  ),
  tipo = c("Referencia", "NYC", "NYC General", "NYC", "Manhattan")
) %>%
  mutate(categoria = fct_reorder(categoria, renta))


colores <- c(
  "Referencia"  = "#94a3b8", # Gris neutro
  "NYC"         = "#38bdf8", # Azul claro
  "NYC General" = "#0284c7", # Azul institucional destacado
  "Manhattan"   = "#0f172a"  # Azul oscuro / contraste fuerte
)


p <- ggplot(renta_data, aes(x = renta, y = categoria, fill = tipo)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  # Etiquetas de valor directas en las barras
  geom_text(
    aes(label = dollar(renta, prefix = "$", big.mark = ",")),
    hjust = -0.15,
    size = 4,
    fontface = "bold",
    color = "#1e293b"
  ) +
  scale_fill_manual(values = colores) +
  scale_x_continuous(
    labels = dollar_format(prefix = "$", big.mark = ","),
    expand = expansion(mult = c(0, 0.18)), # Espacio para que no se corten las etiquetas
    limits = c(0, 3600)
  ) +
  labs(
    title = "Mediana de renta en Nueva York y comparación nacional",
    subtitle = "NYC se ubica en el 6.° lugar nacional | +0.9% en el último mes y +3.4% anual (NYC general)",
    caption = "Fuente: Apartment List | *Mediana nacional calculada a partir de la brecha del 86.2%",
    x = "Renta mediana mensual (USD)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#0f172a", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10.5, color = "#475569", margin = margin(b = 16)),
    plot.caption = element_text(size = 8.5, color = "#94a3b8", hjust = 0, margin = margin(t = 12)),
    axis.text.y = element_text(face = "bold", color = "#1e293b", size = 11),
    axis.text.x = element_text(color = "#64748b"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#e2e8f0", linewidth = 0.5),
    plot.margin = margin(t = 16, r = 16, b = 16, l = 16)
  )

print(p)

library(patchwork)

df_fuentes <- tibble(
  umbral = factor(c("Carga de renta (≥ 30% del ingreso)", "Carga severa (≥ 50% del ingreso)")),
    levels = c("Carga severa (≥ 50% del ingreso)", "Carga de renta (≥ 30% del ingreso)"),
  fuente = factor( c("RGB (2026) / TIME"),
    levels = c("RGB (2026) / TIME")
  ),
  valor = c(0.51, 0.29), # Representación numérica de >50%, >50%, ~30% y ~25%
  etiqueta = c("> 50%", "Casi 30%")
)

# Panel B: Matiz de Furman Center (Hogar con $60,000 USD anuales)
df_matiz <- tibble(
  base_ingreso = factor(
    c("Ingreso antes de impuestos\n(Bruto: $60,000 USD)", 
      "Ingreso disponible\n(Neto tras impuestos: $45,000 USD)"),
    levels = c("Ingreso disponible\n(Neto tras impuestos: $45,000 USD)", 
               "Ingreso antes de impuestos\n(Bruto: $60,000 USD)")
  ),
  porcentaje = c(0.30, 0.40),
  monto_renta = c(18000, 18000),
  tipo = c("Estándar oficial", "Impacto real")
)

p1 <- ggplot(df_fuentes, aes(x = valor, y = umbral)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = etiqueta),
    position = position_dodge(width = 0.7),
    hjust = -0.15,
    size = 3.6,
    fontface = "bold",
    color = "#1e293b"
  ) +
  scale_fill_manual(values = c(
    "RGB (2026) / TIME" = "#1e3a8a",          # Azul marino
    "Furman Center / HVS (2023)" = "#0284c7"   # Azul cerúleo
  )) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.65),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "A. Prevalencia de sobrecosto: Comparación de estudios",
    subtitle = "Proporción de hogares inquilinos según RGB (2026)",
    x = "Porcentaje de inquilinos",
    y = NULL,
    fill = "Fuente"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#0f172a"),
    plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", color = "#1e293b", size = 9.5),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#f1f5f9")
  )



p2 <- ggplot(df_matiz, aes(x = porcentaje, y = base_ingreso, fill = tipo)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_vline(xintercept = 0.30, linetype = "dashed", color = "#64748b", linewidth = 0.7) +
  geom_text(
    aes(label = percent(porcentaje, accuracy = 1)),
    hjust = -0.2,
    size = 4,
    fontface = "bold",
    color = "#1e293b"
  ) +
  scale_fill_manual(values = c(
    "Estándar oficial" = "#94a3b8",  # Gris neutral
    "Impacto real"     = "#dc2626"   # Rojo advertencia
  )) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.50),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "B. El matiz impositivo: Salario Bruto vs. Salario Neto",
    subtitle = "Hogar con $60,000 USD anuales pagando $18,000 USD en renta ($1,500/mes)",
    x = "Proporción del salario destinado al alquiler",
    y = NULL
  ) +
  annotate(
    "text",
    x = 0.31, y = 1.6,
    label = "Umbral estándar (30%)",
    hjust = 0, size = 3.2, color = "#475569", fontface = "italic"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#0f172a"),
    plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold", color = "#1e293b", size = 9.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#f1f5f9")
  )



grafica_final <- (p1 / p2) +
  plot_layout(heights = c(1.2, 0.9)) +
  plot_annotation(
    title = "Inseguridad habitacional y presión fiscal en inquilinos de Nueva York",
    caption = "Fuentes: Rent Guidelines Board (Income and Affordability Study 2026, vía TIME) y NYU Furman Center (HVS 2023).\n*El cálculo neto refleja el descuento de impuestos federales, estatales y locales de NYC.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, color = "#0f172a", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#64748b", hjust = 0, margin = margin(t = 12)),
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    )
  )

print(grafica_final)
