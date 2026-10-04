=============================================================================
  # ENG 2025 (Encuesta Nacional de Gasto) - Gasto en alimentacion y salud
  # Cortes: sexo de la jefatura del hogar x ambito rural/urbano
  #
  # Replica del analisis hecho sobre ENIGH 2024, adaptado al diseno de la ENG.
  # Insumo: concentradohogar.csv de los microdatos de la ENG 2025 (INEGI).
  #
  # ADVERTENCIA DE COMPARABILIDAD
  # -----------------------------
# La ENG 2025 NO es una nueva edicion de la ENIGH y sus niveles no forman
# serie con ella. Son instrumentos distintos:
#   - Proposito: la ENG existe para actualizar los ponderadores del INPC;
#     la ENIGH, para medir ingreso, gasto y bienestar.
#   - Captacion: la ENG levanta los doce meses del ano e incluye un
#     Cuadernillo de Gastos Diarios que recoge compras pequenas y frecuentes
#     que un recordatorio trimestral tiende a omitir.
#   - Ingreso: la ENG lo capta de forma mucho mas breve que la ENIGH
#     (ing_cor = trabajo + transferencias gubernamentales + remesas + otros
#     + no monetario). Las proporciones del ingreso son por tanto el
#     indicador MENOS comparable entre ambas encuestas.
# Consecuencia practica: la diferencia de 68% en el gasto en salud entre
# ENIGH 2024 y ENG 2025 es en su mayor parte instrumento, no cambio real.
# Las dos encuestas se reportan por separado, nunca como una serie.
#
# PERIODICIDAD Y PONDERADORES
# ---------------------------
# Los montos de concentradohogar son valores TRIMESTRALES, "independientemente
# del periodo de referencia y de captacion" (manual, seccion 1.2.1), asi que
# son directamente comparables con las cifras trimestrales de la ENIGH.
#

library(survey)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

options(survey.lonely.psu = "adjust")

ruta <- "ENG/concentradohogar.csv"   # <- ajusta la ruta (ENG 2025)


h <- read.csv(ruta, stringsAsFactors = FALSE) %>%
  mutate(
    # tam_loc 1-3 = urbano (2 500 y mas habitantes); 4 = rural. Igual que ENIGH.
    ambito = factor(if_else(tam_loc == 4, "Rural", "Urbano"),
                    levels = c("Urbano", "Rural")),
    sexo   = factor(if_else(sexo_jefe == 1, "Masculina", "Femenina"),
                    levels = c("Masculina", "Femenina")),
    grupo  = interaction(sexo, ambito, sep = " / "),
    alim_pc  = alimentos / tot_integ,
    salud_pc = salud / tot_integ,
    carga_alim  = if_else(ing_cor > 0, 100 * alimentos / ing_cor, NA_real_),
    carga_salud = if_else(ing_cor > 0, 100 * salud / ing_cor, NA_real_),
    cat10 = as.numeric(carga_salud > 10),
    cat30 = as.numeric(carga_salud > 30),
    gasta_salud = as.numeric(salud > 0)
  )


dis <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                 data = h, nest = TRUE)

# Variante trimestral, si se quiere estacionalidad. No mezclar con la anual.
# dis_t <- svydesign(ids = ~upm_t, strata = ~est_dis_t, weights = ~factor_t,
#                    data = h, nest = TRUE)
# svyby(~salud, ~trimestre, dis_t, svymean)   # el gasto en salud es estacional

# Deciles con cortes PONDERADOS
cortes <- as.numeric(svyquantile(~ing_cor, dis, seq(0.1, 0.9, 0.1),
                                 ci = FALSE)$ing_cor)
h   <- h   %>% mutate(decil = factor(findInterval(ing_cor, cortes) + 1, levels = 1:10))
dis <- update(dis, decil = factor(findInterval(ing_cor, cortes) + 1, levels = 1:10))

cat("Muestra:", nrow(h), "| Hogares expandidos:",
    format(sum(h$factor), big.mark = ","), "\n")
print(svymean(~gasto_mon + alimentos + salud + ing_cor, dis))
print(svyby(~gasto_mon + alimentos + salud, ~ambito, dis, svymean))


por_hogar <- svyby(~alimentos + salud, ~sexo + ambito, dis, svymean,
                   vartype = c("se", "ci"))
print(por_hogar)


per_capita <- lapply(levels(h$grupo), function(g) {
  sub <- subset(dis, grupo == g)
  a <- svyratio(~alimentos, ~tot_integ, sub)
  s <- svyratio(~salud,     ~tot_integ, sub)
  data.frame(grupo = g,
             alim_pc  = coef(a), alim_se  = SE(a),
             salud_pc = coef(s), salud_se = SE(s))
}) %>% bind_rows()
print(per_capita)

# Participacion en el gasto monetario -------------------------------
participacion <- lapply(levels(h$grupo), function(g) {
  sub <- subset(dis, grupo == g)
  a <- svyratio(~alimentos, ~gasto_mon, sub)
  s <- svyratio(~salud,     ~gasto_mon, sub)
  data.frame(grupo = g,
             alim_pct  = 100 * coef(a), alim_se  = 100 * SE(a),
             salud_pct = 100 * coef(s), salud_se = 100 * SE(s))
}) %>% bind_rows()
print(participacion)

# ---- 7. Proporcion del ingreso --------------------------------------------

prop_ing <- lapply(levels(h$grupo), function(g) {
  sub <- subset(dis, grupo == g)
  a <- svyratio(~alimentos, ~ing_cor, sub)
  s <- svyratio(~salud,     ~ing_cor, sub)
  data.frame(grupo = g,
             alim_pct  = 100 * coef(a), alim_se  = 100 * SE(a),
             salud_pct = 100 * coef(s), salud_se = 100 * SE(s))
}) %>% bind_rows()
print(prop_ing)
print(svyratio(~alimentos + salud, ~ing_cor, dis))

dis_ing <- subset(dis, ing_cor > 0)
print(svyby(~carga_alim + carga_salud, ~sexo + ambito, dis_ing,
            svymean, na.rm = TRUE, vartype = c("se", "ci")))
print(svyby(~carga_alim + carga_salud, ~sexo + ambito, dis_ing,
            svyquantile, quantiles = 0.5, ci = TRUE, na.rm = TRUE))

decil_alim  <- svyby(~alimentos, ~decil, dis, svyratio, denominator = ~ing_cor)
decil_salud <- svyby(~salud,     ~decil, dis, svyratio, denominator = ~ing_cor)
print(decil_alim); print(decil_salud)
print(svyby(~ing_cor, ~decil, dis, svymean))

# Incidencia, intensidad y gasto catastrofico -----------------------

print(svyby(~gasta_salud, ~sexo + ambito, dis, svymean))
print(svyby(~salud, ~sexo + ambito, subset(dis, salud > 0), svymean))
print(svyby(~cat10 + cat30, ~sexo + ambito, dis_ing, svymean,
            na.rm = TRUE, vartype = c("se", "ci")))
print(svyby(~cat10, ~decil, dis_ing, svymean, na.rm = TRUE))
print(svyttest(cat10 ~ ambito, dis_ing))

# Pruebas de hipotesis ---------------------------------------------
# Resultado en ENG 2025, igual que en ENIGH 2024: la brecha por sexo es
# significativa en niveles y se anula al normalizar. La unica diferencia
# significativa per capita es alimentos en el ambito rural, y va A FAVOR
# de la jefatura femenina.
for (amb in c("Urbano", "Rural")) {
  sub  <- subset(dis, ambito == amb)
  subi <- subset(dis_ing, ambito == amb)
  cat("\n=======", amb, "=======\n")
  print(svyttest(alimentos ~ sexo, sub))
  print(svyttest(salud ~ sexo, sub))
  print(svyttest(alim_pc ~ sexo, sub))
  print(svyttest(salud_pc ~ sexo, sub))
  print(svyttest(carga_salud ~ sexo, subi))
}
print(summary(svyglm(salud ~ sexo * ambito, design = dis)))

# Graficas ----------------------------------------------------------
pal    <- c("Masculina" = "#2a78d6", "Femenina" = "#eb6834")
tema   <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.x = element_blank(), legend.position = "top")
fuente <- "Fuente: INEGI, ENG 2025, concentradohogar. Factor de expansion anual."

g1 <- por_hogar %>%
  pivot_longer(c(alimentos, salud), names_to = "rubro", values_to = "monto") %>%
  mutate(se = if_else(rubro == "alimentos", se.alimentos, se.salud),
         rubro = recode(rubro, alimentos = "Alimentos",
                        salud     = "Cuidados de la salud")) %>%
  ggplot(aes(ambito, monto, fill = sexo)) +
  geom_col(position = position_dodge(0.75), width = 0.6) +
  geom_errorbar(aes(ymin = monto - 1.96 * se, ymax = monto + 1.96 * se),
                position = position_dodge(0.75), width = 0.12, linewidth = 0.4) +
  geom_text(aes(y = monto + 1.96 * se, label = scales::dollar(round(monto,0))), 
            position = position_dodge(0.75), 
            vjust = -0.5,
            size = 3.5) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = "Jefatura") +
  scale_y_continuous(labels = label_dollar(big.mark = ",")) +
  labs(title = "Gasto trimestral por hogar, ENG 2025",
       subtitle = "Barras de error: intervalo de confianza al 95%",
       x = NULL, y = "Pesos por hogar", caption = fuente) + tema
g1
g1_1 <- por_hogar %>%
  pivot_longer(c(alimentos, salud), names_to = "rubro", values_to = "monto") %>%
  mutate(se = if_else(rubro == "alimentos", se.alimentos, se.salud),
         rubro = recode(rubro, alimentos = "Alimentos",
                        salud     = "Cuidados de la salud")) %>%
  filter(rubro=="Alimentos") |> 
  ggplot(aes(ambito, monto, fill = sexo)) +
  geom_col(position = position_dodge(0.75), width = 0.6) +
  geom_errorbar(aes(ymin = monto - 1.96 * se, ymax = monto + 1.96 * se),
                position = position_dodge(0.75), width = 0.12, linewidth = 0.4) +
  geom_text(aes(y = monto + 1.96 * se, label = scales::dollar(round(monto,0))), 
            position = position_dodge(0.75), 
            vjust = -0.5,
            size = 3.5) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = "Jefatura") +
  scale_y_continuous(labels = label_dollar(big.mark = ",")) +
  labs(title = "Gasto trimestral por hogar, ENG 2025",
       subtitle = "Barras de error: intervalo de confianza al 95%",
       x = NULL, y = "Pesos por hogar", caption = fuente) + tema
g1_1
g1_2 <- por_hogar %>%
  pivot_longer(c(alimentos, salud), names_to = "rubro", values_to = "monto") %>%
  mutate(se = if_else(rubro == "alimentos", se.alimentos, se.salud),
         rubro = recode(rubro, alimentos = "Alimentos",
                        salud     = "Cuidados de la salud")) %>%
  filter(rubro=="Cuidados de la salud") |> 
  ggplot(aes(ambito, monto, fill = sexo)) +
  geom_col(position = position_dodge(0.75), width = 0.6) +
  geom_errorbar(aes(ymin = monto - 1.96 * se, ymax = monto + 1.96 * se),
                position = position_dodge(0.75), width = 0.12, linewidth = 0.4) +
  geom_text(aes(y = monto + 1.96 * se, label = scales::dollar(round(monto,0))), 
            position = position_dodge(0.75), 
            vjust = -0.5,
            size = 3.5) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = "Jefatura") +
  scale_y_continuous(labels = label_dollar(big.mark = ",")) +
  labs(title = "Gasto trimestral por hogar, ENG 2025",
       subtitle = "Barras de error: intervalo de confianza al 95%",
       x = NULL, y = "Pesos por hogar", caption = fuente) + tema

g1_2

g2 <- per_capita %>%
  separate(grupo, c("sexo", "ambito"), sep = " / ") %>%
  pivot_longer(c(alim_pc, salud_pc), names_to = "rubro", values_to = "monto") %>%
  mutate(se = if_else(rubro == "alim_pc", alim_se, salud_se),
         rubro  = recode(rubro, alim_pc = "Alimentos", salud_pc = "Salud"),
         sexo   = factor(sexo, levels = c("Masculina", "Femenina")),
         ambito = factor(ambito, levels = c("Urbano", "Rural"))) %>%
  ggplot(aes(ambito, monto, fill = sexo)) +
  geom_col(position = position_dodge(0.75), width = 0.6) +
  geom_errorbar(aes(ymin = monto - 1.96 * se, ymax = monto + 1.96 * se),
                position = position_dodge(0.75), width = 0.12, linewidth = 0.4) +
  geom_text(aes(y = monto + 1.96 * se, label = scales::dollar(round(monto,0))), 
            position = position_dodge(0.75), 
            vjust = -0.5,
            size = 3.5) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = "Jefatura") +
  scale_y_continuous(labels = label_dollar(big.mark = ",")) +
  labs(title = "Gasto trimestral per capita, ENG 2025",
       x = NULL, y = "Pesos por integrante", caption = fuente) + tema

g2_1 <- per_capita %>%
  separate(grupo, c("sexo", "ambito"), sep = " / ") %>%
  pivot_longer(c(alim_pc, salud_pc), names_to = "rubro", values_to = "monto") %>%
  mutate(se = if_else(rubro == "alim_pc", alim_se, salud_se),
         rubro  = recode(rubro, alim_pc = "Alimentos", salud_pc = "Salud"),
         sexo   = factor(sexo, levels = c("Masculina", "Femenina")),
         ambito = factor(ambito, levels = c("Urbano", "Rural"))) %>%
  filter(rubro== "Alimentos") |> 
  ggplot(aes(ambito, monto, fill = sexo)) +
  geom_col(position = position_dodge(0.75), width = 0.6) +
  geom_errorbar(aes(ymin = monto - 1.96 * se, ymax = monto + 1.96 * se),
                position = position_dodge(0.75), width = 0.12, linewidth = 0.4) +
  geom_text(aes(y = monto + 1.96 * se, label = scales::dollar(round(monto,0))), 
            position = position_dodge(0.75), 
            vjust = -0.5,
            size = 3.5) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = "Jefatura") +
  scale_y_continuous(labels = label_dollar(big.mark = ",")) +
  labs(title = "Gasto trimestral per capita, ENG 2025",
       x = NULL, y = "Pesos por integrante", caption = fuente) + tema

g2_1

g2_2 <- per_capita %>%
  separate(grupo, c("sexo", "ambito"), sep = " / ") %>%
  pivot_longer(c(alim_pc, salud_pc), names_to = "rubro", values_to = "monto") %>%
  mutate(se = if_else(rubro == "alim_pc", alim_se, salud_se),
         rubro  = recode(rubro, alim_pc = "Alimentos", salud_pc = "Salud"),
         sexo   = factor(sexo, levels = c("Masculina", "Femenina")),
         ambito = factor(ambito, levels = c("Urbano", "Rural"))) %>%
  filter(rubro== "Salud") |> 
  ggplot(aes(ambito, monto, fill = sexo)) +
  geom_col(position = position_dodge(0.75), width = 0.6) +
  geom_errorbar(aes(ymin = monto - 1.96 * se, ymax = monto + 1.96 * se),
                position = position_dodge(0.75), width = 0.12, linewidth = 0.4) +
  geom_text(aes(y = monto + 1.96 * se, label = scales::dollar(round(monto,0))), 
            position = position_dodge(0.75), 
            vjust = -0.5,
            size = 3.5) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = "Jefatura") +
  scale_y_continuous(labels = label_dollar(big.mark = ",")) +
  labs(title = "Gasto trimestral per capita, ENG 2025",
       x = NULL, y = "Pesos por integrante", caption = fuente) + tema
g2_2

g3 <- bind_rows(
  decil_alim  %>% rename(pct = `alimentos/ing_cor`, se = `se.alimentos/ing_cor`) %>%
    mutate(rubro = "Alimentos"),
  decil_salud %>% rename(pct = `salud/ing_cor`, se = `se.salud/ing_cor`) %>%
    mutate(rubro = "Salud")) %>%
  mutate(pct = 100 * pct, se = 100 * se) %>%
  ggplot(aes(decil, pct)) +
  geom_col(aes(fill = rubro), width = 0.65) +
  geom_errorbar(aes(ymin = pct - 1.96 * se, ymax = pct + 1.96 * se),
                width = 0.12, linewidth = 0.4) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = c("Alimentos" = "#2a78d6", "Salud" = "#1baf7a"),
                    guide = "none") +
  scale_x_discrete(labels = c("I","II","III","IV","V","VI","VII","VIII","IX","X")) +
  labs(title = "Porcentaje del ingreso destinado a alimentos y salud, ENG 2025",
       subtitle = "En salud la curva es plana salvo el decil I: el gasto se comporta como un piso",
       x = "Decil de ingreso corriente", y = "% del ingreso del decil",
       caption = fuente) + tema

inc <- svyby(~cat10, ~sexo + ambito, dis_ing, svymean, na.rm = TRUE)
g4 <- inc %>%
  mutate(pct = 100 * cat10, se = 100 * se,
         celda = paste(substr(sexo, 1, 4), ambito, sep = "/")) %>%
  ggplot(aes(celda, pct)) +
  geom_col(fill = "#e34948", width = 0.6) +
  geom_errorbar(aes(ymin = pct - 1.96 * se, ymax = pct + 1.96 * se),
                width = 0.12, linewidth = 0.4) +
  labs(title = "Hogares con gasto en salud mayor al 10% de su ingreso, ENG 2025",
       subtitle = "Umbral OMS de gasto catastrofico",
       x = NULL, y = "% de hogares", caption = fuente) + tema

ggsave("ENG/eng2025_por_hogar.png",     g1, width = 9,   height = 4.5, dpi = 200)
ggsave("ENG/eng2025_por_hogar_alimentos.png",     g1_1, width = 9,   height = 4.5, dpi = 200)
ggsave("ENG/eng2025_por_hogar_salud.png",     g1_2, width = 9,   height = 4.5, dpi = 200)

ggsave("ENG/eng2025_per_capita.png",    g2, width = 9,   height = 4.5, dpi = 200)
ggsave("ENG/eng2025_per_capita_alimentos.png",    g2_1, width = 9,   height = 4.5, dpi = 200)
ggsave("ENG/eng2025_per_capita_salud.png",    g2_2, width = 9,   height = 4.5, dpi = 200)
ggsave("ENG/eng2025_decil_ingreso.png", g3, width = 9.5, height = 4.5, dpi = 200)
ggsave("ENG/eng2025_catastrofico.png",  g4, width = 7,   height = 4.5, dpi = 200)

# Exportar tablas ---------------------------------------------------
write.csv(por_hogar,     "eng_tab_por_hogar.csv",     row.names = FALSE)
write.csv(per_capita,    "eng_tab_per_capita.csv",    row.names = FALSE)
write.csv(participacion, "eng_tab_participacion.csv", row.names = FALSE)
write.csv(prop_ing,      "eng_tab_prop_ingreso.csv",  row.names = FALSE)


#------------------Hogares unipersonales------------------------------------------------
library(survey)
#Debido a que el gasto se puede distribbuir de forma distinta en los miembros de los hogares un forma de observar brechas en gastos en salu
#es observando los hogares unipersonales (una sola persona) 

DIV    <- 3                      # 3 = pesos mensuales; 1 = trimestrales
unidad <- if (DIV == 3) "Pesos mensuales" else "Pesos trimestrales"

h <- read.csv(ruta, stringsAsFactors = FALSE) %>%
  filter(tot_integ == 1) %>%          # equivalente a clase_hog == 1
  mutate(
    sexo = factor(if_else(sexo_jefe == 1, "Hombres", "Mujeres"),
                  levels = c("Hombres", "Mujeres")),
    across(c(alimentos, salud, gasto_mon, ing_cor), ~ .x / DIV),
    gedad = cut(edad_jefe, c(0, 39, 59, 74, Inf),
                labels = c("<40", "40-59", "60-74", "75+")),
    gasta_salud = as.numeric(salud > 0),
    pct_alim  = if_else(gasto_mon > 0, 100 * alimentos / gasto_mon, NA_real_),
    pct_salud = if_else(gasto_mon > 0, 100 * salud / gasto_mon, NA_real_)
  )

dis <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                 data = h, nest = TRUE)

cat("ENG 2025 — hogares unipersonales. Muestra:", nrow(h),
    "| expandidos:", format(sum(h$factor), big.mark = ","), "\n")
print(table(h$sexo))                            # n por sexo: vigilar subgrupos
print(svyby(~factor, ~sexo, dis, svytotal))
print(svyby(~edad_jefe, ~sexo, dis, svymean))   # ellas son mayores: 59.5 vs 52.0

niveles <- svyby(~alimentos + salud, ~sexo, dis, svymean, vartype = c("se", "ci"))
print(niveles)
print(svyby(~pct_alim + pct_salud, ~sexo, dis, svymean, na.rm = TRUE,
            vartype = c("se", "ci")))

# Mediana e incidencia: en salud el promedio describe la cola, no al hogar
# tipico. En la ENG la incidencia es alta (~80%) porque el Cuadernillo de
# Gastos Diarios capta compras de medicamentos que un recordatorio trimestral
# omite; aun asi la distribucion sigue siendo muy asimetrica.
print(svyby(~salud, ~sexo, dis, svyquantile, quantiles = 0.5, ci = FALSE, na.rm = TRUE))
print(svyby(~gasta_salud, ~sexo, dis, svymean))
print(svyby(~salud, ~sexo, subset(dis, salud > 0), svymean))  # intensidad

# Pruebas de la brecha ----------------------------------------------

cat("\n--- Brecha mujeres - hombres ---\n")
print(svyttest(alimentos ~ sexo, dis))   # significativa en pesos
print(svyttest(pct_alim   ~ sexo, dis))  # NO significativa en % del gasto
print(svyttest(salud      ~ sexo, dis))  # significativa en pesos
print(svyttest(pct_salud  ~ sexo, dis))  # significativa tambien en % del gasto

# Robustez -----------------------------------------------------------
# El gasto en salud tiene cola pesada: en la ENG el 1% de hogares con mayor
# gasto aporta ~30% (hombres) y ~33% (mujeres) del total.
cap <- h %>% group_by(sexo) %>%
  summarise(p99 = quantile(salud, 0.99, na.rm = TRUE), .groups = "drop")
hw  <- h %>% left_join(cap, by = "sexo") %>% mutate(salud_w = pmin(salud, p99))
dis_w <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor,
                   data = hw, nest = TRUE)
print(svyttest(salud_w ~ sexo, dis_w))
print(svyttest(salud ~ sexo, subset(dis, edad_jefe >= 40)))

robustez <- function(prueba, nombre) {
  ic <- confint(prueba)
  data.frame(spec = nombre, dif = unname(coef(prueba)),
             lo = ic[1], hi = ic[2], p = prueba$p.value)
}
tabla_rob <- bind_rows(
  robustez(svyttest(salud   ~ sexo, dis),                          "Estimación base"),
  robustez(svyttest(salud_w ~ sexo, dis_w),                        "Winsorizado al p99"),
  robustez(svyttest(salud   ~ sexo, subset(dis, edad_jefe >= 40)), "Solo 40 años y más")
) %>% mutate(spec = factor(spec, levels = rev(spec)))
print(tabla_rob)

# Edad: estratificacion y estandarizacion ---------------------------
# Objecion obvia: ellas gastan mas porque son mayores (44.8% tiene 65+ contra
# 26.8% de ellos). La respuesta es que la edad explica solo ~14% de la brecha
# en la ENG; el resto persiste al comparar personas de la misma edad.
# Cuidado al leer el grupo <40: n = 248 en mujeres, IC muy ancho.
por_edad <- svyby(~alimentos + salud, ~gedad + sexo, dis, svymean,
                  vartype = c("se", "ci"))
print(por_edad)
print(svyby(~factor, ~gedad + sexo, dis, svytotal))   # composicion por edad
print(table(h$gedad, h$sexo))                          # n por celda

pesos_m <- h %>% filter(sexo == "Mujeres") %>% count(gedad, wt = factor) %>%
  mutate(p = n / sum(n))
med_h <- por_edad %>% filter(sexo == "Hombres") %>% select(gedad, salud)
est_h <- sum(pesos_m$p * med_h$salud[match(pesos_m$gedad, med_h$gedad)])
obs_h <- coef(svymean(~salud, subset(dis, sexo == "Hombres")))
obs_m <- coef(svymean(~salud, subset(dis, sexo == "Mujeres")))
cat(sprintf("\nHombres observado %.0f | estandarizado a la edad de ellas %.0f | Mujeres %.0f\n",
            obs_h, est_h, obs_m))
cat(sprintf("Brecha bruta %+.0f | ajustada por edad %+.0f | explicado por edad %.0f%%\n",
            obs_m - obs_h, obs_m - est_h, 100 * (est_h - obs_h) / (obs_m - obs_h)))

# Alternativa parametrica equivalente (y mas facil de extender)
print(summary(svyglm(salud ~ sexo + gedad, design = dis)))

#Graficas -----------------------------------------------------------
pal  <- c("Hombres" = "#2a78d6", "Mujeres" = "#eb6834")
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.x = element_blank(),
        plot.title.position = "plot", legend.position = "top")
fuente <- "Fuente: INEGI, ENG 2025, concentradohogar. Hogares unipersonales, factor anual."
pesos  <- label_dollar(big.mark = ",")

nivel_plot <- function(rubro, se_col, titulo) {
  d <- niveles %>% transmute(sexo, m = .data[[rubro]], se = .data[[se_col]])
  ggplot(d, aes(sexo, m, fill = sexo)) +
    geom_col(width = 0.55) +
    geom_errorbar(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se),
                  width = 0.11, linewidth = 0.4) +
    geom_text(aes(y = m + 1.96 * se, label = pesos(round(m))),
              vjust = -0.7, size = 4) +
    scale_fill_manual(values = pal, guide = "none") +
    scale_y_continuous(labels = pesos, expand = expansion(mult = c(0, 0.18))) +
    labs(title = titulo, x = NULL, y = unidad, caption = fuente) + tema
}
g1 <- nivel_plot("alimentos", "se.alimentos", "Gasto en alimentos, ENG 2025")
g1
g2 <- nivel_plot("salud",     "se.salud",     "Gasto en salud, ENG 2025")
g2
g3 <- por_edad %>%
  pivot_longer(c(alimentos, salud), names_to = "rubro", values_to = "m") %>%
  mutate(se = if_else(rubro == "alimentos", se.alimentos, se.salud),
         rubro = recode(rubro, alimentos = "Alimentos", salud = "Salud")) %>%
  ggplot(aes(gedad, m, fill = sexo)) +
  geom_col(position = position_dodge(0.78), width = 0.66) +
  geom_errorbar(aes(ymin = m - 1.96 * se, ymax = m + 1.96 * se),
                position = position_dodge(0.78), width = 0.12, linewidth = 0.4) +
  facet_wrap(~rubro, scales = "free_y") +
  scale_fill_manual(values = pal, name = NULL) +
  scale_y_continuous(labels = pesos) +
  labs(title = "Gasto por edad en hogares unipersonales, ENG 2025",
       subtitle = "En salud la brecha persiste dentro de cada grupo de edad",
       x = "Edad", y = unidad, caption = fuente) + tema

g4 <- data.frame(
  cat = factor(c("Hombres\n(observado)", "Hombres\nestandarizados\na la edad de ellas",
                 "Mujeres\n(observado)"),
               levels = c("Hombres\n(observado)",
                          "Hombres\nestandarizados\na la edad de ellas",
                          "Mujeres\n(observado)")),
  v = c(obs_h, est_h, obs_m)) %>%
  ggplot(aes(cat, v, fill = cat)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = pesos(round(v))), vjust = -0.7, size = 4) +
  scale_fill_manual(values = c("#2a78d6", "#888780", "#eb6834"), guide = "none") +
  scale_y_continuous(labels = pesos, expand = expansion(mult = c(0, 0.16))) +
  labs(title = "¿Cuánto de la brecha en salud explica la edad?",
       subtitle = sprintf("La edad explica %.0f%%; quedan %s sin explicar",
                          100 * (est_h - obs_h) / (obs_m - obs_h),
                          pesos(round(obs_m - est_h))),
       x = NULL, y = unidad, caption = fuente) + tema

g5 <- ggplot(tabla_rob, aes(dif, spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#52514e") +
  geom_errorbarh(aes(xmin = lo, xmax = hi, colour = lo > 0),
                 height = 0.14, linewidth = 0.8) +
  geom_point(aes(colour = lo > 0), size = 3) +
  geom_text(aes(x = hi, label = sprintf("%+.0f  p=%.3f", dif, p)),
            hjust = -0.12, size = 3.4, colour = "#52514e") +
  scale_colour_manual(values = c("TRUE" = "#2a78d6", "FALSE" = "#888780"),
                      guide = "none") +
  scale_x_continuous(labels = pesos, expand = expansion(mult = c(0.08, 0.34))) +
  labs(title = "Brecha de género en gasto en salud, ENG 2025",
       subtitle = "Intervalos de confianza al 95% de la diferencia mujeres − hombres",
       x = paste0("Diferencia (", tolower(unidad), ")"), y = NULL,
       caption = paste(fuente,
                       "\nEl traslape de los intervalos individuales no decide; lo que decide es si este IC cruza el cero.")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), plot.title.position = "plot")

ggsave("eng2025_uni_alimentos.png",      g1, width = 5.6, height = 4.6, dpi = 200)
ggsave("eng2025_uni_salud.png",          g2, width = 5.6, height = 4.6, dpi = 200)
ggsave("eng2025_uni_edad.png",           g3, width = 9.2, height = 4.6, dpi = 200)
ggsave("eng2025_uni_descomposicion.png", g4, width = 6.6, height = 4.8, dpi = 200)
ggsave("eng2025_uni_robustez.png",       g5, width = 8.4, height = 4.2, dpi = 200)

write.csv(niveles,   "eng2025_uni_niveles.csv",  row.names = FALSE)
write.csv(por_edad,  "eng2025_uni_por_edad.csv", row.names = FALSE)
write.csv(tabla_rob, "eng2025_uni_robustez.csv", row.names = FALSE)

# ---- Notas a considerar ---------------
# Lo que la brecha deja sin explicar admite varias lecturas que concentradohogar
# no separa: mayor morbilidad, mas uso preventivo, menor cobertura formal (y por
# tanto mas gasto de bolsillo), o gasto en salud reproductiva. La ENG ofrece dos
# vias que la ENIGH no, ambas en la tabla GASTOSHOGAR (no en POBLACION):
#
# (a) inst_1 / inst_2 — institucion donde se recibio cada servicio de salud:
#     01 particulares, 02 IMSS, 03 ISSSTE, 04 PEMEX, 05 Marina, 06 Ejercito,
#     07 SSA, 09 DIF, 10 universidades, 11 Cruz Roja y similares,
#     12 Prospera/Bienestar, 13 otro hogar.
#     Permite ver si ellas gastan mas porque usan mas al proveedor privado.
gh <- read.csv("ENG/gastoshogar.csv")
       sal <- gh %>% filter(substr(clave,1,1) == "J")   # verificar la clave
#       # agregar gasto_tri por hogar e inst_1, unir a h por folioviv+foliohog
#       # (el hogar es unipersonal, asi que la union es uno a uno)
#
# (b) inmujer / imujer_tri — gasto realizado en productos o servicios obtenidos
#     exclusivamente para mujeres o ninas. En hogares unipersonales de mujeres
#     es en buena medida redundante, pero permite cuantificar que parte del
#     gasto en salud corresponde a productos especificamente femeninos.
#     Ojo: el valor -1 significa dato no especificado, no cero. Hay que
#     convertirlo a NA antes de agregar.
