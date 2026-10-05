
# =============================================================================
# Conteo Brasil 2026 · primera vuelta (4 de octubre de 2026)
#
# Descarga el corte más reciente de los archivos oficiales del TSE y genera

# Fuente: https://resultados.tse.jus.br/oficial/ele2026/<elección>/dados/<uf>/<uf>-c<cargo>-e<elección>-u.json
#         Elección 6257 = federal (presidente); 6259 = estatales (senado, cámara).


BASE             <- Sys.getenv("CONTEO_BASE", "https://resultados.tse.jus.br/oficial/ele2026")
ELE_FEDERAL      <- "6257"   # presidente
ELE_ESTATAL      <- "6259"   # gobernador, senado, diputados
DIR_SALIDA       <- "conteo_brasil_2026"
INCLUIR_EXTERIOR <- TRUE     # 186 ciudades: tarda cerca de medio minuto
INCLUIR_SENADO   <- TRUE
INCLUIR_CAMARA   <- TRUE     # 27 archivos grandes (todos los candidatos a diputado federal)
PAUSA            <- 0.05     # segundos entre solicitudes

paquetes <- c("jsonlite", "httr", "dplyr", "tidyr", "ggplot2", "maps", "scales")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) install.packages(faltan, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(jsonlite); library(httr); library(dplyr); library(tidyr); library(ggplot2)
})


UFS <- c(AC = "Acre", AL = "Alagoas", AP = "Amapá", AM = "Amazonas", BA = "Bahía", CE = "Ceará",
         DF = "Distrito Federal", ES = "Espírito Santo", GO = "Goiás", MA = "Maranhão",
         MT = "Mato Grosso", MS = "Mato Grosso do Sul", MG = "Minas Gerais", PA = "Pará",
         PB = "Paraíba", PR = "Paraná", PE = "Pernambuco", PI = "Piauí", RJ = "Río de Janeiro",
         RN = "Rio Grande do Norte", RS = "Rio Grande do Sul", RO = "Rondônia", RR = "Roraima",
         SC = "Santa Catarina", SP = "São Paulo", SE = "Sergipe", TO = "Tocantins")

# Posición de cada estado en el mapa de casillas (columna, fila)
CASILLAS <- data.frame(
  uf  = c("RR","AP","AM","PA","MA","CE","RN","AC","RO","TO","PI","PB","PE","MT","GO","BA","SE","AL","MS","DF","MG","ES","PR","SP","RJ","SC","RS"),
  col = c(1,2, 1,2,3,4,5, 0,1,2,3,4,5, 1,2,3,4,5, 1,2,3,4, 1,2,3, 1, 1),
  fil = c(0,0, 1,1,1,1,1, 2,2,2,2,2,2, 3,3,3,3,3, 4,4,4,4, 5,5,5, 6, 7),
  stringsAsFactors = FALSE)

# Candidaturas principales, por número en la urna: nombre corto y color (los mismos del tablero)
PRINCIPALES <- data.frame(
  numero = c("22", "13", "70", "14", "55", "30"),
  corto  = c("Flávio Bolsonaro", "Lula", "Augusto Cury", "Renan Santos", "Ronaldo Caiado", "Romeu Zema"),
  color  = c("#2a78d6", "#e34948", "#1baf7a", "#eda100", "#4a3aa7", "#eb6834"),
  stringsAsFactors = FALSE)
COLOR_OTROS <- "#9aa39f"
PALETA <- c(setNames(PRINCIPALES$color, PRINCIPALES$corto), "Otras candidaturas" = COLOR_OTROS)

# Gráficas de herradura. ORDEN_ARCO fija qué partidos van en cada extremo: la primera mitad de la lista
# se dibuja desde la izquierda y la segunda termina en la derecha; los demás partidos quedan en medio,
# de mayor a menor. Cambia el orden o los colores a tu gusto (las siglas son las que publica el TSE).
ORDEN_ARCO <- c("PT", "PSB", "MDB", "PSD", "UNIÃO", "PP", "REPUBLICANOS", "PL")
COLORES_PARTIDO <- c("PT" = "#e34948", "PSB" = "#4a3aa7", "MDB" = "#008300", "PSD" = "#e87ba4",
                     "UNIÃO" = "#eda100", "PP" = "#1baf7a", "REPUBLICANOS" = "#eb6834", "PL" = "#2a78d6")
GRISES_PARTIDO <- c("#8d9793")              # bloque de "Otros partidos" (los que no tienen color arriba)
COLOR_SIN_DEFINIR <- "#e9ecea"

# Ciudades del exterior (código del TSE) y país al que pertenecen. El TSE no publica el país;
# la correspondencia viene del proyecto abierto github.com/ws49066/apuracao-eleicao-2026-exterior.
LOCALES_EXT <- read.table(sep = ";", header = TRUE, colClasses = "character", quote = "", na.strings = "", text = "codigo;iso2
29254;CI
29262;AE
99198;NG
29270;GH
99325;ET
30457;NL
29289;JO
29297;TR
29300;DZ
29319;UY
29327;PY
39241;KZ
29335;GR
39080;US
99171;IQ
39128;AZ
99350;ML
29343;TH
29351;ES
99473;BH
29360;LB
29378;RS
38881;BZ
29386;DE
29394;GW
29408;CO
29416;US
39209;SK
39187;CG
29424;BB
29432;BE
29440;RO
29459;HU
29467;AR
29475;GF
29483;EG
29491;AU
30651;CN
29505;VE
99384;LC
29513;US
29521;UY
29530;ZA
29556;PY
29564;VE
99210;BO
29572;BO
30929;LK
38903;GN
29580;PY
29599;DK
38920;BJ
29602;AR
29610;SN
29629;BD
29637;SY
38962;TZ
29653;QA
29661;IE
29645;TL
99503;GB
29670;PY
29688;SE
30961;PT
29696;DE
30669;BW
29700;CH
29718;GY
98000;GT
29742;JP
29750;VN
29769;ZW
30902;US
29777;CU
29785;FI
29793;HK
29807;US
29815;CM
38989;AM
29823;PE
29831;PK
39306;TR
29840;ID
29173;NP
29858;UA
99430;JM
29874;CD
29882;KW
29890;MY
29904;BO
29912;NG
29939;GA
99341;MW
29947;PE
29955;PT
39160;SI
29963;TG
29971;GB
29980;US
29998;AO
99287;ZM
30066;ES
39263;GQ
30082;PH
30074;NI
30090;MZ
99511;FR
39102;OM
39004;AR
30104;MX
30112;US
30120;IT
30147;UY
30155;CA
30163;RU
30171;IN
30180;DE
30198;JP
30201;KE
99180;BS
39322;CY
30210;IN
30228;US
99490;US
30244;NO
30252;CA
30260;PA
30279;SR
30287;FR
30295;AR
30309;PY
30317;CN
30325;TT
30341;PT
30333;HT
30350;CZ
30368;CV
30376;ZA
99155;AR
99236;BO
99295;KP
30392;EC
30406;MA
30414;PS
30422;SA
30430;UY
99244;UY
30449;IT
99147;AG
30465;PY
30473;BO
99279;VE
30481;CL
30988;BA
30538;KR
29548;SG
99333;GF
30562;AU
30490;DO
30503;US
30511;CR
30520;SV
39225;ST
30546;BG
30570;TW
99317;EE
99104;GE
30597;IR
30600;HN
30619;IL
99139;AL
30635;CA
30686;LY
30708;TN
30627;JP
39284;BF
39063;CA
30740;PL
30767;AT
30783;US
30805;NZ
30821;NA
30848;CN
99376;MM
39020;HR
30864;CH")
PAISES <- read.table(sep = ";", header = TRUE, colClasses = "character", quote = "", na.strings = "", text = "iso2;pais
CI;Costa de Marfil
AE;Emiratos Árabes Unidos
NG;Nigeria
GH;Ghana
ET;Etiopía
NL;Países Bajos
JO;Jordania
TR;Turquía
DZ;Argelia
UY;Uruguay
PY;Paraguay
KZ;Kazajistán
GR;Grecia
US;Estados Unidos
IQ;Irak
AZ;Azerbaiyán
ML;Malí
TH;Tailandia
ES;España
BH;Baréin
LB;Líbano
RS;Serbia
BZ;Belice
DE;Alemania
GW;Guinea-Bisáu
CO;Colombia
SK;Eslovaquia
CG;República del Congo
BB;Barbados
BE;Bélgica
RO;Rumania
HU;Hungría
AR;Argentina
GF;Guayana Francesa
EG;Egipto
AU;Australia
CN;China
VE;Venezuela
LC;Santa Lucía
ZA;Sudáfrica
BO;Bolivia
LK;Sri Lanka
GN;Guinea
DK;Dinamarca
BJ;Benín
SN;Senegal
BD;Bangladés
SY;Siria
TZ;Tanzania
QA;Catar
IE;Irlanda
TL;Timor Oriental
GB;Reino Unido
SE;Suecia
PT;Portugal
BW;Botsuana
CH;Suiza
GY;Guyana
GT;Guatemala
JP;Japón
VN;Vietnam
ZW;Zimbabue
CU;Cuba
FI;Finlandia
HK;Hong Kong (China)
CM;Camerún
AM;Armenia
PE;Perú
PK;Pakistán
ID;Indonesia
NP;Nepal
UA;Ucrania
JM;Jamaica
CD;República Democrática del Congo
KW;Kuwait
MY;Malasia
GA;Gabón
MW;Malaui
SI;Eslovenia
TG;Togo
AO;Angola
ZM;Zambia
GQ;Guinea Ecuatorial
PH;Filipinas
NI;Nicaragua
MZ;Mozambique
FR;Francia
OM;Omán
MX;México
IT;Italia
CA;Canadá
RU;Rusia
IN;India
KE;Kenia
BS;Bahamas
CY;Chipre
NO;Noruega
PA;Panamá
SR;Surinam
TT;Trinidad y Tobago
HT;Haití
CZ;Chequia
CV;Cabo Verde
KP;Corea del Norte
EC;Ecuador
MA;Marruecos
PS;Palestina
SA;Arabia Saudita
AG;Antigua y Barbuda
CL;Chile
BA;Bosnia y Herzegovina
KR;Corea del Sur
SG;Singapur
DO;República Dominicana
CR;Costa Rica
SV;El Salvador
ST;Santo Tomé y Príncipe
BG;Bulgaria
TW;Taiwán
EE;Estonia
GE;Georgia
IR;Irán
HN;Honduras
IL;Israel
AL;Albania
LY;Libia
TN;Túnez
BF;Burkina Faso
PL;Polonia
AT;Austria
NZ;Nueva Zelanda
NA;Namibia
MM;Myanmar
HR;Croacia")

# ---- Utilidades -------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
campo <- function(x, k) if (is.list(x)) x[[k]] else NULL   # acceso exacto, sin coincidencia parcial
num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- as.character(x)
  x <- ifelse(grepl(",", x, fixed = TRUE), gsub(",", ".", gsub(".", "", x, fixed = TRUE), fixed = TRUE), x)
  suppressWarnings(as.numeric(x))
}
miles <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
titulo <- function(x) {
  vapply(strsplit(tolower(x), " ", fixed = TRUE), function(p) {
    paste(ifelse(p %in% c("de", "da", "do", "das", "dos", "e"), p,
                 paste0(toupper(substring(p, 1, 1)), substring(p, 2))), collapse = " ")
  }, character(1)) -> out
  out[is.na(x)] <- NA_character_
  out
}
nombre_corto <- function(numero, nombre) {
  i <- match(as.character(numero), PRINCIPALES$numero)
  ifelse(is.na(i), titulo(nombre), PRINCIPALES$corto[i])
}
clave_color <- function(numero) {
  i <- match(as.character(numero), PRINCIPALES$numero)
  ifelse(is.na(i), "Otras candidaturas", PRINCIPALES$corto[i])
}

url_tse <- function(ele, uf, cargo, mun = "") {
  uf <- tolower(uf)
  sprintf("%s/%s/dados/%s/%s%s-c%04d-e%06d-u.json", BASE, ele, uf, uf, mun, as.integer(cargo), as.integer(ele))
}

leer_json <- function(url, intentos = 3) {
  if (!grepl("^https?://", url)) {                       # ruta local (para pruebas)
    if (!file.exists(url)) return(NULL)
    return(tryCatch(fromJSON(url, simplifyVector = FALSE), error = function(e) NULL))
  }
  for (i in seq_len(intentos)) {
    r <- tryCatch(GET(url, user_agent("conteo-brasil-2026 (R)"), timeout(30)), error = function(e) NULL)
    if (!is.null(r)) {
      if (status_code(r) == 200) {
        txt <- content(r, as = "text", encoding = "UTF-8")
        return(tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL))
      }
      if (status_code(r) %in% c(403, 404)) return(NULL)  # el archivo todavía no existe
    }
    Sys.sleep(1.5 * i)
  }
  NULL
}

# Lee un archivo "-u.json" y devuelve list(resumen = 1 fila, cands = una fila por candidatura)
leer_resultado <- function(ele, uf, cargo, mun = "") {
  Sys.sleep(PAUSA)
  d <- leer_json(url_tse(ele, uf, cargo, mun))
  if (is.null(d)) return(NULL)
  cargos <- campo(d, "carg") %||% list()
  if (!length(cargos)) return(NULL)
  cds <- vapply(cargos, function(x) suppressWarnings(as.integer(campo(x, "cd") %||% NA)), integer(1))
  cg <- cargos[[if (any(cds == cargo, na.rm = TRUE)) which(cds == cargo)[1] else 1]]
  filas <- list()
  for (a in campo(cg, "agr") %||% list()) for (p in campo(a, "par") %||% list()) for (k in campo(p, "cand") %||% list()) {
    filas[[length(filas) + 1]] <- data.frame(
      numero = as.character(campo(k, "n") %||% NA), nombre = as.character(campo(k, "nmu") %||% campo(k, "nm") %||% NA),
      partido = as.character(campo(p, "sg") %||% NA), votos = num(campo(k, "vap")), pct_tse = num(campo(k, "pvap")),
      electo = identical(tolower(as.character(campo(k, "e") %||% "n")), "s"),
      situacion = as.character(campo(k, "st") %||% ""), stringsAsFactors = FALSE)
  }
  cands <- if (length(filas)) bind_rows(filas) else data.frame(numero = character(), nombre = character(), partido = character(),
                                                               votos = numeric(), pct_tse = numeric(), electo = logical(), situacion = character(), stringsAsFactors = FALSE)
  cands$votos[is.na(cands$votos)] <- 0
  s <- campo(d, "s"); e <- campo(d, "e"); v <- campo(d, "v")
  validos <- num(campo(v, "vv")); if (is.na(validos)) validos <- sum(cands$votos)
  blancos <- num(campo(v, "vb")); nulos <- num(campo(v, "tvn") %||% campo(v, "vn"))
  total <- num(campo(v, "tv")); if (is.na(total)) total <- sum(c(validos, blancos, nulos), na.rm = TRUE)
  resumen <- data.frame(
    ambito = toupper(uf), codigo = mun,
    fecha = as.character(campo(d, "dt") %||% campo(d, "dg") %||% NA), hora = as.character(campo(d, "ht") %||% campo(d, "hg") %||% NA),
    pct_secciones = num(campo(s, "pst")), electorado = num(campo(e, "te")), participacion = num(campo(e, "c")),
    abstencion = num(campo(e, "a")), pct_abstencion = num(campo(e, "pa")),
    validos = validos, blancos = blancos, nulos = nulos, total = total,
    vacantes = num(campo(cg, "nv")), final = identical(tolower(as.character(campo(d, "tf") %||% "n")), "s"),
    stringsAsFactors = FALSE)
  cands <- cands %>% arrange(desc(votos)) %>% mutate(pct = if (validos > 0) 100 * votos / validos else 0)
  list(resumen = resumen, cands = cands)
}

guardar <- function(g, archivo, ancho = 9, alto = 5.5) {
  ggsave(file.path(dir_corte, archivo), g, width = ancho, height = alto, dpi = 150, bg = "white")
}
tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), plot.caption = element_text(colour = "grey40", hjust = 0))

# ---- Gráfica de herradura ---------------------------------------------------
sin_acentos <- function(x) chartr("ÃÁÂÀÉÊÍÓÔÕÚÇ", "AAAAEEIOOOUC", toupper(x))

# Coordenadas de n asientos en media luna, ordenados de izquierda a derecha
asientos <- function(n) {
  filas <- max(3, round(sqrt(n / 3.5)))
  radios <- seq(0.5, 1, length.out = filas)
  por_fila <- floor(n * radios / sum(radios))
  falta <- n - sum(por_fila); i <- filas
  while (falta > 0) { por_fila[i] <- por_fila[i] + 1; falta <- falta - 1; i <- if (i == 1) filas else i - 1 }
  a <- do.call(rbind, lapply(seq_len(filas), function(f) {
    k <- por_fila[f]
    if (k == 0) return(NULL)
    ang <- if (k == 1) pi / 2 else seq(pi, 0, length.out = k)
    data.frame(fila = f, ang = ang, x = radios[f] * cos(ang), y = radios[f] * sin(ang))
  }))
  a <- a[order(-a$ang, a$fila), ]
  attr(a, "separacion") <- min(pi / max(por_fila[filas] - 1, 1), 0.5 / max(filas - 1, 1))
  a
}

# df: data.frame(partido, escanos). total: tamaño de la cámara que se dibuja.
# Los partidos con color en COLORES_PARTIDO se dibujan por separado; el resto se junta en un bloque
# gris de "Otros partidos" y su desglose va al pie. Para separar uno más, dale un color en COLORES_PARTIDO.
herradura <- function(df, titulo_g, subtitulo, total, archivo, pie = NULL) {
  df <- df[!is.na(df$escanos) & df$escanos > 0, c("partido", "escanos")]
  if (!nrow(df)) return(invisible(NULL))
  df <- df[order(-df$escanos, df$partido), ]
  cat_col <- setNames(unname(COLORES_PARTIDO), sin_acentos(names(COLORES_PARTIDO)))
  df$color <- unname(cat_col[sin_acentos(df$partido)])
  otros <- df[is.na(df$color), ]; df <- df[!is.na(df$color), ]
  clave <- sin_acentos(df$partido); arco <- sin_acentos(ORDEN_ARCO)
  mitad <- ceiling(length(arco) / 2)
  izq <- match(arco[seq_len(mitad)], clave, nomatch = 0)
  der <- match(arco[-seq_len(mitad)], clave, nomatch = 0)
  medio <- setdiff(seq_len(nrow(df)), c(izq, der))        # con color pero fuera de ORDEN_ARCO
  bloque <- function(idx) df[idx, c("partido", "escanos", "color")]
  centro <- bloque(medio)
  if (nrow(otros)) {
    centro <- rbind(centro, data.frame(partido = "Otros partidos", escanos = sum(otros$escanos), color = GRISES_PARTIDO[1], stringsAsFactors = FALSE))
    desglose <- paste0("Otros partidos: ", paste(sprintf("%s %d", otros$partido, otros$escanos), collapse = ", "), ".")
    pie <- paste(c(pie, strwrap(desglose, width = 115)), collapse = "\n")
  }
  d <- rbind(bloque(izq), centro, bloque(der))
  pendientes <- max(0, total - sum(d$escanos))
  if (pendientes > 0) d <- rbind(d, data.frame(partido = "Sin definir", escanos = pendientes, color = COLOR_SIN_DEFINIR, stringsAsFactors = FALSE))
  definidos <- sum(d$escanos) - pendientes
  a <- asientos(sum(d$escanos))
  a$partido <- factor(rep(d$partido, d$escanos), levels = d$partido)
  tam <- min(13, 0.78 * 95 * attr(a, "separacion"))
  g <- ggplot(a, aes(x = x, y = y, fill = partido)) +
    geom_point(shape = 21, colour = "white", stroke = 0.3, size = tam) +
    annotate("text", x = 0, y = 0.17, label = miles(definidos), size = 11, fontface = "bold", colour = "grey15") +
    annotate("text", x = 0, y = 0.03, label = if (pendientes > 0) sprintf("de %s escaños", miles(total)) else "escaños", size = 4, colour = "grey35") +
    scale_fill_manual(values = setNames(d$color, d$partido), breaks = d$partido, labels = sprintf("%s (%d)", d$partido, d$escanos), name = NULL, drop = FALSE) +
    coord_equal(xlim = c(-1.1, 1.1), ylim = c(-0.09, 1.1), expand = FALSE) +
    guides(fill = guide_legend(nrow = ceiling(nrow(d) / 5), byrow = TRUE, override.aes = list(size = 4.5))) +
    labs(title = titulo_g, subtitle = subtitulo, caption = pie) +
    tema + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
                 legend.position = "bottom", legend.text = element_text(size = 10))
  guardar(g, archivo, ancho = 9, alto = 6.3 + 0.22 * ceiling(nrow(d) / 5))
  invisible(g)
}

# =============================================================================
# 1. Presidencia nacional
# =============================================================================
cat("Leyendo presidencia nacional...\n")
nac <- leer_resultado(ELE_FEDERAL, "br", 1)
if (is.null(nac)) stop("No se pudo leer el archivo nacional del TSE:\n  ", url_tse(ELE_FEDERAL, "br", 1),
                       "\nRevisa tu conexión o abre esa dirección en el navegador para ver si responde.")
rn <- nac$resumen
sello <- paste(rn$fecha, rn$hora)
etiqueta_corte <- sprintf("Corte del TSE: %s (hora de Brasilia) · %.2f%% de secciones totalizadas", sello, rn$pct_secciones)
dir_corte <- file.path(DIR_SALIDA, paste0("corte_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(dir_corte, recursive = TRUE, showWarnings = FALSE)

pres <- nac$cands %>% mutate(candidato = nombre_corto(numero, nombre), clave = clave_color(numero))
write.csv(pres %>% select(numero, candidato, partido, votos, pct, pct_tse), file.path(dir_corte, "presidencia_nacional.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(rn, file.path(dir_corte, "resumen_nacional.csv"), row.names = FALSE, fileEncoding = "UTF-8")

graf <- pres %>% mutate(etq = paste0(candidato, " (", partido, ")"))
g1 <- ggplot(graf, aes(x = pct, y = reorder(etq, pct), fill = clave)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 50, colour = "grey35", linewidth = 0.4) +
  annotate("text", x = 51, y = Inf, label = "50%", size = 3, colour = "grey35", vjust = 1.4, hjust = 0) +
  geom_text(aes(label = sprintf("%.2f%%  ·  %s votos", pct, miles(votos))), hjust = -0.04, size = 3.3) +
  scale_fill_manual(values = PALETA, guide = "none") +
  scale_x_continuous(limits = c(0, max(85, max(graf$pct) * 1.5)), breaks = seq(0, 100, 10), labels = function(x) paste0(x, "%")) +
  labs(title = "Presidencia de la República: votos válidos por candidatura", subtitle = etiqueta_corte, x = NULL, y = NULL,
       caption = sprintf("Válidos: %s · En blanco: %s · Nulos: %s · Abstención: %s%%.\nLa línea marca 50%%: superarla evita la segunda vuelta.",
                         miles(rn$validos), miles(rn$blancos), miles(rn$nulos), format(rn$pct_abstencion, nsmall = 2))) +
  tema + theme(panel.grid.major.y = element_blank())
guardar(g1, "01_presidencia_nacional.png")

# ---- Tendencia: se acumula un renglón por candidatura en cada corrida ---------
arch_serie <- file.path(DIR_SALIDA, "serie_nacional.csv")
nuevo <- pres %>% transmute(fecha = rn$fecha, hora = rn$hora, pct_secciones = rn$pct_secciones, numero, candidato, votos, pct)
serie <- if (file.exists(arch_serie)) read.csv(arch_serie, colClasses = c(numero = "character"), fileEncoding = "UTF-8", stringsAsFactors = FALSE) else nuevo[0, ]
serie <- bind_rows(serie %>% filter(!(abs(pct_secciones - rn$pct_secciones) < 1e-9)), nuevo) %>% arrange(pct_secciones)
write.csv(serie, arch_serie, row.names = FALSE, fileEncoding = "UTF-8")
dos <- head(pres$numero, 2)
st <- serie %>% filter(numero %in% dos) %>% mutate(clave = clave_color(numero))
g2 <- ggplot(st, aes(x = pct_secciones, y = pct, colour = clave, group = clave)) +
  geom_hline(yintercept = 50, colour = "grey60", linewidth = 0.4) +
  { if (length(unique(st$pct_secciones)) > 1) geom_line(linewidth = 0.9) } + geom_point(size = 2.6) +
  geom_text(data = st %>% group_by(clave) %>% slice_max(pct_secciones, n = 1, with_ties = FALSE),
            aes(label = sprintf("%.2f%%", pct)), hjust = -0.25, size = 3.4, colour = "grey15", show.legend = FALSE) +
  scale_colour_manual(values = PALETA, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.03, 0.14))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Cómo se mueven los dos primeros lugares", subtitle = "Porcentaje de votos válidos según avanza el conteo",
       x = "Secciones totalizadas", y = NULL, caption = sprintf("%d cortes guardados en %s", length(unique(serie$pct_secciones)), arch_serie)) +
  tema + theme(legend.position = "top", legend.justification = "left")
guardar(g2, "02_tendencia.png")

# =============================================================================
# 2. Presidencia por estado
# =============================================================================
cat("Leyendo presidencia por estado")
est_cands <- list(); est_res <- list()
for (uf in names(UFS)) {
  cat(".")
  r <- leer_resultado(ELE_FEDERAL, uf, 1)
  if (is.null(r)) next
  est_res[[uf]] <- r$resumen
  est_cands[[uf]] <- r$cands %>% mutate(uf = uf)
}
cat("\n")
estados <- bind_rows(lapply(names(UFS), function(uf) {
  r <- est_res[[uf]]; k <- est_cands[[uf]]
  if (is.null(r) || is.null(k) || !nrow(k) || !isTRUE(r$validos > 0))
    return(data.frame(uf = uf, estado = UFS[[uf]], pct_secciones = if (is.null(r)) NA else r$pct_secciones, stringsAsFactors = FALSE))
  data.frame(uf = uf, estado = UFS[[uf]], pct_secciones = r$pct_secciones,
             va_adelante = nombre_corto(k$numero[1], k$nombre[1]), partido = k$partido[1], numero_lider = k$numero[1],
             pct_lider = k$pct[1], votos_lider = k$votos[1],
             segundo = if (nrow(k) > 1) nombre_corto(k$numero[2], k$nombre[2]) else NA, pct_segundo = if (nrow(k) > 1) k$pct[2] else NA,
             ventaja_pts = if (nrow(k) > 1) k$pct[1] - k$pct[2] else NA,
             votos_validos = r$validos, votos_totales = r$total, pct_abstencion = r$pct_abstencion, stringsAsFactors = FALSE)
}))
write.csv(estados, file.path(dir_corte, "presidencia_estados.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (length(est_cands)) write.csv(bind_rows(est_cands) %>% mutate(candidato = nombre_corto(numero, nombre)) %>% select(uf, numero, candidato, partido, votos, pct),
                                 file.path(dir_corte, "presidencia_estados_candidatos.csv"), row.names = FALSE, fileEncoding = "UTF-8")

if ("va_adelante" %in% names(estados)) {
  cas <- CASILLAS %>% left_join(estados, by = "uf") %>%
    mutate(clave = ifelse(is.na(va_adelante), NA, clave_color(numero_lider)),
           texto = ifelse(is.na(va_adelante), paste0(uf, "\n-"), sprintf("%s\n%.1f%%", uf, pct_lider)))
  g3 <- ggplot(cas, aes(x = col, y = -fil)) +
    geom_tile(fill = "grey88", colour = "white", linewidth = 1.2, width = 0.96, height = 0.96) +
    geom_tile(data = cas %>% filter(!is.na(clave)), aes(fill = clave), colour = "white", linewidth = 1.2, width = 0.96, height = 0.96) +
    geom_text(aes(label = texto, colour = is.na(clave)), size = 3.3, lineheight = 0.95, fontface = "bold", show.legend = FALSE) +
    scale_colour_manual(values = c("FALSE" = "white", "TRUE" = "grey35"), guide = "none") +
    scale_fill_manual(values = PALETA, name = "Va adelante") +
    coord_equal() +
    labs(title = "Presidencia por estado: quién va adelante", subtitle = etiqueta_corte,
         caption = "Cada casilla es una unidad federativa; el número es el porcentaje de votos válidos de quien va adelante.") +
    tema + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "right")
  guardar(g3, "03_estados_mapa.png", ancho = 8, alto = 7)
}

# =============================================================================
# 3. Voto en el extranjero
# =============================================================================
ext_paises <- NULL
if (INCLUIR_EXTERIOR) {
  cfg <- leer_json(sprintf("%s/%s/config/mun-e%06d-cm.json", BASE, ELE_FEDERAL, as.integer(ELE_FEDERAL)))
  ciudades <- NULL
  for (a in campo(cfg, "abr") %||% list()) if (identical(toupper(as.character(campo(a, "cd"))), "ZZ"))
    ciudades <- bind_rows(lapply(campo(a, "mu") %||% list(), function(m) data.frame(codigo = as.character(campo(m, "cd")), ciudad = as.character(campo(m, "nm") %||% NA), stringsAsFactors = FALSE)))
  if (is.null(ciudades) || !nrow(ciudades)) ciudades <- data.frame(codigo = LOCALES_EXT$codigo, ciudad = NA_character_, stringsAsFactors = FALSE)
  cat(sprintf("Leyendo %d ciudades del exterior", nrow(ciudades)))
  ext_c <- list(); ext_r <- list()
  for (i in seq_len(nrow(ciudades))) {
    if (i %% 10 == 0) cat(".")
    r <- leer_resultado(ELE_FEDERAL, "zz", 1, mun = ciudades$codigo[i])
    if (is.null(r)) next
    ext_r[[length(ext_r) + 1]] <- r$resumen %>% mutate(ciudad = ciudades$ciudad[i])
    ext_c[[length(ext_c) + 1]] <- r$cands %>% mutate(codigo = ciudades$codigo[i])
  }
  cat("\n")
  if (length(ext_c)) {
    ext_r <- bind_rows(ext_r) %>% left_join(LOCALES_EXT, by = "codigo") %>% left_join(PAISES, by = "iso2")
    ext_c <- bind_rows(ext_c) %>% left_join(LOCALES_EXT, by = "codigo")
    sin_pais <- ext_r %>% filter(is.na(iso2))
    if (nrow(sin_pais)) cat("  Ciudades sin país asignado (agrégalas a LOCALES_EXT):", paste(sin_pais$codigo, sin_pais$ciudad, collapse = "; "), "\n")
    write.csv(ext_r %>% select(codigo, ciudad, iso2, pais, pct_secciones, validos, blancos, nulos, total),
              file.path(dir_corte, "exterior_ciudades.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    por_pais <- ext_c %>% filter(!is.na(iso2)) %>% group_by(iso2, numero) %>% summarise(nombre = first(nombre), votos = sum(votos), .groups = "drop")
    tot_pais <- ext_r %>% filter(!is.na(iso2)) %>% group_by(iso2, pais) %>% summarise(validos = sum(validos, na.rm = TRUE), total = sum(total, na.rm = TRUE), ciudades = n(), .groups = "drop")
    lider <- por_pais %>% group_by(iso2) %>% arrange(desc(votos), .by_group = TRUE) %>%
      summarise(numero_lider = first(numero), va_adelante = nombre_corto(first(numero), first(nombre)), votos_lider = first(votos),
                segundo = nombre_corto(nth(numero, 2), nth(nombre, 2)), votos_segundo = nth(votos, 2), .groups = "drop")
    ancho_pais <- por_pais %>% filter(numero %in% c("13", "22")) %>% mutate(col = ifelse(numero == "13", "votos_lula", "votos_flavio")) %>%
      select(iso2, col, votos) %>% pivot_wider(names_from = col, values_from = votos, values_fill = 0)
    ext_paises <- tot_pais %>% filter(validos > 0) %>% left_join(lider, by = "iso2") %>% left_join(ancho_pais, by = "iso2") %>%
      mutate(pct_lider = 100 * votos_lider / validos, pct_segundo = 100 * votos_segundo / validos) %>% arrange(desc(validos))
    write.csv(ext_paises, file.path(dir_corte, "exterior_paises.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    
    if (nrow(ext_paises)) {
      mundo <- map_data("world") %>% filter(region != "Antarctica")
      mundo$iso2 <- maps::iso.alpha(mundo$region, n = 2)
      mundo$iso2[mundo$region == "French Guiana"] <- "GF"
      mundo <- mundo %>% left_join(ext_paises %>% mutate(clave = clave_color(numero_lider)) %>% select(iso2, clave), by = "iso2")
      # un punto por país con resultados, en el centro de su polígono más grande, con tamaño según sus votos válidos
      centros <- mundo %>% filter(!is.na(clave)) %>% group_by(iso2, group) %>%
        summarise(x = mean(range(long)), y = mean(range(lat)), n = n(), .groups = "drop") %>%
        group_by(iso2) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(iso2, x, y)
      extra <- data.frame(iso2 = c("HK"), x = c(114.17), y = c(22.32))
      puntos <- ext_paises %>% mutate(clave = clave_color(numero_lider)) %>% left_join(bind_rows(centros, extra %>% filter(!iso2 %in% centros$iso2)), by = "iso2") %>% filter(!is.na(x))
      nl <- table(ext_paises$va_adelante)
      g4 <- ggplot() +
        geom_polygon(data = mundo, aes(x = long, y = lat, group = group), fill = "grey86", colour = "white", linewidth = 0.1) +
        geom_polygon(data = mundo %>% filter(!is.na(clave)), aes(x = long, y = lat, group = group, fill = clave), colour = "white", linewidth = 0.1) +
        geom_point(data = puntos, aes(x = x, y = y, size = validos, fill = clave), shape = 21, colour = "white", stroke = 0.6, show.legend = c(size = TRUE, fill = FALSE)) +
        scale_fill_manual(values = PALETA, name = "Va adelante") +
        scale_size_area(max_size = 9, labels = miles, name = "Votos válidos") +
        guides(size = guide_legend(override.aes = list(fill = "grey55"))) +
        coord_quickmap(xlim = c(-170, 180), ylim = c(-55, 80)) +
        labs(title = "Voto en el extranjero: quién va adelante en cada país",
             subtitle = paste0(paste(sprintf("%s en %d países", names(nl), as.integer(nl)), collapse = " · "), " · ", etiqueta_corte),
             caption = "Datos del TSE por ciudad (consulados), sumados por país. Países en gris: sin votos totalizados.") +
        tema + theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
      guardar(g4, "04_exterior_mapa.png", ancho = 11, alto = 6.2)
    }
  } else cat("  El TSE todavía no publica archivos por ciudad del exterior.\n")
}

# =============================================================================
# 4. Senado (dos escaños por estado)
# =============================================================================
sen_partidos <- NULL
if (INCLUIR_SENADO) {
  cat("Leyendo Senado")
  sen <- list()
  for (uf in names(UFS)) {
    cat(".")
    r <- leer_resultado(ELE_ESTATAL, uf, 5)
    if (is.null(r) || !nrow(r$cands) || !isTRUE(r$resumen$validos > 0)) next
    vac <- if (is.na(r$resumen$vacantes) || r$resumen$vacantes < 1) 2 else r$resumen$vacantes
    nombre_uf <- UFS[[uf]]
    sen[[uf]] <- r$cands %>% mutate(uf = uf, estado = nombre_uf, pct_secciones = r$resumen$pct_secciones, lugar = row_number(), candidato = titulo(nombre), en_escano = lugar <= vac) %>%
      filter(lugar <= vac + 1) %>% select(uf, estado, pct_secciones, lugar, candidato, partido, votos, pct, en_escano, electo)
  }
  cat("\n")
  if (length(sen)) {
    sen <- bind_rows(sen)
    write.csv(sen, file.path(dir_corte, "senado_estados.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    sen_partidos <- sen %>% filter(en_escano) %>% count(partido, name = "escanos") %>% arrange(desc(escanos))
    write.csv(sen_partidos, file.path(dir_corte, "senado_partidos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    g5 <- ggplot(sen_partidos, aes(x = escanos, y = reorder(partido, escanos))) +
      geom_col(fill = "#0b5d4e", width = 0.62) + geom_text(aes(label = escanos), hjust = -0.4, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Senado: quién va adelante en los 54 escaños en disputa",
           subtitle = sprintf("Dos primeros lugares de cada estado con votos (%d escaños) · %s", sum(sen_partidos$escanos), etiqueta_corte), x = NULL, y = NULL) +
      tema + theme(panel.grid.major.y = element_blank())
    guardar(g5, "05_senado_partidos.png", alto = max(4, 0.32 * nrow(sen_partidos) + 1.5))
    sen_definido <- all(sen$electo[sen$en_escano])
    herradura(sen_partidos, "Senado: escaños en disputa por partido",
              paste0(if (sen_definido) "Electos según el TSE" else "Según los dos primeros lugares de cada estado", "\n", etiqueta_corte),
              total = 54, archivo = "07_senado_herradura.png",
              pie = "Se renuevan 54 de los 81 escaños (dos por estado); los otros 27 senadores siguen en funciones y no aparecen aquí.")
  }
}

# =============================================================================
# 5. Cámara de Diputados (513 escaños)
# =============================================================================
cam_partidos <- NULL
if (INCLUIR_CAMARA) {
  cat("Leyendo Cámara de Diputados")
  cam <- list()
  for (uf in names(UFS)) {
    cat(".")
    r <- leer_resultado(ELE_ESTATAL, uf, 6)
    if (is.null(r) || !nrow(r$cands)) next
    cam[[uf]] <- r$cands %>% mutate(uf = uf, pct_secciones = r$resumen$pct_secciones, vacantes = r$resumen$vacantes)
  }
  cat("\n")
  if (length(cam)) {
    cam <- bind_rows(cam)
    cam_partidos <- cam %>% group_by(partido) %>% summarise(electos = sum(electo), votos_nominales = sum(votos), .groups = "drop") %>% arrange(desc(electos), desc(votos_nominales))
    write.csv(cam_partidos, file.path(dir_corte, "camara_partidos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    write.csv(cam %>% filter(electo) %>% mutate(candidato = titulo(nombre)) %>% select(uf, candidato, partido, votos), file.path(dir_corte, "camara_electos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    hay_electos <- sum(cam_partidos$electos) > 0
    gc <- cam_partidos %>% mutate(valor = if (hay_electos) electos else votos_nominales) %>% filter(valor > 0) %>% slice_max(valor, n = 20, with_ties = FALSE)
    g6 <- ggplot(gc, aes(x = valor, y = reorder(partido, valor))) +
      geom_col(fill = "#0b5d4e", width = 0.62) + geom_text(aes(label = miles(valor)), hjust = -0.2, size = 3.3) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = miles) +
      labs(title = if (hay_electos) sprintf("Cámara de Diputados: electos por partido (%d de 513 marcados por el TSE)", sum(cam_partidos$electos))
           else "Cámara de Diputados: votos nominales por partido (el TSE aún no marca electos)",
           subtitle = etiqueta_corte, x = NULL, y = NULL,
           caption = "Los escaños se reparten por estado con el cociente electoral; hasta que el TSE marca electos, los votos son solo una referencia.") +
      tema + theme(panel.grid.major.y = element_blank())
    guardar(g6, "06_camara_partidos.png", alto = max(4, 0.3 * nrow(gc) + 1.6))
    if (hay_electos) {
      herradura(cam_partidos %>% transmute(partido, escanos = electos), "Cámara de Diputados: electos por partido",
                paste0("Diputados que el TSE ya marca como electos\n", etiqueta_corte), total = 513, archivo = "08_camara_herradura.png",
                pie = "La mayoría absoluta son 257 escaños.")
    } else cat("  La herradura de la Cámara se dibuja cuando el TSE empiece a marcar diputados electos.\n")
  }
}

