# Instalación (si no lo tienes)
install.packages("rgee")
library(rgee)
library(sf)

# Inicializar GEE (esto abrirá una pestaña en tu navegador)

Sys.setenv(RETICULATE_PYTHON = "")
library(rgee)
library(reticulate)

# Forzamos la creación de un entorno específico llamado 'rgee_env'
# Esto suele saltarse los bloqueos de rutas predeterminadas
Sys.setenv(RETICULATE_USE_UV = "0")
library(reticulate)
install_miniconda()
ee_install()

library(rgee)
library(sf)

# Inicializar GEE (esto abrirá una pestaña en tu navegador)
ee_Initialize()


# 1. Definir el área de interés (ROI) - Ejemplo: Centro de CDMX
roi <- ee$Geometry$Point(c(-99.1332, 19.4326))$buffer(15000) 

# 2. Cargar colección de Landsat 9 (Surface Temperature)
landsat_collection <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2") %>%
  ee$ImageCollection$filterBounds(roi) %>%
  ee$ImageCollection$filterDate("2025-01-01", "2025-12-31") %>%
  ee$ImageCollection$filterMetadata("CLOUD_COVER", "less_than", 10)

# 3. Función para aplicar factor de escala y convertir a Celsius
get_lst <- function(image) {
  temp_celsius <- image$select("ST_B10")$
    multiply(0.00341802)$
    add(149.0)$
    subtract(273.15)
  
  return(image$addBands(temp_celsius$rename("LST_Celsius")))
}

# 4. Procesar la colección y reducir a una imagen (Promedio)
lst_processed <- landsat_collection$map(get_lst)
lst_median <- lst_processed$select("LST_Celsius")$median()$clip(roi)

# 5. Visualizar rápidamente en el visualizador de GEE
Map$centerObject(roi, 11)
Map$addLayer(
  eeObject = lst_median, 
  visParams = list(min = 20, max = 45, palette = c("blue", "yellow", "red")),
  name = "Temperatura Superficial CDMX"
)

# 1. Definir los puntos centrales (Longitud, Latitud)
pt_cdmx <- ee$Geometry$Point(c(-99.1332, 19.4326))
pt_gdl <- ee$Geometry$Point(c(-103.3496, 20.6596))
pt_mty <- ee$Geometry$Point(c(-100.3161, 25.6866))

# 2. Crear buffers de 20 km alrededor de los centros (Abarca el área metropolitana de cada una)
buffer_size <- 20000 # 20km en metros

# 3. Crear una FeatureCollection con nombres para identificarlas después
ciudades_fc <- ee$FeatureCollection(list(
  ee$Feature(pt_cdmx$buffer(buffer_size), list(name = "CDMX")),
  ee$Feature(pt_gdl$buffer(buffer_size), list(name = "Guadalajara")),
  ee$Feature(pt_mty$buffer(buffer_size), list(name = "Monterrey"))
))

# 4. Cargar y filtrar la colección de Landsat 9 (Nivel 2, Temp. Superficial)
landsat_collection <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2") %>%
  ee$ImageCollection$filterBounds(ciudades_fc) %>%
  ee$ImageCollection$filterDate("2024-05-01", "2024-08-31") %>%
  ee$ImageCollection$filterMetadata("CLOUD_COVER", "less_than", 15) # Filtrar nubes

# 5. Función de conversión (Escala a Celsius)
get_lst <- function(image) {
  # La banda ST_B10 contiene la info térmica
  temp_celsius <- image$select("ST_B10")$
    multiply(0.00341802)$
    add(149.0)$
    subtract(273.15)
  
  return(image$addBands(temp_celsius$rename("LST_Celsius")))
}

# 6. Mapear función y calcular la mediana (ideal para mitigar nubes/anomalías)
lst_median <- landsat_collection$map(get_lst)$select("LST_Celsius")$median()

# 7. Parámetros de visualización (25 a 50 grados Celsius)
visParams <- list(min = 25, max = 50, palette = c("blue", "yellow", "orange", "red"))

# Centrar mapa en México
Map$centerObject(ciudades_fc, 5)

# Visualizar la capa base y las ciudades
Map$addLayer(lst_median$clip(ciudades_fc), visParams, "Temperatura (LST)") +
  Map$addLayer(ciudades_fc, list(color = "black"), "Límites Metropolitanos")
# Función para crear un anillo (buffer exterior - buffer interior)
get_rural_ring <- function(point) {
  core <- point$buffer(10000) # Núcleo urbano (10km)
  outer <- point$buffer(25000) # Límite rural (25km)
  return(outer$difference(core))
}


####Islas de calor como la diferencia con la zona rural
# Aplicar a tus puntos de CDMX, GDL y MTY
rural_cdmx <- get_rural_ring(pt_cdmx)
rural_gdl <- get_rural_ring(pt_gdl)
rural_mty <- get_rural_ring(pt_mty)

get_intensity_map <- function(lst_img, rural_area) {
  # 1. Calcular la media rural
  rural_mean_dict <- lst_img$reduceRegion(
    reducer = ee$Reducer$mean(),
    geometry = rural_area,
    scale = 30,
    maxPixels = 1e9
  )
  
  # 2. Extraer el valor como un número de GEE
  rural_mean_val <- ee$Number(rural_mean_dict$get("LST_Celsius"))
  
  # 3. Restar la media rural a todo el mapa
  intensity_map <- lst_img$subtract(rural_mean_val)
  
  return(intensity_map)
}

# Calcular intensidad para cada ciudad
icu_cdmx <- get_intensity_map(lst_median, rural_cdmx)$clip(pt_cdmx$buffer(15000))
icu_gdl <- get_intensity_map(lst_median, rural_gdl)$clip(pt_gdl$buffer(15000))
icu_mty <- get_intensity_map(lst_median, rural_mty)$clip(pt_mty$buffer(15000))
# Paleta de intensidad: de 0 (blanco/amarillo) a 10 grados de diferencia (rojo oscuro)
vis_icu <- list(
  min = 0, 
  max = 8, 
  palette = c("#ffffff", "#ffeb3b", "#f44336", "#b71c1c")
)

Map$centerObject(pt_cdmx, 10)
Map$addLayer(icu_cdmx, vis_icu, "Intensidad ICU CDMX") +
  Map$addLayer(icu_gdl, vis_icu, "Intensidad ICU Guadalajara") +
  Map$addLayer(icu_mty, vis_icu, "Intensidad ICU Monterrey")

# 1. Definir los parámetros de exportación para CDMX
task_cdmx <- ee_image_to_drive(
  image = icu_cdmx,
  description = "Intensidad_ICU_CDMX_2024",
  folder = "Mapas_ICU_Metropolitanos", # Carpeta que se creará en tu Google Drive
  region = pt_cdmx$buffer(15000),      # El polígono de corte
  scale = 30,                          # 30 metros (Resolución nativa de Landsat térmica)
  crs = "EPSG:4326",                   # Coordenadas geográficas estándar (WGS84)
  maxPixels = 1e10
)

# 2. Definir para Guadalajara
task_gdl <- ee_image_to_drive(
  image = icu_gdl,
  description = "Intensidad_ICU_GDL_2024",
  folder = "Mapas_ICU_Metropolitanos",
  region = pt_gdl$buffer(15000),
  scale = 30,
  crs = "EPSG:4326",
  maxPixels = 1e10
)

# 3. Definir para Monterrey
task_mty <- ee_image_to_drive(
  image = icu_mty,
  description = "Intensidad_ICU_MTY_2024",
  folder = "Mapas_ICU_Metropolitanos",
  region = pt_mty$buffer(15000),
  scale = 30,
  crs = "EPSG:4326",
  maxPixels = 1e10
)

# 4. Iniciar las tareas en los servidores de Google
task_cdmx$start()
task_gdl$start()
task_mty$start()

