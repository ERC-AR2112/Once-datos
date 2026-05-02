install.packages("rgee")
library(rgee)
library(sf)


Sys.setenv(RETICULATE_PYTHON = "")
library(rgee)
library(reticulate)


Sys.setenv(RETICULATE_USE_UV = "0")
library(reticulate)
install_miniconda()
ee_install()

library(rgee)
library(sf)

ee_Initialize()



roi <- ee$Geometry$Point(c(-99.1332, 19.4326))$buffer(15000) 


landsat_collection <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2") %>%
  ee$ImageCollection$filterBounds(roi) %>%
  ee$ImageCollection$filterDate("2025-01-01", "2025-12-31") %>%
  ee$ImageCollection$filterMetadata("CLOUD_COVER", "less_than", 10)

# Factor de escala y convertir a Celsius
get_lst <- function(image) {
  temp_celsius <- image$select("ST_B10")$
    multiply(0.00341802)$
    add(149.0)$
    subtract(273.15)
  
  return(image$addBands(temp_celsius$rename("LST_Celsius")))
}


lst_processed <- landsat_collection$map(get_lst)
lst_median <- lst_processed$select("LST_Celsius")$median()$clip(roi)

Map$centerObject(roi, 11)
Map$addLayer(
  eeObject = lst_median, 
  visParams = list(min = 20, max = 45, palette = c("blue", "yellow", "red")),
  name = "Temperatura Superficial CDMX"
)


pt_cdmx <- ee$Geometry$Point(c(-99.1332, 19.4326))
pt_gdl <- ee$Geometry$Point(c(-103.3496, 20.6596))
pt_mty <- ee$Geometry$Point(c(-100.3161, 25.6866))


buffer_size <- 20000 # 20km en metros


ciudades_fc <- ee$FeatureCollection(list(
  ee$Feature(pt_cdmx$buffer(buffer_size), list(name = "CDMX")),
  ee$Feature(pt_gdl$buffer(buffer_size), list(name = "Guadalajara")),
  ee$Feature(pt_mty$buffer(buffer_size), list(name = "Monterrey"))
))


landsat_collection <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2") %>%
  ee$ImageCollection$filterBounds(ciudades_fc) %>%
  ee$ImageCollection$filterDate("2024-05-01", "2024-08-31") %>%
  ee$ImageCollection$filterMetadata("CLOUD_COVER", "less_than", 15) # Filtrar nubes


get_lst <- function(image) {
  # La banda ST_B10 contiene la info térmica
  temp_celsius <- image$select("ST_B10")$
    multiply(0.00341802)$
    add(149.0)$
    subtract(273.15)
  
  return(image$addBands(temp_celsius$rename("LST_Celsius")))
}


lst_median <- landsat_collection$map(get_lst)$select("LST_Celsius")$median()


visParams <- list(min = 25, max = 50, palette = c("blue", "yellow", "orange", "red"))


Map$centerObject(ciudades_fc, 5)


Map$addLayer(lst_median$clip(ciudades_fc), visParams, "Temperatura (LST)") +
  Map$addLayer(ciudades_fc, list(color = "black"), "Límites Metropolitanos")

get_rural_ring <- function(point) {
  core <- point$buffer(10000) # Núcleo urbano (10km)
  outer <- point$buffer(25000) # Límite rural (25km)
  return(outer$difference(core))
}



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


task_cdmx <- ee_image_to_drive(
  image = icu_cdmx,
  description = "Intensidad_ICU_CDMX_2024",
  folder = "Mapas_ICU_Metropolitanos", 
  region = pt_cdmx$buffer(15000),      
  scale = 30,                         
  crs = "EPSG:4326",                   
  maxPixels = 1e10
)


task_gdl <- ee_image_to_drive(
  image = icu_gdl,
  description = "Intensidad_ICU_GDL_2024",
  folder = "Mapas_ICU_Metropolitanos",
  region = pt_gdl$buffer(15000),
  scale = 30,
  crs = "EPSG:4326",
  maxPixels = 1e10
)


task_mty <- ee_image_to_drive(
  image = icu_mty,
  description = "Intensidad_ICU_MTY_2024",
  folder = "Mapas_ICU_Metropolitanos",
  region = pt_mty$buffer(15000),
  scale = 30,
  crs = "EPSG:4326",
  maxPixels = 1e10
)


task_cdmx$start()
task_gdl$start()
task_mty$start()

