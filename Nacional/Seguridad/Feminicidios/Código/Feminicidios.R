library(readxl)
delitos_1525 <- read_excel("Estatal-Víctimas-2015-2025_feb2026.xlsx")
library(tidyverse)
library(flextable)
glimpse(delitos_1525)
delitos_1525 %>% count(`Subtipo de delito`)
homicidios_fem <- delitos_1525 %>% 
  filter(`Subtipo de delito`=="Feminicidio"| `Subtipo de delito` == "Homicidio doloso",
          Sexo=="Mujer")
homicidios_fem %>% 
  group_by(Año, `Subtipo de delito`) %>% 
  summarise("Total de homicidios femeninos" = sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre)) %>% 
  pivot_wider(names_from = `Subtipo de delito`, values_from = `Total de homicidios femeninos`) %>% 
  mutate("Total de muertes de mujeres por homicidio" = Feminicidio + `Homicidio doloso`) %>% 
  flextable()
homicidios_fem %>% 
  mutate(Año = as.character(Año)) %>% 
  group_by(Año, `Subtipo de delito`, Modalidad) %>% 
  summarise("Total de homicidios femeninos" = sum(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre)) %>% 
  pivot_wider(names_from = `Subtipo de delito`, values_from = `Total de homicidios femeninos`) %>% 
  mutate("Total de muertes de mujeres por homicidio" = Feminicidio + `Homicidio doloso`) %>% 
  flextable()

#### con base de delitos municiapal

library(readxl)
fem_mpal <- read_excel("Municipal-Delitos-2015-2025_feb2026.xlsx")
library(tidyverse)
fem_mpal %>% glimpse()

fem_mpal %>% 
  count(`Subtipo de delito`) %>% 
  print(n=Inf)

fem_mpal %>% count(`Tipo de delito`) %>% 
  print(n=Inf)

fem_mpal %>% count(`Bien jurídico afectado`) %>% 
  print(n=Inf)

fem_mpal %>% count(Entidad) %>% 
  print(n=Inf)

edomex_mpal <- fem_mpal %>% 
  filter(Entidad== "México")
gto_mpal <- fem_mpal %>% filter(Entidad=="Guanajuato")
