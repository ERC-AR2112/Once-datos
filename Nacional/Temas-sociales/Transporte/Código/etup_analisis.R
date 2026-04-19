#Analisis de ENTUP
options(scipen = 999)
library(readr)
etup <- read_csv("etup_mensual_tr_cifra_1986_2025.csv")
library(tidyverse)
glimpse(etup)
etup %>% 
  filter(ID_ENTIDAD=="09") %>% 
  count(ANIO) %>% 
  print(n=Inf)

etup %>% 
  filter(ID_ENTIDAD== "09", ANIO>= 2024, ID_MES=="12") %>% 
  mutate(ANIO= as.factor(ANIO)) %>% 
  group_by(ANIO, TRANSPORTE) %>% 
  summarise(Total_pasajeros = sum(VALOR, na.rm = T)) %>% 
  rename( "AÑO" = ANIO) %>% 
  ggplot(aes(TRANSPORTE, Total_pasajeros, fill = AÑO)) +
  geom_col(position = "dodge")+
  geom_text(aes(label = Total_pasajeros), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,   
            size = 3.5)+
  labs(y= "Total de pasajero en diciembre", 
       x = "Medio de transporte")+
  theme_bw()

etup %>% 
  filter(ID_ENTIDAD== "09", ANIO>= 2021) %>% 
  select (ANIO, ID_MES, TRANSPORTE, VALOR) %>%
  filter(TRANSPORTE == "Cablebús") %>%
  group_by(ANIO, ID_MES) %>%
  summarise(Total_pasajeros = sum(VALOR, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Fecha = make_date(year = ANIO, month = ID_MES, day = 1)) %>% 
  ggplot(aes(x = Fecha, y = Total_pasajeros)) + 
  geom_line(color = "darkblue", size = 1) +
  geom_point() +
  scale_x_date(date_labels = "%B %Y",      
               date_breaks = "2 month") + 
  labs(title = "Evolución de pasajeros - Cablebús",
       x = "Año", 
       y = "Total de Pasajeros") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



etup %>% 
  filter(ID_ENTIDAD== "09", ANIO>= 2020) %>% 
  select (ANIO, ID_MES, TRANSPORTE, VALOR) %>%
  filter(TRANSPORTE == "Trolebús") %>%
  group_by(ANIO, ID_MES) %>%
  summarise(Total_pasajeros = sum(VALOR, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Fecha = make_date(year = ANIO, month = ID_MES, day = 1)) %>% 
  ggplot(aes(x = Fecha, y = Total_pasajeros)) + 
  geom_line(color = "darkblue", size = 1) +
  geom_point() +
  scale_x_date(date_labels = "%B %Y",      
               date_breaks = "2 month") + 
  labs(title = "Evolución de pasajeros - Trolebús",
       x = "Año", 
       y = "Total de Pasajeros") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Cablebus
library(readxl)
cablebus <- read_excel("cablebus.xlsx")
glimpse(cablebus)
cablebus %>% count(Línea)

diccionario_meses <- c("Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4, 
                       "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8, 
                       "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12)

cablebus %>%
  mutate(Mes_Num = diccionario_meses[mes],
         Fecha_Completa = make_date(year = anio, month = Mes_Num, day = 1)) %>%

  filter(!is.na(Fecha_Completa)) %>% 
  group_by(Línea, Fecha_Completa) %>%
  summarise(Afluencia_total = sum(afluencia, na.rm = TRUE)) %>%
  ggplot(aes(x = Fecha_Completa, y = Afluencia_total, color = Línea)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %y", date_breaks = "4 months") +
  labs(title = "Evolución Cablebús", x = "Fecha", y = "Afluencia") +
  theme_bw()


### Trolebús
trole <- read_excel("trole.xlsx")

glimpse(trole)

trole %>%
  mutate(Mes_Num = diccionario_meses[mes],
         Fecha_Completa = make_date(year = anio, month = Mes_Num, day = 1)) %>%
  filter(!is.na(Fecha_Completa)) %>% 
  group_by(linea, Fecha_Completa) %>%
  summarise(Afluencia_total = sum(afluencia, na.rm = TRUE)) %>%
  ggplot(aes(x = Fecha_Completa, y = Afluencia_total, color = linea)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %y", date_breaks = "4 months") +
  labs(title = "Evolución Trolebús", x = "Fecha", y = "Afluencia") +
  theme_bw()
