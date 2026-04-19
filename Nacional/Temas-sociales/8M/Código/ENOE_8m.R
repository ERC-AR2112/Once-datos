#### ENOE
library(pacman)
p_load(foreign, tidyverse, survey, srvyr)

sdem <- read.dbf("ENOE_SDEMT425.dbf")
sdem_clean <- sdem %>%
  filter(R_DEF == "00", 
         C_RES %in% c("1", "3"), 
         as.numeric(as.character(EDA)) >= 15 & as.numeric(as.character(EDA)) <= 98)
doble_jornada <- sdem_clean %>%
  filter(CLASE2==1) %>% 
  group_by(SEX, DOMESTICO) %>%
  summarise(total = sum(as.numeric(as.character(FAC_TRI)))) %>% 
  mutate(porcentaje = total / sum(total) * 100)

doble_jornada %>% 
  filter(DOMESTICO==3) %>% 
  mutate(Sexo = if_else(SEX==2, "Mujer", "Hombre")) %>% 
  ggplot(aes(x=Sexo, y= porcentaje, fill= Sexo))+
  geom_col()+
  geom_text(aes(label = round(porcentaje, 2)), vjust = 1)+
  labs(y= "Porcentaje que trabaja y realiza quehaceres domesticos")+
  theme_light()+theme(legend.position = "none")

coe1 <- read.dbf("ENOE_COE1T425.dbf")
coe2 <- read.dbf("ENOE_COE2T425.dbf")
sdem_clean <- left_join(sdem_clean, coe1, by= c("CD_A", "CVE_ENT", "CON", "V_SEL", "N_HOG", "H_MUD", "N_REN"))

sdem_clean <- left_join(sdem_clean, coe2, by= c("CD_A", "CVE_ENT", "CON", "V_SEL", "N_HOG", "H_MUD", "N_REN"))

ingresos_mujeres <- sdem_clean %>%
  filter(CLASE2 == 1) %>% 
  group_by(SEX, ING7C) %>%
  summarise(cantidad = sum(as.numeric(as.character(FAC_TRI)))) %>%
  mutate(pct = cantidad / sum(cantidad) * 100)

ingresos_mujeres %>% 
  filter(ING7C != 6, ING7C != 7) %>% 
  mutate( Ingreso = factor(ING7C, levels =  c(1, 2, 3, 4, 5), labels = c ("Hasta un salario mínimo", "Más de 1 hasta 2 salarios mínimos", "Más de 2 hasta 3 salarios mínimos", "Más de 3 hasta 5 salarios mínimos", "Más de 5 salarios mínimos")),
          Sexo =  if_else(SEX == 2, "Mujer", "Hombre")) %>% ggplot(aes(x= Ingreso, y = pct, fill= Sexo)) + 
  geom_col(position= "dodge")
  

salud_mujeres <- sdem_clean %>%
  filter( CLASE2 == 1) %>%
  group_by(SEX, MEDICA5C) %>%
  summarise(total = sum(as.numeric(as.character(FAC_TRI)))) %>% mutate(pct = total/sum(total)*100)

salud_mujeres <- sdem_clean %>%
  filter( CLASE2 == 1) %>%
  group_by(SEX, MEDICA5C) %>%
  summarise(total = sum(as.numeric(as.character(FAC_TRI)))) %>% mutate(Porcentaje = total/sum(total)*100,
                                                                       Prestaciones = factor(MEDICA5C, levels = c(1, 2, 3, 4, 5), labels = c("Sin prestaciones", "Solo acceso a instituciones de salud", "Acceso a instituciones de salud
y otras prestaciones", "No tiene acceso a instituciones de
salud pero sí a otras prestaciones", "No especificado"))) 

salud_mujeres %>% 
  mutate(Sexo = if_else(SEX == 2, "Mujer", "Hombre") ) %>% 
  ggplot(aes(x= Prestaciones, y = Porcentaje, fill = Sexo))+
  geom_col(position = "dodge")+
  geom_text( aes(label= round(Porcentaje, 2)), vjust= 1)+
  theme_light()


OCUPACIÓN_mujeres <- sdem_clean %>%
  filter(CLASE2 == 1) %>% 
  group_by(SEX) %>%
  summarise(cantidad = sum(as.numeric(as.character(FAC_TRI)))) %>%
  mutate(pct = cantidad / sum(cantidad) * 100)


OCUPACIÓN_mujeres %>% 
  mutate(SEXO = if_else(SEX == 2, "Mujer", "Hombre")) %>% 
  ggplot(aes(x= SEXO, y = pct, fill = SEXO)) + 
  geom_col()+
  geom_text(aes(label = round(pct, 2)), vjust= 1)+
theme_light()+
  theme(legend.position = "none")+
  labs(y= "Porcentaje de personas ocupadas", x= "Sexo")
