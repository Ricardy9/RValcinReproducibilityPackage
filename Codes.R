

###################

## PAQUETES USADOS 
library(dplyr)
library(ggplot2)
library(patchwork)
library(openxlsx)
library(broom)
library("srvyr")

### IMPORTACION DE LA BASE DE DATOS Y SELECCION DE VARIABLES 

# Importar la tabla TPer_Vic1 
t1 <- read.csv("TPer_Vic1.csv")

#. Inclusion de las variables de entidad y municipio, así como las variables del diseño muestral y el factor de expansión.....

base <- t1 %>% 
  select(UPM, SEXO, EDAD, NOM_ENT, NOM_MUN, AP4_11_10, 
         FAC_ELE, AP4_4_A, AP4_1, AP4_3_1, ESTRATO, FAC_HOG)

###  VARIABLES ELEGIDAS 
# Variabele cuantitativa : EDAD 
# Variable cualitativa dicotómica : SEXO (SEXO DE LA PERSONA), AP4_11_10( PERSONA EN SITUACION DE DESPLAZAMIENTO 
#FORZADO O NO )
#   AP4_1 (tiempo en la vivienda),
#   AP4_4_A (seguro(a) se siente al caminar solo(a) por la noche en los alrededores de su vivienda) 
#   Otra variable de percepcion de seguridad 
# AP4_3_1 : Sentirse seguro en la vivienda de residencia 

######################

### Cambio de los tipos de las variables 

str(base)
base$SEXO <- as.factor(base$SEXO)
base$NOM_ENT <- as.factor(base$NOM_ENT)
base$NOM_MUN  <- as.factor(base$NOM_MUN)
base$AP4_11_10 <- as.factor(base$AP4_11_10)
base$AP4_4_A <- as.factor(base$AP4_4_A)
base$AP4_1 <- as.factor(base$AP4_1)
base$AP4_3_1 <- as.factor(base$AP4_3_1)


### Analisis de casos faltantes 
VIM::aggr(base, col = c("darkblue", "gray"),
          numbers = TRUE, sortVars = TRUE,
          labels = names(base), cex.axis = .7,
          gap = 3, ylab = c("casos faltantes",
                            "estructura"))

### Renombrar las variables 
# Dado que esta variable es nuestra variable de interes, 
#Se ha destacado las personas que no respondieron 
base <- base %>% 
  filter(AP4_11_10 != "9") %>% 
  filter(AP4_4_A != 5 & AP4_4_A != 6)

base <- base %>% 
  filter(EDAD < 98)

base_ <- base %>% 
  mutate(SEXO = case_when(SEXO == 1 ~ "Hombre", SEXO == 2 ~ "Mujer"))

base_ <- base_ %>% 
  mutate(DespForc = case_when(AP4_11_10 == 1 ~ "Si", AP4_11_10 == 2 ~ "No"))

base_ <- base_ %>% 
  mutate(AP4_3_1 = case_when(AP4_3_1 == 1 ~ "1- Si", AP4_3_1 == 2 ~ "2- No", AP4_3_1 == 9 ~"3- No responde"))

table(base_$AP4_3_1)

base_ <- base_ %>% 
  mutate(SentirSeguro = case_when(AP4_4_A == 1 ~ "4-Muy seguro", AP4_4_A == 2 ~ "3-Seguro",
                                  AP4_4_A == 3 ~ "1-Inseguro", AP4_4_A == 4 ~ "2-Muy inseguro"))

table(base_$DespForc)


# Convertirse la variable AP4_1 en dicotómica.

base_ <- base_ %>% 
  mutate(Menos6 = ifelse(AP4_1 == 1, 1, 0))

base_ <- base_ %>% 
  mutate(Menos6Mes = case_when(Menos6 == "1"~ "Menos de 6 meses", Menos6 == "0" ~ "Mas de 6 meses" ))

table(base_$Menos6Mes)

# CONSTRUCCION DE 2 BASES , POBLACION DESPLAZADA Y DE COMPARACION

baseDesp <- base_ %>% 
  filter(AP4_11_10 == 1)  #### DESPLAZADA 

baseNDesp <- base_ %>% 
  filter(AP4_11_10 == 2)  ### DE COMPARACION 

### Perfil de las personas desplazadas 

par(mfrow = c(1, 2))
g1 <- baseDesp %>%  count(SEXO) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=SEXO, y = prop, fill = SEXO))+
  scale_y_continuous(labels=scales::percent) +
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(100*prop,2)), vjust = 2, colour = "white")+ 
  theme(legend.position="none") +
  labs(x= "Sexo",
       y= "Porcentaje",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Personas desplazadas por violencia")+
  scale_fill_manual(values = c("#53868B", "#2F4F4F")) 
  
g2 <- baseNDesp %>%  count(SEXO) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=SEXO, y = prop, fill = SEXO))+
  scale_y_continuous(labels=scales::percent) +
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(100*prop,2)), vjust = 2, colour = "white")+ 
  theme(legend.position="none") +
  labs(x= "Sexo",
       y= "Porcentaje",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Población de comparación")+
  scale_fill_manual(values = c("#53868B", "#2F4F4F")) 
g1
g2
g1 + g2

par(mfrow = c(1, 2))
g3 <- ggplot(baseDesp, aes(x = EDAD)) +
  geom_histogram(aes(color = SEXO, fill = SEXO), 
                 position = "identity", bins = 30, alpha = 0.4) +
  labs(caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Personas desplazadas por violencia")+
  scale_color_manual(values = c("#53868B", "#2F4F4F")) +
  scale_fill_manual(values = c("#53868B", "#2F4F4F")) 

g4 <- ggplot(baseNDesp, aes(x = EDAD)) +
  geom_histogram(aes(color = SEXO, fill = SEXO), 
                 position = "identity", bins = 30, alpha = 0.4) +
  labs(caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Población de comparación") +
  scale_color_manual(values = c("#53868B", "#2F4F4F")) +
  scale_fill_manual(values = c("#53868B", "#2F4F4F")) 
g3
g4
g3+g4

g5 <- ggplot(base_ , aes(x = DespForc , y = EDAD, fill = DespForc)) + geom_boxplot() + theme_bw() + 
  labs(x= "En situacion de desplazamiento forzado",
       y= "Edad",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Figura 2 :Distribución de edad segun que las personas sean desplazadas por violencia o no")+
scale_fill_manual(values = c("#53868B", "#2F4F4F"))
g5

# Distribucoion del tiempo en la vivienda actual segun que la persona sea desplazad o no 
table(base_$DespForc, base_$Menos6Mes)

# Percepcion sobre la seguridad 
# En términos de delincuencia, dígame ¿qué tan seguro(a) se siente al 
# caminar solo(a) por la noche en los alrededores de su vivienda?

g6 <- baseDesp %>%  count(SentirSeguro) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=SentirSeguro, y = prop, fill = "2"))+
  scale_y_continuous(labels=scales::percent) +
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(100*prop,2)), vjust = 2, colour = "white")+ 
  theme(legend.position="none") +
  labs(x= "Sentirse seguro(a)",
       y= "Porcentaje",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Personas desplazadas")+
  scale_fill_manual(values = c("#53868B", "#E7B800")) 

g7 <- baseNDesp %>%  count(SentirSeguro) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=SentirSeguro, y = prop, fill = "2"))+
  scale_y_continuous(labels=scales::percent) +
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(100*prop,2)), vjust = 2, colour = "white")+ 
  theme(legend.position="none") +
  labs(x= "Sentirse seguro(a)",
       y= "Porcentaje",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Población de comparación")+
  scale_fill_manual(values = c("#53868B", "#E7B800")) 
g6
g7
g6+ g7 

g8 <- baseNDesp %>%  count(AP4_3_1) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=AP4_3_1, y = prop, fill = "2"))+
  scale_y_continuous(labels=scales::percent) +
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(100*prop,2)), vjust = 2, colour = "white")+ 
  theme(legend.position="none") +
  labs(x= "Sentirse seguro en el lugar de residencia",
       y= "Porcentaje",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Población de comparacion")+
  scale_fill_manual(values = c("#53868B", "#E7B800"))  

g9 <- baseDesp %>%  count(AP4_3_1) %>% 
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=AP4_3_1, y = prop, fill = "2"))+
  scale_y_continuous(labels=scales::percent) +
  geom_col()+
  theme_classic()+
  geom_text(aes(label = round(100*prop,2)), vjust = 2, colour = "white")+ 
  theme(legend.position="none") +
  labs(x= "Sentirse seguro en el lugar de destino ",
       y= "Porcentaje",caption = "Fuente : Elaboración propia con los datos de la ENVIPE(2022)",
       title ="Personas desplazadas")+
  scale_fill_manual(values = c("#53868B", "#E7B800")) 
g8
g9
g8+g9

# Diferencia de medias entre la variable numérica y alguna variable dicotómica. 
# intervalo de confianza.

baseNdf <- base %>% 
  filter(AP4_11_10 == 2)

t2 <- tidy(t.test(x = baseDesp$EDAD, y=  baseNDesp$EDAD))
t2
# Estimacion del numero de personas desplazadas 
#Ahora declaramos el diseño muestral

mydesign <- base_ %>%
  as_survey_design(ids=UPM,
                   strata=ESTRATO,
                   weights=FAC_ELE,
                   nest=T)
mydesign %>%
  group_by(DespForc) %>%
  summarise(Media = survey_mean(EDAD))

#Tablas
library(survey)
svytable(~SEXO + DespForc , design = mydesign)

#Tablas
library(dplyr)
library(survey)
t1 <- dplyr::as_tibble(svytable(~DespForc + NOM_ENT, design = mydesign))

print(t1, n=64)

as.data.frame(t1)

write.xlsx(t1, 'despF2022.xlsx')

t2 <- dplyr::as_tibble(svytable(~DespForc + NOM_MUN, design = mydesign))

print(t2, n=64)

as.data.frame(t2)

write.xlsx(t2, 'despF2022M.xlsx')
