

###################   DATA PROCESSING ######################
# This part consists of make the data analysis 

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


