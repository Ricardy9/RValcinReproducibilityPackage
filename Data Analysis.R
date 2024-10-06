
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