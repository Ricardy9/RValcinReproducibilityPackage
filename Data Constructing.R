# CONSTRUCCION DE 2 BASES , POBLACION DESPLAZADA Y DE COMPARACION

baseDesp <- base_ %>% 
  filter(AP4_11_10 == 1)  #### DESPLAZADA 

baseNDesp <- base_ %>% 
  filter(AP4_11_10 == 2)  ### DE COMPARACION 

#Ahora declaramos el diseño muestral

mydesign <- base_ %>%
  as_survey_design(ids=UPM,
                   strata=ESTRATO,
                   weights=FAC_ELE,
                   nest=T)
mydesign %>%
  group_by(DespForc) %>%
  summarise(Media = survey_mean(EDAD))

