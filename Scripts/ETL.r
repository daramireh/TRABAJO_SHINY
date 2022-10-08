#--------------------------- EDA TRANSFIYA ----------------------------#
## ------------------------- Librerias --------------------------------#
library(readxl)
library(tidyverse)
library(readr)


##----------------- BASE DE DATOS DE PRUEBAS TRANSFIYA ----------------#
ACH <- read_excel("C:/Users/DELL/OneDrive - SKITCONSULTING LTDA/SKIT KONCILIA/DASH/TRABAJO_SHINY/TODO_ACH1.xlsx")

ACH$FECHAPROCESO = as.factor(ACH$FECHAPROCESO)


ACH$ORIGEN = ifelse(ACH$ENTIDADORIGEN == 1032, 1, 0)
ACH$DESTINO = ifelse(ACH$ENTIDADFIN == 32, 1, 0)

### ------------------- RESUMEN CANALES ACH & PSE---------------------#
resu_ACH = ACH %>% 
  group_by(FECHAPROCESO, TIPOTX, CANAL, ORIGEN, DESTINO) %>%
  summarise(valtransaccion = mean(VALORTRANSACCION),
            transacciones = n(),
            consistentes = sum(ESCONSISTENTE))


#---------------------------------------------------------------------

ACH_RESUMEN = resu_ACH %>% 
  group_by(CANAL, ORIGEN, DESTINO) %>%
  summarise(valor = sum(valtransaccion),
            trans = sum(transacciones),
            consistentes = sum(consistentes)) %>%
  mutate(conciliado = consistentes / trans * 100,
         peso_val = valor / sum(resu_ACH$valtransaccion) * 100,
         peso_trans = trans / sum(resu_ACH$transacciones) * 100)%>%
  mutate(ponderado_val = conciliado * (peso_val/100),
         ponderado_trans = conciliado * (peso_trans/100))

#--------------------------------------------------------------------

ACH_FECHA = resu_ACH %>% 
  group_by(FECHAPROCESO, CANAL, ORIGEN, DESTINO) %>%
  summarise(valor = sum(valtransaccion),
            trans = sum(transacciones),
            consistentes = sum(consistentes)) %>%
  mutate(conciliado = consistentes / trans * 100,
         peso_val = valor / sum(resu_ACH$valtransaccion) * 100,
         peso_trans = trans / sum(resu_ACH$transacciones) * 100)%>%
  mutate(ponderado_val = conciliado * (peso_val/100),
         ponderado_trans = conciliado * (peso_trans/100))

###-----------------------------------------------------------
# exportar a .csv

write.csv(ACH_RESUMEN, 'ACH_RESUMEN.csv')
write.csv(ACH_FECHA, 'ACH_FECHA.csv')


