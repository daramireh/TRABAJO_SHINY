#--------------------------- EDA TRANSFIYA ----------------------------#
## ------------------------- Librerias --------------------------------#
library(readxl)
library(tidyverse)
library(readr)


##----------------- BASE DE DATOS DE PRUEBAS TRANSFIYA ----------------#
df <- read_excel("C:/Users/DELL/OneDrive - SKITCONSULTING LTDA/SKIT KONCILIA/DASH/TRABAJO_SHINY/TRANSFIYA.xlsx")
ACH <- read_excel("C:/Users/DELL/OneDrive - SKITCONSULTING LTDA/SKIT KONCILIA/DASH/TRABAJO_SHINY/TODO_ACH1.xlsx")

df$FECHAPROCESO = as.factor(df$FECHAPROCESO)
ACH$FECHAPROCESO = as.factor(ACH$FECHAPROCESO)


ACH$ORIGEN = ifelse(ACH$ENTIDADORIGEN == 1032, 1, 0)
ACH$DESTINO = ifelse(ACH$ENTIDADFIN == 32, 1, 0)
### ------------------- RESUMEN CANAL TRANSFIYA -----------------------#
resumen = df %>% group_by(FECHAPROCESO, TIPOTX, AREA) %>%
  summarise(valtransaccion = mean(VALORTRANSACCION),
            transacciones = n(),
            consistentes = sum(ESCONSISTENTE))

resumen = resumen %>%
  rename(CANAL = AREA)

### ------------------- RESUMEN CANALES ACH & PSE---------------------#
resu_ACH = ACH %>% 
  group_by(FECHAPROCESO, TIPOTX, CANAL, ORIGEN, DESTINO) %>%
  summarise(valtransaccion = mean(VALORTRANSACCION),
            transacciones = n(),
            consistentes = sum(ESCONSISTENTE))

###----------------------- ENTIDADES ----------------------

ACH %>%
  group_by(ENTIDADORIGEN)%>%
  summarise(val = sum(VALORTRANSACCION),
            trans = n(),
            consistentes = sum(ESCONSISTENTE))


ACH %>%
  group_by(ENTIDADFIN)%>%
  summarise(val = sum(VALORTRANSACCION),
            trans = n(),
            consistentes = sum(ESCONSISTENTE))


ACH %>%
  group_by(ENTIDADORIGEN, ENTIDADFIN)%>%
  summarise(val = sum(VALORTRANSACCION),
            trans = n(),
            consistentes = sum(ESCONSISTENTE))

### ------------------ UNIFICANDO TABLAS ----------------------------#
PROCESO = rbind(resumen, resu_ACH)
### -------- AGREGA PONDERACION DE VALORES Y TRANSACCIONES-----------#
PROCESO = PROCESO %>%
  mutate(conciliado = consistentes / transacciones * 100,
         peso_val = valtransaccion / sum(PROCESO$valtransaccion) * 100,
         peso_trans = transacciones / sum(PROCESO$transacciones) * 100)

###-------------- ESTADO DE CONCILIACION DEL BANCO ------------------#
BANCO = PROCESO %>% 
  group_by(CANAL) %>%
  summarise(valor = sum(valtransaccion),
            trans = sum(transacciones),
            consistentes = sum(consistentes)) %>%
  mutate(conciliado = consistentes / trans * 100,
         peso_val = valor / sum(PROCESO$valtransaccion) * 100,
         peso_trans = trans / sum(PROCESO$transacciones) * 100)%>%
  mutate(ponderado_val = conciliado * (peso_val/100),
         ponderado_trans = conciliado * (peso_trans/100))
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

###-------------- ESTADO DE CONCILIACION DEL BANCO POR FECHA --------#
BANCO_FECHA = PROCESO %>% 
  group_by(FECHAPROCESO, CANAL) %>%
  summarise(valor = sum(valtransaccion),
            trans = sum(transacciones),
            consistentes = sum(consistentes)) %>%
  mutate(conciliado = consistentes / trans * 100,
         peso_val = valor / sum(PROCESO$valtransaccion) * 100,
         peso_trans = trans / sum(PROCESO$transacciones) * 100)%>%
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

write.csv(PROCESO, 'PROCESO.csv')
write.csv(BANCO, 'BANCO.csv')
write.csv(BANCO_FECHA, 'BANCO_FECHA.csv')
write.csv(ACH_RESUMEN, 'ACH_RESUMEN.csv')
write.csv(ACH_FECHA, 'ACH_FECHA.csv')


## ------------------------ VISUALIZACIONES -----------------------#
ggplot(PROCESO, aes(x = CANAL, 
                y = transacciones)) + 
  geom_bar(width = 0.9, 
           stat = "identity",
           position = position_dodge()) +  
  # ylim(c(0,1000))+ 
  # labs(x = "Bienestar afectado", 
  #      y = "Frecuencias \n (Porcentajes)") +
  # labs(fill = "Bienestar afectado") +
  # geom_text(aes(label=paste0(Total,"\n", "", "(", Porcentaje, "%", ")")),
  #           vjust=-0.9, 
  #           color="black", 
  #           hjust=0.5,
  #           position = position_dodge(0.9),  
  #           angle=0, 
  #           size=4.0)+
  # scale_x_discrete('Bienestar afectado', labels = c('No', 
  #                                                   'Sí')) +
  # theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1)) +      
  # theme_bw(base_size = 16) +                                              
  # facet_wrap(~"Estudiantes por bienestar afectado por COVID")  

ggplot(data = ACH, aes(x = CANAL, y = VALORTRANSACCION))+
  stat_boxplot(geom = "errorbar", # Bigotes
               width = 0.2) +
  geom_boxplot(alpha = 0.9, outlier.colour = "red") +
  geom_jitter(position=position_jitter(0.2),
              shape=21,
              fill = '#90B1DB',
              color = '#90B1DB',
              alpha = .5)+
  scale_y_continuous(name = "EDAD") +  # Etiqueta de la variable continua
  scale_x_discrete(name = "BIENESTAR AFECTADO POR COVID") +        # Etiqueta de los grupos
  ggtitle("Distribución de la edad de los estudiantes") + # Título del plot
  theme_update(plot.title = element_text(hjust = 0.5))+
  theme_classic()














