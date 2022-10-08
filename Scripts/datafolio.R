library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library("png") 


#Base de datos ----

df<-read.csv(url("https://raw.githubusercontent.com/daramireh/TRABAJO_SHINY/main/ACH_FECHA.csv"))

df%>%select("FECHAPROCESO","valor","CANAL","conciliado","trans","consistentes")%>%
  mutate(FECHAPROCESO=as.Date(FECHAPROCESO))%>%
  group_by(FECHAPROCESO)%>%summarise(consistentes=sum(consistentes),
                                     trans=sum(trans),
                                     valor=sum(valor))->df1

df1$pc_concil <- (df1$consistentes/df1$trans)*100
df$pc_concil <- (df$consistentes/df$trans)*100

#Cantidad de transacciones vs transacciones conciliadas----
plt2<-plot_ly()

plt2<-plt2%>%add_trace(x=df1$FECHAPROCESO,y=df1$trans,
                     type="scatter",
                     name="transacciones",
                     mode="lines")
ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "<b>Porcentaje de conciliación</b>")

plt2<-plt2%>%
  add_trace(x=df1$FECHAPROCESO,y=df1$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliación",
            mode="lines")


plt2 <- plt2 %>% 
  layout(
    title = "Número de transacciones vs transacciones conciliadas", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Cantidad de transacciones</b>"
               ))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         legend=(list(orientation = 'h',
                      xanchor="center",
                      x=0.5))
  )

plt2



#Monto de transacciones vs transacciones conciliadas----


plt1<-plot_ly()

plt1<-plt1%>%add_trace(x=df1$FECHAPROCESO,y=df1$valor,
                       type="scatter",
                       name="valor",
                       mode="lines")
ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "<b>Porcentaje</b>")

plt1<-plt1%>%add_trace(x=df1$FECHAPROCESO,y=df1$pc_concil,
                       yaxis="y2",
                       type="scatter",
                       name="% Conciliación",
                       mode="lines")


plt1 <- plt1 %>% layout(
  title = "Monto total de transacciones vs transacciones conciliadas", yaxis2 = ay,
  xaxis = list(title="Fechas"),
  yaxis = list(title="<b>Monto total de transacciones</b>")
)%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         legend = list(orientation = 'h',
                       xanchor="center",
                       x=0.5))

plt1

#Conciliacion por Canal----

df%>%select("FECHAPROCESO","CANAL","consistentes","trans")%>%
  mutate(FECHAPROCESO=as.Date(FECHAPROCESO))%>%
  group_by(FECHAPROCESO,CANAL)%>%summarise_each(funs = sum)->dff
dff$pc_concil <- (dff$consistentes/dff$trans)*100

ggplot(dff,aes(x = FECHAPROCESO, y = pc_concil, 
               color = CANAL))+
  geom_line(size=.9)+
  labs(x = "Fecha",
       y = "Porcentaje de conciliación",
       title = "Porcentaje de conciliación por canal")+
  scale_y_continuous(name="Porcentaje de conciliación",
                     limits=c(60, 100))+
  theme_minimal()->plt3

plt3

