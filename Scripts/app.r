library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)

# TABLERO DESCRIPTIVO DE TRANSACCIONES INCONSISTENTES EN LA CONCILIACIÓN DEL PROCESO ACH DE TRANSFERENCIAS INTERBANCARIAS DE UNA ENTIDAD FINANCIERA ----

# 1. Data frames----
df_banco <-read.csv(url("https://raw.githubusercontent.com/daramireh/TRABAJO_SHINY/main/ACH_RESUMEN.csv"))
df_proceso <-read.csv(url("https://raw.githubusercontent.com/daramireh/TRABAJO_SHINY/main/ACH_FECHA.csv"))


# 2. Datos de los botones----

## 2.1. General ----
tot_n_tx <- sum(df_banco$trans)
tot_n_consistente <- sum(df_banco$consistentes)
tot_vr_tx <- sum(df_banco$valor)
tot_pc_concil <- tot_n_consistente / tot_n_tx * 100

## 2.2. ACH ----
df_proceso_ach <- df_banco %>% 
  filter(., CANAL %in% c("ACH"))
ach_n_tx <- sum(df_proceso_ach$trans)
ach_n_consistente <- sum(df_proceso_ach$consistentes)
ach_vr_tx <- sum(df_proceso_ach$valor)
ach_pc_concil <- ach_n_consistente / ach_n_tx * 100

## 2.3. PSE ----
df_proceso_pse <- df_banco %>% 
  filter(., CANAL %in% c("PSE"))
pse_n_tx <- sum(df_proceso_pse$trans)
pse_n_consistente <- sum(df_proceso_pse$consistentes)
pse_vr_tx <- sum(df_proceso_pse$valor)
pse_pc_concil <- pse_n_consistente / pse_n_tx * 100




# 3. Gráficos ----

df_proceso %>%
  select("FECHAPROCESO", "valor", "consistentes", "trans")%>%
  mutate(FECHAPROCESO=as.Date(FECHAPROCESO))%>%
  group_by(FECHAPROCESO)%>%summarise_each(funs = sum)-> df1
df1$pc_concil <- df1$consistentes / df1$trans * 100

## 3.1. General ----

### 3.1.1. Número de transacciones vs % Conciliaciones ----

gra<-plot_ly()

gra<-gra %>%
  add_trace(x=df1$FECHAPROCESO,y=df1$trans,
            type="scatter",
            name="Número transacciones",
            mode="lines")
ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "<b>Conciliación</b> \n en %")

gra<-gra%>%
  add_trace(x=df1$FECHAPROCESO,y=df1$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliaciones",
            mode="lines")

gra <- gra %>% 
  layout(
    title = "Relación del volumen de las transacciones con el porcentaje de conciliación", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Cantidad de transacciones</b>"))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )

### 3.1.2. Valor transacciones vs % conciliaciones ----
graf <-plot_ly()

graf <- graf%>%
  add_trace(x=df1$FECHAPROCESO, y=df1$valor,
            type="scatter",
            name="Valor transacciones",
            mode="lines")

ay <- list(tickfont = list(color = "black"),
           overlaying = "y",
           side = "right",
           title = "<b>Conciliación</b> \n en %")

graf<-graf %>%
  add_trace(x=df1$FECHAPROCESO,y=df1$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliaciones",
            mode="lines")


graf <- graf %>% 
  layout(
    title = "Relación del monto total de las transacciones con el porcentaje de conciliación", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Valor de transacciones</b> \n en millones de pesos"))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )


## 3.2. ACH ----

df_proceso %>%
  select("FECHAPROCESO","valor","consistentes","trans", "CANAL")%>%
  mutate(FECHAPROCESO = as.Date(FECHAPROCESO)) %>%
  group_by(FECHAPROCESO, CANAL) %>% 
  summarise_each(funs = sum) -> df_proceso_canal
df_proceso_canal$pc_concil <- df_proceso_canal$consistentes / df_proceso_canal$trans *100

df_proceso_canal %>% 
  filter(CANAL %in% "ACH") -> df_proceso_canal_ach


### 3.2.1. Número de transacciones vs % Conciliaciones ----

gra_ach<-plot_ly()

gra_ach<-gra_ach %>%
  add_trace(x=df_proceso_canal_ach$FECHAPROCESO,y=df_proceso_canal_ach$trans,
            type="scatter",
            name="Número transacciones",
            mode="lines")
ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "<b>Conciliación</b> \n en %")

gra_ach<-gra_ach%>%
  add_trace(x=df_proceso_canal_ach$FECHAPROCESO,y=df_proceso_canal_ach$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliaciones",
            mode="lines")

gra_ach <- gra_ach %>% 
  layout(
    title = "Relación del volumen de las transacciones con el porcentaje de conciliación", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Cantidad de transacciones</b>"))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )

### 3.2.2. Valor transacciones vs % conciliaciones ----
graf_ach <-plot_ly()

graf_ach <- graf_ach%>%
  add_trace(x=df_proceso_canal_ach$FECHAPROCESO, y=df_proceso_canal_ach$valor,
            type="scatter",
            name="Valor transacciones",
            mode="lines")

ay <- list(tickfont = list(color = "black"),
           overlaying = "y",
           side = "right",
           title = "<b>Conciliación</b> \n en %")

graf_ach<-graf_ach %>%
  add_trace(x=df_proceso_canal_ach$FECHAPROCESO,y=df_proceso_canal_ach$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliaciones",
            mode="lines")


graf_ach <- graf_ach %>% 
  layout(
    title = "Relación del monto total de las transacciones con el porcentaje de conciliación", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Valor de transacciones</b> \n en millones de pesos"))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )



## 3.3. PSE ----

df_proceso_canal %>% 
  filter(CANAL %in% "PSE") -> df_proceso_canal_pse


### 3.3.1. Número de transacciones vs % Conciliaciones ----

gra_pse<-plot_ly()

gra_pse<-gra_pse %>%
  add_trace(x=df_proceso_canal_pse$FECHAPROCESO,y=df_proceso_canal_pse$trans,
            type="scatter",
            name="Número transacciones",
            mode="lines")
ay <- list(
  tickfont = list(color = "black"),
  overlaying = "y",
  side = "right",
  title = "<b>Conciliación</b> \n en %")

gra_pse<-gra_pse%>%
  add_trace(x=df_proceso_canal_pse$FECHAPROCESO,y=df_proceso_canal_pse$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliaciones",
            mode="lines")

gra_pse <- gra_pse %>% 
  layout(
    title = "Relación del volumen de las transacciones con el porcentaje de conciliación", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Cantidad de transacciones</b>"))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )

### 3.3.2. Valor transacciones vs % conciliaciones ----
graf_pse <-plot_ly()

graf_pse <- graf_pse%>%
  add_trace(x=df_proceso_canal_pse$FECHAPROCESO, y=df_proceso_canal_pse$valor,
            type="scatter",
            name="Valor transacciones",
            mode="lines")

ay <- list(tickfont = list(color = "black"),
           overlaying = "y",
           side = "right",
           title = "<b>Conciliación</b> \n en %")

graf_pse<-graf_pse %>%
  add_trace(x=df_proceso_canal_pse$FECHAPROCESO,y=df_proceso_canal_pse$pc_concil,
            yaxis="y2",
            type="scatter",
            name="% Conciliaciones",
            mode="lines")


graf_pse <- graf_pse %>% 
  layout(
    title = "Relación del monto total de las transacciones con el porcentaje de conciliación", yaxis2 = ay,
    xaxis = list(title="Fechas"),
    yaxis = list(title="<b>Valor de transacciones</b> \n en millones de pesos"))%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  )



# 4. UI ----
ui <- dashboardPage(
  
  dashboardHeader(title = "Menú"),
  
  dashboardSidebar(sidebarMenu(
                     menuItem("Introducción", tabName = "intro"),
                     menuItem("General", tabName = "main"),
                     menuItem("Canal - ACH", tabName = "ach"),
                     menuItem("Canal - PSE", tabName = "pse")
                   )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "intro",
              h1(paste0("Tablero descriptivo de transacciones inconsistentes en la conciliación de transferencias interbancarias de una entidad financiera")),
              p("El proceso de conciliación bancaria consiste en validar y verificar que la información existente de las transacciones bancarias sea igual en todas las fuentes de información; así las cosas, es fundamental para entender el estado de cuentas de cada entidad financiera y, en particular, de las bancarias, toda vez que permite garantizar que las cifras sean exactas y coincidan con la realidad de la entidad."),

              p("ACH se refiere a las transferencias directas, es decir, aquellas que no son productos de compras sino de envíos directos de un banco a otro. Estas pueden ser realizadas vía app, plataforma web, cajero automático, oficina, etc. Por su parte PSE se refiere a las transferencias producto de una compra o pago en línea."),

              p("El objetivo del presente tablero observar el comportamiento a nivel general de las transferencias interbancarias y el porcentaje conciliado o cuadrado para cada canal comparándolos en número de transacciones y monto de las mismas."),

              p("Se espera encontrar que:"),

              p("1.	Los días de pago de salarios, es decir, quincenas (15 y 30 del mes), el volumen de transacciones y valores de estas alcanza los picos en el mes."),
              p("2.	Los días de pago de salarios, al ser los días pico de transacciones, el porcentaje de conciliación disminuye respecto de los días valle."),
              p("3.	Los días valle, se tienen los porcentajes de conciliación más altos del mes."),
              p("4.	El canal ACH es donde se realizan mayor número de transacciones."),
              p("5.	El canal PSE agrupa el mayor monto de las transacciones.")
      ),
              
            
      tabItem(tabName = "main",
              
       ## 4.1. GENERAL ----
       
         ### 4.1.1.  Botones ----
             fluidRow(
                column(4,
                       infoBoxOutput("main_n_tx_box", width = 12)),
                column(4,
                       infoBoxOutput("main_vr_tx_box", width = 12)),
                column(4,
                       infoBoxOutput("main_pc_concil_box", width = 12))
              ),
       

        ### 4.1.2. Gráficos ----
              fluidRow(
                column(1),
                column(5,
                       plotlyOutput("main_n_tx_concil_gph", height = "600px")),
                column(5,
                       plotlyOutput("main_vr_tx_concil_gph", height = "600px"))
              )
      ),
      
      
      ## 4.2. ACH ----

      tabItem(tabName = "ach",
              
              ### 4.2.1.  Botones ----
              fluidRow(
                column(4,
                       infoBoxOutput("ach_n_tx_box", width = 12)),
                column(4,
                       infoBoxOutput("ach_vr_tx_box", width = 12)),
                column(4,
                       infoBoxOutput("ach_pc_concil_box", width = 12))
              ),
              
              ### 4.2.2. Gráficos ----
              fluidRow(
                column(1),
                column(5,
                       plotlyOutput("ach_n_tx_concil_gph", height = "600px")),
                column(5,
                       plotlyOutput("ach_vr_tx_concil_gph", height = "600px"))
              )
              
      ),

            
      ## 4.3. PSE ----
       
      
      tabItem(tabName = "pse",

              ### 4.3.1.  Botones ----
              fluidRow(
                column(4,
                       infoBoxOutput("pse_n_tx_box", width = 12)),
                column(4,
                       infoBoxOutput("pse_vr_tx_box", width = 12)),
                column(4,
                       infoBoxOutput("pse_pc_concil_box", width = 12))
              ),  
              
              ### 4.3.2. Gráficos ----
              fluidRow(
                column(1),
                column(5,
                       plotlyOutput("pse_n_tx_concil_gph", height = "600px")),
                column(5,
                       plotlyOutput("pse_vr_tx_concil_gph", height = "600px"))
              )
              
      )
      
    )
  )  
)

# 5. Servidor ----
server <- function(input, output) {
  
  # Botones de GENERAL
  output$main_n_tx_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0(format(tot_n_tx, big.mark = ".", decimal.mark = ",", scientific = F)), style = "font-size: 160%;" ),
      tags$p("Número de transacciones"), 
      icon = icon("list"),
      color = "purple"
    )
  })  
  output$main_vr_tx_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0("$", format(tot_vr_tx, big.mark = ".", decimal.mark = ",", scientific = F)), style = "font-size: 160%;" ),
      "Valor de las transacciones", 
      icon = icon("list"),
      color = "purple",
      width = 4
    )
  })  
  output$main_pc_concil_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0(format(tot_pc_concil, format = "f", digits = 3, decimal.mark = ","), "%"), style = "font-size: 160%;" ),
      "Transacciones conciliadas", 
      icon = icon("list"),
      color = "purple"
    )
  })  

  # Botones de ACH
  output$ach_n_tx_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0(format(ach_n_tx, big.mark = ".", decimal.mark = ",", scientific = F), ""), style = "font-size: 160%;" ),
      tags$p("Número de transacciones"), 
      icon = icon("list"),
      color = "purple"
    )
  })  
  output$ach_vr_tx_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0("$", format(ach_vr_tx, big.mark = ".", decimal.mark = ",", scientific = F), ""), style = "font-size: 160%;" ),
      "Valor de las transacciones", 
      icon = icon("list"),
      color = "purple",
      width = 4
    )
  })  
  output$ach_pc_concil_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0(format(ach_pc_concil, format = "f", digits = 3, decimal.mark = ","), "%"), style = "font-size: 160%;" ),
      "Transacciones conciliadas", 
      icon = icon("list"),
      color = "purple"
    )
  })  

  # Botones de PSE
  output$pse_n_tx_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0(format(pse_n_tx, big.mark = ".", decimal.mark = ",", scientific = F), ""), style = "font-size: 160%;" ),
      tags$p("Número de transacciones"), 
      icon = icon("list"),
      color = "purple"
    )
  })  
  output$pse_vr_tx_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0("$", format(pse_vr_tx, big.mark = ".", decimal.mark = ",", scientific = F), ""), style = "font-size: 160%;" ),
      "Valor de las transacciones", 
      icon = icon("list"),
      color = "purple",
      width = 4
    )
  })  
  output$pse_pc_concil_box <- renderInfoBox({  
    infoBox(
      tags$p( paste0(format(pse_pc_concil, format = "f", digits = 3, decimal.mark = ","), "%"), style = "font-size: 160%;" ),
      "Transacciones conciliadas", 
      icon = icon("list"),
      color = "purple"
    )
  })  


  # Gráficos  
   
    ## MAIN 
  output$main_n_tx_concil_gph <- renderPlotly({
    gra
  })
  output$main_vr_tx_concil_gph <- renderPlotly({
    graf
  })
  
    # ACH
  output$ach_n_tx_concil_gph <- renderPlotly({
    gra_ach
  })
  output$ach_vr_tx_concil_gph <- renderPlotly({
    graf_ach
  })

    # PSE
  output$pse_n_tx_concil_gph <- renderPlotly({
    gra_pse
  })
  output$pse_vr_tx_concil_gph <- renderPlotly({
    graf_pse
  })
  
}



shinyApp(ui, server)
