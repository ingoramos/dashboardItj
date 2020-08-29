#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
library(plyr)
library(tidyverse)
#library(caret)
library(chron)
library(plotly)
library(readxl)
library(dygraphs)
library(xts)
library(lubridate)
library(chron)
library(reshape2)
#library(RCurl)
#library(gganimate)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(ggridges)

library(fmsb)

library(leaflet)
library(ggExtra)
library(ggpol)

library(treemap)
library(d3treeR)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Covid-19 Itajaí"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Óbitos/UTI's", tabName = "dashboard2", icon = icon("dashboard")),
            menuItem("Mapa", tabName = "map", icon = icon("map-marker-alt", lib = "font-awesome")),
            menuItem("Dados", tabName = "dados", icon = icon("table", lib = "font-awesome")),
            menuItem("Contato", tabName = "contato", icon = icon("send",lib="glyphicon"))
        )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        valueBoxOutput("casos_confirmados"),
                        valueBoxOutput("casos_descartados")
                    ),
                    
                    fluidRow(
                        valueBoxOutput("casos_ativos"),
                        valueBoxOutput("casos_curados"),
                        valueBoxOutput("casos_obitos"),
                    ),
                    
                    fluidRow(
                        box(dygraphOutput("plot1", height = 480)),
                        box(selectInput("lineVars", label = "Selecione", choices = list("Casos Ativos" = "casos_ativos", "Novos Casos Confirmados" = "casos_confirmados_total",
                                                                                        "Novos Casos Curados" = "casos_curados_total", "Novos Casos Descartados" = "casos_descartados_total",
                                                                                        "Óbitos" = "mortes_total"), selected = 1),
                            plotlyOutput("linePlot", height = 400))
                    ),
                    
                    fluidRow(
                        box(selectInput("vars", label = "Selecione", choices = list("Mês" = "mes", "Semana" = "semana_ano"), selected = 1),
                            plotOutput("plot2", height = 400)),
                        box(selectInput("numVars", label = "Selecione", choices = list("Casos Ativos" = "casos_ativos", "Novos Casos Confirmados" = "casos_confirmados_novos",
                                                                                       "Novos Casos Curados" = "casos_curados_novos", "Novos Casos Descartados" = "casos_descartados_novos",
                                                                                       "Óbitos" = "mortes_novos"), selected = 1),
                            plotlyOutput("plot3", height = 400))
                    )
            ),
            
            
            tabItem(tabName = "dashboard2",
                    fluidRow(
                        box(selectInput("varsObito", label = "Selecione", choices = list("Mês" = "mes", "Semana" = "semana_ano"), selected = 1)),
                        valueBoxOutput("internacoes")
                    ),
                    fluidRow(
                        box(plotlyOutput("plot4", height = 400)),
                        box(plotlyOutput("pyramidPlot", height = 400))
                    ),
                    fluidRow(
                        box(d3tree3Output("treePlot"), height = 400),
                        box(plotOutput("donutPlot", height = 400))
                    ),
                    
            ),
            
            tabItem(tabName = "map",
                    
                    
                    fluidRow(
                        tags$style(type = "text/css", "#plotMapa {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("plotMapa", height = "100%")
                    )
                    
                    
            ),
            
            
            tabItem(tabName = "dados",
                    fluidRow(h1("Dados usados para o desenvolvimento do dashboard")),
                    fluidRow(tags$br("Os dados serão atualizados diarimente, os dados são extraídos dos boletins diários do município")),
                    fluidRow(
                        dataTableOutput("tabela"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        
                    )),
            
            
            tabItem(tabName = "contato",
                    h2("Ingo Ramos"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    coronaData <- reactive({
        
        corona <- read_csv2("https://raw.githubusercontent.com/ingoramos/dashboardItj/master/covid_Itajai.csv",  locale = locale(encoding = 'LATIN1'))
        corona$data <- as.Date(corona$data, format='%d/%m/%Y')
        corona$report_time <- format(as.POSIXct(corona$report_time,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
        
        corona['date_time'] <- as.POSIXct(paste(corona$data, corona$report_time), format="%Y-%m-%d %H:%M:%S")
        
        corona$semana_ano <- NA
        
        for (i in 1:nrow(corona)){
            corona$semana_ano[i] <- paste0("Semana ", lubridate::week(ymd(corona$data[i])))
        }
        return(corona)
    })
    
    output$casos_confirmados <- renderValueBox({
        
        corona <- coronaData()
        
        confirmados <- tail(corona$casos_confirmados_total, 1) / tail(corona$exames_realizados_total, 1) * 100
        confirmados <- round(confirmados, digits = 2)
        
        valueBox(
            paste0(confirmados, "%"), "Casos Confirmados (em relação ao número de exames)", icon = icon("head-side-cough", lib = "font-awesome"),
            color = 'yellow'
        )
    })
    
    output$casos_descartados <- renderValueBox({
        
        corona <- coronaData()
        
        descartados <- tail(corona$casos_descartados_total, 1) / tail(corona$exames_realizados_total, 1) * 100
        descartados <- round(descartados, digits = 2)
        
        valueBox(
            paste0(descartados, "%"), "Casos Descartados (em relação ao número de exames)", icon = icon("head-side-cough-slash", lib = "font-awesome"),
            color = 'blue'
        )
    })
    
    output$casos_ativos <- renderValueBox({
        
        corona <- coronaData()
        
        ativos <- tail(corona$casos_ativos, 1) / tail(corona$casos_confirmados_total, 1) * 100
        ativos <- round(ativos, digits = 2)
        
        valueBox(
            paste0(ativos, "%"), "Casos Ativos (em relação aos casos confirmados)", icon = icon("lungs-virus", lib = "font-awesome"),
            color = 'red'
        )
    })
    
    output$casos_curados <- renderValueBox({
        
        corona <- coronaData()
        
        curados <- tail(corona$casos_curados_total, 1) / tail(corona$casos_confirmados_total, 1) * 100
        curados <- round(curados, digits = 2)
        
        valueBox(
            paste0(curados, "%"), "Casos Curados (em relação aos casos confirmados)", icon = icon("lungs", lib = "font-awesome"),
            color = 'green'
        )
    })
    
    output$casos_obitos <- renderValueBox({
        
        corona <- coronaData()
        
        obitos <- tail(corona$mortes_total, 1) / tail(corona$casos_confirmados_total, 1) * 100
        obitos <- round(obitos, digits = 2)
        
        valueBox(
            paste0(obitos, "%"), "Óbitos (em relação aos casos confirmados)", icon = icon("skull-crossbones", lib = "font-awesome"),
            color = 'black'
        )
    })
    
    dyReactive <- reactive({
        corona <- coronaData()
        corona$date_time <- ymd_hms(corona$date_time)
        don <- xts(x = corona$casos_ativos, order.by = corona$date_time)
        return(don)
    })
    
    
    output$plot1 <- renderDygraph({
        
        p <- dygraph(dyReactive(), main = "Casos Ativos por Boletim Epidemiológico") %>%
            dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
            dyRoller(rollPeriod = 1)
        
        p
    })
    
    output$linePlot <- renderPlotly({
        
        lineData <- coronaData()
        
        x2 <- as.character(input$lineVars)
        x2 <- strsplit(x2, "_")
        if (input$lineVars == "mortes_total"){
            x2 <- paste0("Óbitos")
        }
        else {
            x2 <- paste0(x2[[1]][1], " ", x2[[1]][2])
        }
        
        lp <- ggplot(lineData, aes_string(x="data", y=input$lineVars)) +
            geom_area(fill="#69b3a2", alpha=0.4) +
            geom_line(color="#69b3a2", size=2) +
            geom_point(size=3, color="#69b3a2") +
            theme_ipsum() +
            ggtitle(paste0("Evolução de ", x2))
        
        ggplotly(lp)
        
    })
    
    mapaData <- reactive({
        
        mapa <- read.csv("https://raw.githubusercontent.com/ingoramos/dashboardItj/master/covid_bairros.csv", encoding = "UTF-8")
        
        mapa$type <- NA
        
        for (i in 1:nrow(mapa)){
            if(mapa$num_casos[i] <= mean(mapa$num_casos)){mapa$type[i] <- 'abaixo'}
            else {mapa$type[i] <- 'acima'}
        }
        return(mapa)
    })
    
    output$plotMapa <- renderLeaflet({
        
        pal <- colorFactor(c("navy", "red"), domain = c("abaixo", "acima"))
        
        titulo <- tags$h4(
            HTML("Casos Acumulados por Bairro")
        )
        
        leaflet(mapaData()) %>% 
            setView(lng = -48.751037, lat = -26.975853, zoom = 12) %>%
            addTiles() %>%
            addCircleMarkers(~longitude, ~latitude, label=~as.character(paste0(bairro, " - ", num_casos, " casos totais")),
                             radius = ~ifelse(type == "acima", 12, 8),
                             color = ~pal(type),
                             stroke = FALSE, fillOpacity = 0.5
            ) %>%
            addControl(titulo, position = "bottomright")
    })
    
    output$plot2 <- renderPlot({
        
        x1 <- as.character(input$numVars)
        x1 <- strsplit(x1, "_")
        x1 <- paste0(x1[[1]][1], " ", x1[[1]][2])
        
        y1 <- as.character(input$vars)
        y1 <- strsplit(y1, "_")
        if (input$vars == "semana_ano"){
            y1 <- paste0(y1[[1]][1], " ", y1[[1]][2])
        }
        else{
            y1 <- paste0(y1[[1]])
        }
        
        p <- ggplot(coronaData(), aes_string(x = input$numVars, y = input$vars, fill = "..x..")) +
            geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
            scale_fill_viridis(name = "Casos Ativos", option = "C") +
            labs(title = paste0(x1, " por ", y1)) +
            theme_ipsum() +
            theme(
                legend.position="none",
                panel.spacing = unit(0.1, "lines"),
                strip.text.x = element_text(size = 8)
            )
        p
    })
    
    output$plot3 <- renderPlotly({
        bp <- coronaData() %>%
            ggplot( aes_string(x=input$vars, y=input$numVars, fill=input$vars)) +
            geom_boxplot() +
            scale_color_viridis(discrete = TRUE, alpha=0.6) +
            geom_jitter(color="black", size=0.4, alpha=0.9) +
            theme_ipsum() +
            theme(
                legend.position="none",
                plot.title = element_text(size=11,),
                axis.text.x = element_text(angle = 45, vjust = 1,
                                           size = 12, hjust = 1, family = "serif")
            ) +
            ggtitle("Boxplot com indivíduos") +
            xlab("")
        
        ggplotly(bp)
    })
    
    marietaData <- reactive({
        
        marieta <- read_csv2("https://raw.githubusercontent.com/ingoramos/dashboardItj/master/marieta.csv",  locale = locale(encoding = 'LATIN1'))
        return(marieta)
    })
    
    output$internacoes <- renderValueBox({
        
        marieta <- marietaData()
        
        internados <- tail(marieta$internacoes_uti, 1) / tail(marieta$total_vagas_uti, 1) * 100
        internados <- round(internados, digits = 2)
        
        valueBox(
            paste0(internados, "%"), "(Ocupação dos Leitos no Hospital Marieta)", icon = icon("hospital-user", lib = "font-awesome"),
            color = 'maroon'
        )    
    })
    
    
    obitosData <- reactive({
        
        #locale = locale(encoding = 'LATIN1')
        
        obitos <- read_csv2("https://raw.githubusercontent.com/ingoramos/dashboardItj/master/obitos_covid.csv",  locale = locale(encoding = 'LATIN1'))
        obitos$num <- 1
        
        for(i in 1:nrow(obitos)){
            if(obitos$genero[i] == "F"){obitos$genero[i] <- "Feminino"}
            else {obitos$genero[i] <- "Masculino"}
        }
        
        obitos$semana_ano <- NA
        
        obitos$data <- as.Date(obitos$data, format='%d/%m/%Y')
        
        for (i in 1:nrow(obitos)){
            obitos$semana_ano[i] <- paste0("Semana ", lubridate::week(ymd(obitos$data[i])))
        }
        
        obitos$faixa_etaria <- NA
        
        for(i in 1:nrow(obitos)){
            if(obitos$idade[i] > 0 & obitos$idade[i] <= 2){obitos$faixa_etaria[i] <- "Até 2 anos"}
            else if(obitos$idade[i] > 2 & obitos$idade[i] <= 5){obitos$faixa_etaria[i] <- "Entre 2 e 5"}
            else if(obitos$idade[i] > 5 & obitos$idade[i] <= 10){obitos$faixa_etaria[i] <- "Entre 6 e 10"}
            else if(obitos$idade[i] > 10 & obitos$idade[i] <= 15){obitos$faixa_etaria[i] <- "Entre 11 e 15"}
            else if(obitos$idade[i] > 15 & obitos$idade[i] <= 25){obitos$faixa_etaria[i] <- "Entre 16 e 25"}
            else if(obitos$idade[i] > 25 & obitos$idade[i] <= 35){obitos$faixa_etaria[i] <- "Entre 26 e 35"}
            else if(obitos$idade[i] > 35 & obitos$idade[i] <= 45){obitos$faixa_etaria[i] <- "Entre 36 e 45"}
            else if(obitos$idade[i] > 45 & obitos$idade[i] <= 55){obitos$faixa_etaria[i] <- "Entre 46 e 55"}
            else if(obitos$idade[i] > 55 & obitos$idade[i] <= 60){obitos$faixa_etaria[i] <- "Entre 56 e 60"}
            else if(obitos$idade[i] > 60 & obitos$idade[i] <= 65){obitos$faixa_etaria[i] <- "Entre 61 e 65"}
            else if(obitos$idade[i] > 65 & obitos$idade[i] <= 70){obitos$faixa_etaria[i] <- "Entre 66 e 70"}
            else if(obitos$idade[i] > 70 & obitos$idade[i] <= 75){obitos$faixa_etaria[i] <- "Entre 71 e 75"}
            else if(obitos$idade[i] > 75 & obitos$idade[i] <= 80){obitos$faixa_etaria[i] <- "Entre 76 e 80"}
            else if(obitos$idade[i] > 80 & obitos$idade[i] <= 85){obitos$faixa_etaria[i] <- "Entre 81 e 85"}
            else if(obitos$idade[i] > 85 & obitos$idade[i] <= 90){obitos$faixa_etaria[i] <- "Entre 86 e 90"}
            else if(obitos$idade[i] > 90 & obitos$idade[i] <= 95){obitos$faixa_etaria[i] <- "Entre 91 e 95"}
            else if(obitos$idade[i] > 95 & obitos$idade[i] <= 100){obitos$faixa_etaria[i] <- "Entre 96 e 100"}
            else if(obitos$idade[i] > 100 & obitos$idade[i] <= 105){obitos$faixa_etaria[i] <- "Entre 101 e 105"}
            else {obitos$faixa_etaria[i] <- "Mais de 106 anos"}
        }
        
        return(obitos)
        
    })
    
    dataObito <- reactive({
        
        selectedVar <- input$varsObito
        #selectedVar <- as.character(selectedVar)
        
        return(selectedVar)
        
    })
    
    output$plot4 <- renderPlotly({
        
        if(dataObito() == "mes"){
            
            bpData <- obitosData() %>%
                group_by(mes, genero) %>%
                summarise(num = sum(num))
            
            bpData
            
            bpo <- ggplot(data = bpData, aes_string(x=input$varsObito, y="num", fill="genero"))+
                geom_bar(stat = "identity", position = position_dodge())+
                geom_text(aes(label=num), vjust=1.6, color="black",
                          position = position_dodge(0.9), size=4.5)+
                scale_fill_brewer(palette="Set2")+
                theme_minimal() +
                ggtitle("Óbitos Mês por Gênero")
            
            ggplotly(bpo)
        }
        
        else{
            bpData <- obitosData() %>%
                group_by(semana_ano, genero) %>%
                summarise(num = sum(num))
            
            bpData
            
            bpo <- ggplot(data = bpData, aes_string(x=input$varsObito, y="num", fill="genero"))+
                geom_bar(stat = "identity", position = position_dodge())+
                geom_text(aes(label=num), vjust=1.6, color="black",
                          position = position_dodge(0.9), size=4.5)+
                scale_fill_brewer(palette="Set2")+
                theme_minimal()+
                ggtitle("Óbitos Semana Ano por Gênero") +
                theme(
                    plot.title = element_text(size=11,),
                    axis.text.x = element_text(angle = 45, vjust = 1,
                                               size = 12, hjust = 1, family = "serif"))
            
            ggplotly(bpo)
        }
        
    })
    
    output$pyramidPlot <- renderPlotly({
        
        pp <- ggplot(data = obitosData(), aes(x = as.factor(faixa_etaria), fill = genero)) + 
            geom_bar(data = dplyr::filter(obitosData(), genero == "Masculino")) + 
            geom_bar(data = dplyr::filter(obitosData(), genero == "Feminino"), aes(y = ..count.. * (-1))) + 
            coord_flip() +
            scale_fill_brewer(palette = "Set2") +
            ggtitle("Óbitos por Faixa Etária por Gênero") +
            theme_bw()
        
        ggplotly(pp)
        
    })
    
    output$treePlot <- renderD3tree3({
        treeData <-obitosData() %>%
            group_by(mes, hospital, faixa_etaria, genero) %>%
            summarise(num = sum(num))
        
        treeData
        
        #p <- 
        #p
        
        d3tree3(treemap(treeData,
                        index=c("mes", "hospital", "faixa_etaria", "genero"),
                        vSize="num",
                        type="index",
                        palette = "Set2",
                        bg.labels=c("white"),
                        fontsize.labels=c(15,15,15,15),
                        title="TreeMap dos Óbitos",
                        align.labels=list(
                            c("center", "center"), 
                            c("right", "bottom")
                        )
        ),
        rootname = "TreeMap")
        
        #p_inter
    })
    
    output$donutPlot <- renderPlot({
        donut <- obitosData() %>%
            group_by(genero) %>%
            summarise(num = sum(num))
        
        donut
        
        donut$perc <- donut$num / sum(donut$num)
        
        donut$round_perc <- round(donut$perc * 100)
        
        donut$ymax <- cumsum(donut$perc)
        
        # Compute the bottom of each rectangle
        donut$ymin <- c(0, head(donut$ymax, n=-1))
        
        # Compute label position
        donut$labelPosition <- (donut$ymax + donut$ymin) / 2
        
        # Compute a good label
        donut$label <- paste0(donut$genero, "\n valor: ", donut$round_perc, "%")
        
        ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=genero)) +
            geom_rect() +
            geom_text( x=2, aes(y=labelPosition, label=label, color=genero), size=6) + # x here controls label position (inner / outer)
            scale_fill_brewer(palette="Set2") +
            scale_color_brewer(palette="Dark2") +
            coord_polar(theta="y") +
            xlim(c(-1, 4)) +
            theme_void() +
            ggtitle("Óbitos por Gênero Acumulados") +
            theme(legend.position = "none")
    })
    
    output$tabela <- renderDataTable({
        data.table::data.table(coronaData())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
