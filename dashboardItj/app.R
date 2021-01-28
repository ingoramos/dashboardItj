#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Pacotes usados para a criação do Dashboard
#leitura e transformação dos dados
library(readr)
library(plyr)
library(dplyr)
library(chron)
library(xts)
library(lubridate)
library(reshape2)
#criação do dashbard
library(shiny)
library(shinydashboard)
#e gráficos
library(ggplot2)
library(plotly)
library(dygraphs)
library(leaflet)
#extras para os gráficos
library(latticeExtra)
library(viridis)
library(ggridges)
library(ggExtra)
library(ggpol)
#library(fmsb)
#gráfico de árvore
library(treemap)
library(d3treeR)

#mensagem de erro do treeplot
#options(shiny.sanitize.errors = TRUE)

# Define a UI para os gráficos.
ui <- dashboardPage(
    #titulo do dashboard
    dashboardHeader(title = "Covid-19 Itajaí"),
    #tipo de dashboard que foi desenvolvido
    dashboardSidebar(
        #definindo o menu lateral com as páginas
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Óbitos/UTI's", tabName = "dashboard2", icon = icon("dashboard")),
            menuItem("Mapa", tabName = "map", icon = icon("map-marker-alt", lib = "font-awesome")),
            menuItem("Dados", tabName = "dados", icon = icon("table", lib = "font-awesome")),
            menuItem("Contato", tabName = "contato", icon = icon("send",lib="glyphicon"))
        )),
    #corpo do dashboard, onde serão "plotados" os gráficos
    dashboardBody(
        tabItems(
            #primeira página do dashboard, são definidas as posições de cada elemento, página geral, com informações sobre casos ativos, descartados, curados
            # confirmados e óbitos.
            tabItem(tabName = "dashboard",
                    # as valueBox são para informar dados gerais sobre o coronavirus em Itajaí.
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
                        #gráfico de linha temporal do número de casos ativos por dia ou boletim
                        box(dygraphOutput("plot1", height = 480)),
                        #gráfico de linha com possibilidade de seleção da variável a ser visualizada
                        box(selectInput("lineVars", label = "Selecione", choices = list("Novos Casos Confirmados" = "casos_confirmados_total",
                                                                                        "Novos Casos Curados" = "casos_curados_total", "Novos Casos Descartados" = "casos_descartados_total",
                                                                                        "Óbitos" = "mortes_total"), selected = 1),
                            plotlyOutput("linePlot", height = 400))
                    ),
                    
                    fluidRow(
                        #gráfico com a a diferença de casos por boletim, ou seja, se o número aumenta ou diminui
                        box(plotlyOutput("loliplot", height = 400)),
                        #gráfico com as variáveis casos confirmados, casos curados e óbitos no mesmo plano (péssimo para essa visualização)
                        box(plotlyOutput("multiLine", height = 400))
                    ),
                    
                    fluidRow(
                        #seleção de variávies para os próximos dois gráficos
                        box(width = 4, selectInput("vars", label = "Selecione", choices = list("Mês" = "mes", "Semana" = "semana_ano"), selected = 1)),
                        
                        box(width = 4, selectInput("page1year", label = "Selecione o Ano", choices = list("2020" = "2020", "2021" = "2021"), selected = 1)),
                        
                        box(width = 4, selectInput("numVars", label = "Selecione", choices = list("Casos Ativos" = "casos_ativos", "Novos Casos Confirmados" = "casos_confirmados_novos",
                                                                                   "Novos Casos Curados" = "casos_curados_novos", "Novos Casos Descartados" = "casos_descartados_novos",
                                                                                   "Óbitos" = "mortes_novos"), selected = 1)),
                    ),
                    
                    fluidRow(
                        #gráfico de distribuição (ridge), mostrando a concentração de dados
                        box(plotOutput("plot2", height = 400)),
                        #gráficode distribuição (boxplot), com individuos
                        box(plotlyOutput("plot3", height = 400))
                    ),
                    
                    
            ),
            
            #página sobre as variáveis de ocupação do hospital e dos óbitos
            tabItem(tabName = "dashboard2",
                    fluidRow(
                        #value boxes com informações sobre internação no Hospital Marieta
                        valueBoxOutput("internacoes"),
                        valueBoxOutput("internadosNum")
                    ),
                    fluidRow(
                        #caixas para seleção de variáveis
                        box(width = 6, selectInput("varsObito", label = "Selecione", choices = list("Mês" = "mes", "Semana" = "semana_ano"), selected = 1)),
                        box(width = 6, selectInput("page2year", label = "Selecione o Ano", choices = list("2020" = "2020", "2021" = "2021"), selected = 1))
                    ),
                    fluidRow(
                        #gráfico de barras comparando o número de mortes por gênero / mês ou semana
                        box(plotlyOutput("plot4", height = 400)),
                        #gráfico de árvore separado por ano, informando as mortes por mês, hospital faixa etária e genero
                        box(d3tree2Output("treePlot"), height = 400)
                        
                    ),
                    fluidRow(
                        #gráfico de piramide, informando a quantidade de óbitos por faixa etária
                        box(plotlyOutput("pyramidPlot", height = 400)),
                        #gráfico de donut, informando número de óbitos acumulados por genero
                        box(plotOutput("donutPlot", height = 400))
                    ),
                    
            ),
            
            #mapa de casos por bairro
            tabItem(tabName = "map",
                    
                    fluidRow(
                        #configurando para que o mapa seja responsivo e ocupe toda a tela
                        tags$style(type = "text/css", "#plotMapa {height: calc(100vh - 80px) !important;}"),
                        leafletOutput("plotMapa", height = "100%")
                    )
                    
                    
            ),
            
            #página com a disponibilização dos dados usados para a criação do dashboard
            tabItem(tabName = "dados",
                    fluidRow(h1("Dados usados para o desenvolvimento do dashboard")),
                    fluidRow(tags$br(h3("Tabela geral com os casos ativos, casos curados, casos confirmados, casos descartados e óbitos:"))),
                    fluidRow(
                        dataTableOutput("tabela1"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        
                    ),
                    fluidRow(tags$br(h3("Dados dos óbitos:"))),
                    fluidRow(
                        dataTableOutput("tabela2"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        
                    ),
                    fluidRow(tags$br(h3("Dados sobre a ocupação do Hospital Marieta:"))),
                    fluidRow(
                        dataTableOutput("tabela3"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        
                    ),
                    fluidRow(tags$br(h3("Dados dos casos acumulados por bairro:"))),
                    fluidRow(
                        dataTableOutput("tabela4"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        
                    )
                    ),
            
            #página de contato e outras informações
            tabItem(tabName = "contato",
                    h2(tags$b("Ingo Ramos")),
                    
                    h4("Sou aluno da Universidade Federal de Santa Catarina - UFSC, porém nativo de Itajaí, e desde o começo da pandemia, venho
                       acompanhando a situação do Município.", tags$br("O propósito deste trabalho é apenas facilitar a visualização dos dados que são 
                       disponibilizados pela prefeitura. ", tags$b("Não tire conclusões precipitadas sobre a situação da pandemia, continue praticando
                       o isolamento (se possível), além de seguir os protocolos de segurança.")), tags$br(),
                       "Vou deixar aqui meus contatos caso você queira tirar alguma dúvida ou até mesmo sugerir algo:", tags$br(),
                       tags$br(),
                       "Clique para acessar o ", tags$a(href="https://www.linkedin.com/in/ingo-ramos/", "LinkedIn."), tags$br(),
                       "Ou mandar um e-mail para:",  tags$b("ingoramos12@gmail.com"), "que responderei assim que possível.", tags$br(),
                       tags$br(),
                       "E para você que quer replicar o trabalho, os dados/código usados para o desenvolvimento estão no ", 
                       tags$a(href="https://github.com/ingoramos/dashboardItj", "GitHub"), " e vou atualizando todos os dias, quando novos dados 
                       estiverem disponíveis.", tags$br(),
                       tags$br(),
                       
                       "O serviço que estou usando para disponibilizar os dados é o da Amazon Web Services - AWS, que não é um serviço gratuito, 
                       por conta disso, o dashboard não estará disponível o dia todo para acesso, apenas em alguns momentos do dia, para que eu 
                       consiga manter por mais meses.", tags$br(), 
                       tags$br(),
                       "Os dados são retirados do ", tags$a(href="http://coronavirus.itajai.sc.gov.br/", "boletim do município.") 
                       
                       )
                    
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # importando a base de dados diretamente do github, com os dados já atualizados
    coronaData <- reactive({
        
        #tratamento dos dados referentes a datas
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
    
    #fazendo as tranformações necessárias para a visualização dos values boxes
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
    
    #criação de um objeto usado para o gráfico de linha temporal
    dyReactive <- reactive({
        corona <- coronaData()
        corona$date_time <- ymd_hms(corona$date_time)
        don <- xts(x = corona$casos_ativos, order.by = corona$date_time)
        return(don)
    })
    
    #gráfico de linha temporal
    output$plot1 <- renderDygraph({
        
        p <- dygraph(dyReactive(), main = "Casos Ativos por Boletim Epidemiológico") %>%
            dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 8, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
            dyRoller(rollPeriod = 1)
        
        p
        
    })
    
    #gráfico de linha com a possibilidade de escolha de variáveis
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
            theme_minimal() +
            ggtitle(paste0("Evolução de ", x2))
        
        ggplotly(lp)
        
    })
    
    #plotagem do mapa com os dados de número de casos por bairro (desatualizado) - etapa de leitura dos dados e transformação
    mapaData <- reactive({
        
        mapa <- read.csv("https://raw.githubusercontent.com/ingoramos/dashboardItj/master/covid_bairros.csv", encoding = "UTF-8")
        
        mapa$type <- NA
        
        for (i in 1:nrow(mapa)){
            if(mapa$num_casos[i] <= mean(mapa$num_casos)){mapa$type[i] <- 'abaixo'}
            else {mapa$type[i] <- 'acima'}
        }
        return(mapa)
    })
    
    #renderizando o gráfico
    output$plotMapa <- renderLeaflet({
        
        pal <- colorFactor(c("navy", "red"), domain = c("abaixo", "acima"))
        
        titulo <- tags$h4(
            HTML("Casos Acumulados por Bairro")
        )
        
        leaflet(mapaData()) %>% 
            setView(lng = -48.751037, lat = -26.975853, zoom = 12) %>%
            addTiles() %>%
            addCircleMarkers(~longitude, ~latitude, label=~as.character(paste0(bairro, " - ", num_casos, " casos totais")),
                             labelOptions = labelOptions(textsize = "15px"),
                             radius = ~ifelse(type == "acima", 14, 10),
                             color = ~pal(type),
                             stroke = FALSE, fillOpacity = 0.5
            ) %>%
            addControl(titulo, position = "bottomright")
    })
    
    #gráfico Ridge de distribuição
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
        
        p <- coronaData() %>%
            filter(ano == input$page1year) %>%
            ggplot(aes_string(x = input$numVars, y = input$vars, fill = "..x..")) +
            geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
            scale_fill_viridis(name = "Casos Ativos", option = "C") +
            labs(title = paste0(x1, " por ", y1)) +
            theme_minimal() +
            theme(
                legend.position="none",
                panel.spacing = unit(0.1, "lines"),
                strip.text.x = element_text(size = 8)
            )
        p
    })
    
    #gráfico de distribuição BoxPlot com individuos
    output$plot3 <- renderPlotly({
        bp <- coronaData() %>%
            filter(ano == input$page1year) %>%
            ggplot( aes_string(x=input$vars, y=input$numVars, fill=input$vars)) +
            geom_boxplot() +
            scale_color_viridis(discrete = TRUE, alpha=0.6) +
            geom_jitter(color="black", size=0.4, alpha=0.9) +
            theme_minimal() +
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
    
    #gráfico loliplot, com a diferença de casos
    output$loliplot <- renderPlotly({
        
        corona <- coronaData() %>% 
            mutate(mycolor = ifelse(dif_casos_ativos>0, "type1", "type2"))
        
        
        lp <- ggplot(corona, aes(x=data, y=dif_casos_ativos)) +
            geom_segment( aes(x=data, xend=data, y=0, yend=dif_casos_ativos, color=mycolor), size=1.3, alpha=0.9) +
            theme_light() +
            theme(
                legend.position = "none",
                panel.border = element_blank(),
            ) +
            ggtitle("Diferença de Casos Ativos por Boletim") +
            xlab("Mês") +
            ylab("Diferença")
        
        
        ggplotly(lp)
        
    })
    
    #gráfico de linha simples
    output$multiLine <- renderPlotly({
        
        #xyplot(casos_confirmados_total + casos_curados_total + mortes_total ~ data, corona, type = "l", lwd=2)
        corona <- coronaData()
        
        mlp <- ggplot(corona, aes(x=data)) +
            geom_line(aes(y=casos_confirmados_total, color = "Confirmados")) +
            geom_line(aes(y=casos_curados_total, color = "Curados")) +
            geom_line(aes(y=mortes_total, color = "Óbitos")) +
            ggtitle("Casos Confirmados, Casos Curados, Óbitos (Acumulados)") +
            theme(axis.title.y=element_blank(),
                  legend.position = "bottom")
        
        mlp <- ggplotly(mlp)
        mlp
    })
    
    #dados da ocupação de leitos de UTI - leitura dos dados
    marietaData <- reactive({
        
        marieta <- read_csv2("https://raw.githubusercontent.com/ingoramos/dashboardItj/master/marieta.csv",  locale = locale(encoding = 'LATIN1'))
        return(marieta)
    })
    
    #value boxes sobre as internações
    output$internacoes <- renderValueBox({
        
        marieta <- marietaData()
        
        internados <- tail(marieta$internacoes_uti, 1) / tail(marieta$total_vagas_uti, 1) * 100
        internados <- round(internados, digits = 2)
        
        valueBox(
            paste0(internados, "%"), "(Ocupação dos Leitos no Hospital Marieta)", icon = icon("hospital-user", lib = "font-awesome"),
            color = 'maroon'
        )    
    })
    
    
    output$internadosNum <- renderValueBox({
        
        marietaNum <- marietaData()
        
        total_internados <- tail(marietaNum$internacoes_uti, 1)
        total_vagas_uti <- tail(marietaNum$total_vagas_uti, 1)
        
        text <- paste0("do total de ", total_vagas_uti, " vagas de UTI disponíveis")
        
        valueBox(
            paste0(total_internados, " internações"), text, icon = icon("procedures", lib = "font-awesome")
        )
    })
    
#    output$vacinados <- renderValueBox({
#        
#    })
    
    #leitura e transformação dos dados dos óbitos
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
            if(obitos$idade[i] >= 0 & obitos$idade[i] <= 2){obitos$faixa_etaria[i] <- "Até 2 anos"}
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
    
    #gráfico de barra comparando o número de óbitos
    output$plot4 <- renderPlotly({
        
        if(dataObito() == "mes"){
            
            bpData <- obitosData() %>%
                filter(ano == input$page2year) %>%
                group_by(mes, genero) %>%
                summarise(num = sum(num))
            
            bpData
            
            bpo <- ggplot(data = bpData, aes_string(x=input$varsObito, y="num", fill="genero"))+
                geom_bar(stat = "identity", position = position_dodge())+
                geom_text(aes(label=num), vjust=1.6, color="black",
                          position = position_dodge(0.9), size=4.5)+
                scale_fill_brewer(palette="Set2")+
                theme_minimal() +
                ggtitle("Óbitos Mês por Gênero") +
                theme(
                    plot.title = element_text(size=11,),
                    axis.text.x = element_text(angle = 45, vjust = 1,
                                               size = 12, hjust = 1, family = "serif"))
            
            ggplotly(bpo)
        }
        
        else{
            bpData <- obitosData() %>%
                filter(ano == input$page2year) %>%
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
    
    #gráfico de piramide com o número de óbitos por faixa etária
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
    
    
    dataTreeMap <- reactive({
        
        treeData <- obitosData() %>%
            filter(ano == input$page2year) %>%
            group_by(mes, hospital, faixa_etaria, genero) %>%
            summarise(num = sum(num))
        
        return(treeData)
        
    })
    
    
    #gráfico de árvore interativo 
    output$treePlot <- renderD3tree2({
        
        #print(dataTreeMap())
        
        tmp <- treemap(
                    dtf = dataTreeMap(),
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
                )
                
        tmpf <- d3tree2(tmp, rootname = "TreeMap")
        
        tmpf
        
    })
    
    #gráfico de donut com o número de óbitos acumulados por genero
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
    
    #output das tabelas de dados
    output$tabela1 <- renderDataTable({
        data.table::data.table(coronaData())
    })
    
    output$tabela2 <- renderDataTable({
        data.table::data.table(obitosData())
    })
    
    output$tabela3 <- renderDataTable({
        data.table::data.table(marietaData())
    })
    
    output$tabela4 <- renderDataTable({
        data.table::data.table(mapaData())
    })
    
}

# Roda a aplicação
shinyApp(ui = ui, server = server)
