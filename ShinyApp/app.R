library(shiny)
library(readxl)
library(ggrepel)
library(ggridges)
library(tidyverse)
library(stringi)
library(scales)
library(leaflet)
library(shinydashboard)
library(reticulate)
library(patchwork)

dfcasen2 <- read_csv('dfcasen.csv')
demo_comuna <- read.csv('demo_comunas.csv', sep = ';')
var_names_grps <- read_csv('names_variables.csv')
pct_carencias <- read_csv('porcentaje_carencias.csv')
desc_var <- read_excel('descripcion_variables.xlsx')

convert_coord <- function(x){
    x2 <- str_replace(x, ',', '.')
    d <- as.numeric(strsplit(x2, '°')[[1]][1])
    min <- as.numeric(strsplit(strsplit(x2, "'")[[1]][1], '°')[[1]][2])
    sec <- as.numeric(strsplit(strsplit(x2, "'")[[1]][2], "\"")[[1]][1])
    coord1 <- abs(d) + min/60 + sec/3600
    
    if (d < 0){
        coord <- -coord1
    }
    else{
        coord <- coord1
    }
    return(coord)
}


ui <- dashboardPage(
    
    dashboardHeader(),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Vision global", tabName = "vision_global"),
            menuItem("Comparación comunas", tabName = "comp_comunas", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        
        tabItems(
            
            tabItem(
                
                tabName = "vision_global",
                
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = 12,
                            ###### Seleccion dimension #####
                            column(
                                width = 5,
                                selectInput(
                                    inputId = "carencia",
                                    label = "Seleccione una dimensión",
                                    choices = levels(as.factor(na.omit(var_names_grps)$real_name))
                                )
                            ),
                            #### Descripcion dimension ####
                            column(
                                width = 12-5,
                                textOutput(outputId = 'descripcion_variable')
                            )
                        )
                    )
                ),
                
                fluidRow(
                    
                    ##### Carencia comunas ####
                    column(
                        width = 12,
                        box(
                            width = 12,
                            height = '820px',
                            column(
                                width = 9,
                                # Rango de población
                                fluidRow(
                                    sliderInput(
                                        inputId = 'choice_pop_plot_carencia',
                                        label = 'Elija la población de las comunas que mostrar',
                                        min = 0,
                                        max = max(demo_comuna$poblacion),
                                        value = c(0,max(demo_comuna$poblacion)))
                                ),
                                # Plot
                                fluidRow(
                                    plotOutput(
                                        outputId = 'plot_carencia_comunas',
                                        width = "800px",
                                        height = '700px'
                                    )
                                )
                            ),
                            # Map
                            column(
                                width = 3,
                                leafletOutput(
                                    outputId = 'map_carencia',
                                    height = '800px')
                            )
                        )
                    )
                )
            ),
            
            tabItem(
                
                tabName = "comp_comunas",
                
                fluidRow(
                    
                    # Elecciones #####
                    column(
                        width = 12,
                        box(
                            width = 12,
                            # Eleccion 1 ####
                            fluidRow(
                                column(
                                    width = 6,
                                    radioButtons(
                                        inputId = 'choice_compare1',
                                        label = 'Quiero comparar',
                                        choices = c('Una region', 'Una comuna'),
                                        inline = T,
                                        selected = 'Una region'
                                    )
                                ),
                                # Eleccion 2 ####
                                column(
                                    width = 6,
                                    radioButtons(
                                        inputId = 'choice_compare2',
                                        label = 'con',
                                        choices = c('El pais', 'Una region', 'Una comuna'),
                                        inline = T,
                                        selected = 'Una comuna'
                                    )
                                )
                            ),
                            
                            # Choices comunes / region / pais #####
                            fluidRow(
                                column(
                                    width = 6,
                                    uiOutput(
                                        outputId = 'ui_selection_comp1'
                                    )
                                ),
                                column(
                                    width = 6,
                                    uiOutput(
                                        outputId = 'ui_selection_comp2'
                                    )
                                )
                            ),
                            
                            # Submit Button #####
                            fluidRow(
                                column(
                                    width = 12,
                                    actionButton(
                                        inputId = 'button',
                                        label = 'Aplicar cambios'
                                    )
                                )
                            )
                        )
                    )
                ),
                
                # Info general ####
                fluidRow(
                    column(
                        width = 12,
                        box(
                            title = 'Datos de población',
                            width = 12,
                            column(
                                width = 12,
                                tableOutput(
                                    outputId = 'info_general'
                                )
                            )
                        )
                    )
                ),
                
                br(),
                
                #### 3 plots patchwork comunas #####
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = 12,
                            height = 820,
                            plotOutput(
                                outputId = 'plot_comunas',
                                height = 800
                            )
                        )
                    )
                )
            )
        )
    )
)


server <- function(input, output){
    
    ##### Descripcion variable #####
    output$descripcion_variable <- renderText(
        {
            filter(desc_var, var == input$carencia)$descripcion
        }
    )
    
    ##### Map carencia #####
    
    output$map_carencia <- renderLeaflet(
        {
            
            demo_comuna$lat <- unlist(lapply(demo_comuna$latitud, convert_coord))
            demo_comuna$lng <- unlist(lapply(demo_comuna$longitud, convert_coord))
            
            dfmap <- pct_carencias %>%
                pivot_longer(3:ncol(.), names_to = 'var', values_to = 'pct_carencia') %>%
                left_join(var_names_grps, by ='var') %>%
                filter(real_name == input$carencia) %>%
                left_join(demo_comuna, by = 'comuna') %>%
                filter(between(poblacion, input$choice_pop_plot_carencia[1], input$choice_pop_plot_carencia[2])) %>%
                mutate('label' = ifelse(
                    !is.na(pct_carencia),
                    paste(comuna, 
                          "<br>", 
                          round(pct_carencia*100, 2), 
                          '%', 
                          ' de hogares con carencia',
                          sep = ""),
                    paste(comuna,
                          "<br>",
                          'Ningún dato')
                ))
            
            colors <- colorNumeric(palette = 'RdYlGn', -dfmap$pct_carencia)  
            
            dfmap %>%
                leaflet() %>%
                addTiles() %>%
                addProviderTiles("Esri.WorldGrayCanvas") %>%
                setView(lat = -33.4372, lng = -70.6506, zoom = 5) %>%
                addCircleMarkers(
                    lng = ~lng,
                    lat = ~lat,
                    popup = ~label,
                    radius = 3,
                    #radius = ~pct_carencia*100,
                    color = ~colors(-pct_carencia)
                )
        }
    )
    
    ##### Carencia comunas #####
    
    output$plot_carencia_comunas <- renderPlot(
        {
            dfplot <- pct_carencias %>%
                pivot_longer(3:ncol(.), names_to = 'var', values_to = 'pct_carencia') %>%
                left_join(select(var_names_grps, c(var, real_name)), by = 'var') %>%
                left_join(select(demo_comuna, c('comuna', 'poblacion')), by = 'comuna') %>%
                filter(real_name == input$carencia & 
                           between(poblacion, input$choice_pop_plot_carencia[1], input$choice_pop_plot_carencia[2])) %>%
                arrange(desc(pct_carencia)) %>%
                head(50) %>%
                #arrange(comuna) %>%
                mutate('id' = row_number(),
                       'angle' = 90 - 360*id/nrow(.),
                       'angle_correct' = ifelse(angle < -90, angle+180, angle))
            
            lines <- c(round(max(dfplot$pct_carencia)*100, -1)/100,
                       round(max(dfplot$pct_carencia)*100, -1)/100/1.5,
                       round(max(dfplot$pct_carencia)*100, -1)/100/2,
                       round(max(dfplot$pct_carencia)*100, -1)/100/2.5)
            
            ggplot() +
                # Segments
                geom_segment(
                    data = dfplot,
                    aes(x = reorder(comuna, -pct_carencia),
                        xend = comuna,
                        y = 0,
                        yend = pct_carencia),
                    alpha = .7,
                    color = 'gray75'
                ) +
                # Dashed segments
                geom_segment(
                    data = dfplot,
                    aes(x = reorder(comuna, -pct_carencia),
                        xend = comuna,
                        y = pct_carencia,
                        yend = max(pct_carencia)+0.1),
                    alpha = .7,
                    color = 'gray75',
                    linetype = 'dotted'
                ) +
                # Percentage lines
                geom_line(
                    data = NULL,
                    aes(x = seq(0,nrow(dfplot)+1),
                        y = lines[1]),
                    color = "gray90",
                    alpha = .7
                ) +
                geom_line(
                    data = NULL,
                    aes(x = seq(0,nrow(dfplot)+1),
                        y = lines[2]),
                    color = "gray90",
                    alpha = .7
                ) +
                geom_line(
                    data = NULL,
                    aes(x = seq(0,nrow(dfplot)+1),
                        y = lines[3]),
                    color = "gray90",
                    alpha = .7
                ) +
                geom_line(
                    data = NULL,
                    aes(x = seq(0,nrow(dfplot)+1),
                        y = lines[4]),
                    color = "gray90",
                    alpha = .7
                ) +
                # Comune names
                geom_text(
                    data = dfplot,
                    aes(reorder(comuna, -pct_carencia), 
                        max(pct_carencia)+0.1, 
                        label = comuna,
                        angle = angle_correct),
                    size = 5
                ) +
                # Points at the end
                geom_point(
                    data = dfplot,
                    aes(reorder(comuna, -pct_carencia), 
                        pct_carencia), 
                    colour = '#330000', 
                    size = 2, 
                    alpha = .3) +
                # Text percentages
                geom_text(
                    data = NULL,
                    aes(x = 0,
                        y = lines[1]),
                    label = paste(round(lines[1]*100,0),'%'),
                    color = "gray75",
                    size = 5
                ) +
                geom_text(
                    data = NULL,
                    aes(x = 0,
                        y = lines[2]),
                    label = paste(round(lines[2]*100,0),'%'),
                    color = "gray75",
                    size = 5
                ) +
                geom_text(
                    data = NULL,
                    aes(x = 0,
                        y = lines[3]),
                    label = paste(round(lines[3]*100,0),'%'),
                    color = "gray75",
                    size = 5
                ) +
                geom_text(
                    data = NULL,
                    aes(x = 0,
                        y = lines[4]),
                    label = paste(round(lines[4]*100,0),'%'),
                    color = "gray75",
                    size = 5
                ) +
                coord_polar(clip = 'on', start = 0) +
                theme_void() +
                theme(
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.title = element_blank()
                )
        }
    )
    
    ##### UI seleccion 1 #####
    
    output$ui_selection_comp1 <- renderUI(
        {
            if (input$choice_compare1 == 'Una region'){
                lab <- NULL
                choice <- levels(as.factor(dfcasen2$region))
                sel <- NULL
            }
            
            if (input$choice_compare1 == 'Una comuna'){
                lab <- NULL
                choice <- levels(as.factor(dfcasen2$comuna))
                sel <- NULL
            }
            
            selectInput(
                inputId = 'eleccion1',
                choices = choice,
                label = lab,
                selected = sel
            )
        }
    )
    
    ##### UI seleccion 2 #####
    
    output$ui_selection_comp2 <- renderUI(
        {
            if (input$choice_compare2 == 'Una region'){
                lab <- NULL
                choice <- levels(as.factor(dfcasen2$region))
                sel <- filter(dfcasen2, comuna == input$eleccion1)$region[1]
            }
            
            if (input$choice_compare2 == 'Una comuna'){
                lab <- NULL
                choice <- levels(as.factor(dfcasen2$comuna))
                sel <- NULL
            }
            
            if (input$choice_compare2 == 'El pais'){
                lab <- NULL
                choice <- NULL
                sel <- NULL
            }
            
            selectInput(
                inputId = 'eleccion2',
                choices = choice,
                label = lab,
                selected = sel
            )
        }
    )
    
    # Button #####
    
    v <- reactiveValues(doPlot = F)
    
    observeEvent(
        input$button,
        {v$doPlot <- input$button}
    )
    
    observeEvent(
        input$comp_comunas,
        {v$doPlot <- F}
    )
    
    ##### Info general ####
    output$info_general <- renderTable(
        {
            if (v$doPlot == FALSE) return()
            
            isolate({
                if (input$choice_compare1 == 'Una region'){
                    dftab <- filter(demo_comuna, region == input$eleccion1)
                    dftab1 <- data.frame('region' = input$eleccion1, 'superficie' = sum(dftab$superficie), 'poblacion' = sum(dftab$poblacion), 'densidad' = round(sum(dftab$poblacion)/sum(dftab$superficie),2))
                    dftab1$superficie <- paste(dftab1$superficie,'km2')
                    dftab1$densidad <- paste(dftab1$densidad,'hab/km2')
                    colnames(dftab1) <- c('REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION')
                }
                
                if (input$choice_compare1 == 'Una comuna'){
                    dftab1 <- filter(demo_comuna, comuna == input$eleccion1)[,c('comuna','provincia','region','superficie','poblacion','densidad')]
                    dftab1$superficie <- paste(dftab1$superficie,'km2')
                    dftab1$densidad <- paste(round(dftab1$densidad,2),'hab/km2')
                    colnames(dftab1) <- c('COMUNA','PROVINCIA','REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION')
                }
                
                if (input$choice_compare2 == 'Una region'){
                    dftab <- filter(demo_comuna, region == input$eleccion2)
                    dftab2 <- data.frame('region' = input$eleccion2, 'superficie' = sum(dftab$superficie), 'poblacion' = sum(dftab$poblacion), 'densidad' = round(sum(dftab$poblacion)/sum(dftab$superficie),2))
                    dftab2$superficie <- paste(dftab2$superficie,'km2')
                    dftab2$densidad <- paste(dftab2$densidad,'hab/km2')
                    colnames(dftab2) <- c('REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION')
                    
                    dftab3 <- as.data.frame(t(bind_rows(dftab1, dftab2)))
                    colnames(dftab3) <- c(input$eleccion1, input$eleccion2)
                    
                    if (input$choice_compare1 == 'Una region'){
                        dftab3 <- dftab3[c('REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION'),]
                    }
                    else{
                        dftab3 <- dftab3[c('COMUNA','PROVINCIA','REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION'),]
                    }
                }
                
                if (input$choice_compare2 == 'Una comuna'){
                    dftab2 <- filter(demo_comuna, comuna == input$eleccion2)[,c('comuna','provincia','region','superficie','poblacion','densidad')]
                    dftab2$superficie <- paste(dftab2$superficie,'km2')
                    dftab2$densidad <- paste(round(dftab2$densidad,2),'hab/km2')
                    colnames(dftab2) <- c('COMUNA','PROVINCIA','REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION')
                    
                    dftab3 <- as.data.frame(t(bind_rows(dftab1, dftab2)))
                    colnames(dftab3) <- c(input$eleccion1, input$eleccion2)
                    dftab3 <- dftab3[c('COMUNA','PROVINCIA','REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION'),]
                }
                
                if (input$choice_compare2 == 'El pais'){
                    dftab <- demo_comuna[,c('superficie','poblacion')]
                    dftab2 <- data.frame('superficie' = sum(dftab$superficie), 'poblacion' = sum(dftab$poblacion), 'densidad' = round(sum(dftab$poblacion)/sum(dftab$superficie),2))
                    dftab2$superficie <- paste(dftab2$superficie,'km2')
                    dftab2$densidad <- paste(dftab2$densidad,'hab/km2')
                    colnames(dftab2) <- c('SUPERFICIE','POBLACION','DENSIDAD DE POBLACION')
                    
                    dftab3 <- as.data.frame(t(bind_rows(dftab1, dftab2)))
                    colnames(dftab3) <- c(input$eleccion1, 'Pais')
                    
                    if (input$choice_compare1 == 'Una region'){
                        dftab3 <- dftab3[c('REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION'),]
                    }
                    else{
                        dftab3 <- dftab3[c('COMUNA','PROVINCIA','REGION','SUPERFICIE','POBLACION','DENSIDAD DE POBLACION'),]
                    }
                }
                
                return(t(dftab3))
            })
        },
        
        rownames = T,
        colnames = T,
        na = ""
    )
    
    ##### Patchwork plots comunas ####
    output$plot_comunas <- renderPlot(
        {
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Eleccion 1
                if (input$choice_compare1 == 'Una region'){
                    df1 <- dfcasen2 %>% filter(region == input$eleccion1)
                    df1$dimension <- input$eleccion1
                    
                    pob1 <- as.data.frame(prop.table(table(filter(dfcasen2, region == input$eleccion1)$pobreza_multi_5d)))
                    colnames(pob1) <- c('Pobreza','Proporcion')
                    pob1$dimension <- input$eleccion1
                    pob1$poblacion <- sum(filter(demo_comuna, region == input$eleccion1)$poblacion)
                    pob1$n_pobres <- pob1$poblacion*pob1$Proporcion
                }
                
                if (input$choice_compare1 == 'Una comuna'){
                    df1 <- dfcasen2 %>% filter(comuna == input$eleccion1)
                    df1$dimension <- input$eleccion1
                    
                    pob1 <- as.data.frame(prop.table(table(filter(dfcasen2, comuna == input$eleccion1)$pobreza_multi_5d)))
                    colnames(pob1) <- c('Pobreza','Proporcion')
                    pob1$dimension <- input$eleccion1
                    pob1$poblacion <- filter(demo_comuna, comuna == input$eleccion1)$poblacion
                    pob1$n_pobres <- pob1$poblacion*pob1$Proporcion
                }
                
                # Eleccion 2
                if (input$choice_compare2 == 'El pais'){
                    df2 <- dfcasen2
                    df2$dimension <- 'Pais'
                    
                    pob2 <- as.data.frame(prop.table(table(dfcasen2$pobreza_multi_5d)))
                    colnames(pob2) <- c('Pobreza','Proporcion')
                    pob2$dimension <- 'Chile'
                    pob2$poblacion <- sum(demo_comuna$poblacion)
                    pob2$n_pobres <- pob2$poblacion*pob1$Proporcion
                    
                    el2 <- 'Pais'
                }
                
                if (input$choice_compare2 == 'Una region'){
                    df2 <- dfcasen2 %>% filter(region == input$eleccion2)
                    df2$dimension <- input$eleccion2
                    
                    pob2 <- as.data.frame(prop.table(table(filter(dfcasen2, region == input$eleccion2)$pobreza_multi_5d)))
                    colnames(pob2) <- c('Pobreza','Proporcion')
                    pob2$dimension <- input$eleccion2
                    pob2$poblacion <- sum(filter(demo_comuna, region == input$eleccion2)$poblacion)
                    pob2$n_pobres <- pob2$poblacion*pob2$Proporcion
                    
                    el2 <- input$eleccion2
                }
                
                if (input$choice_compare2 == 'Una comuna'){
                    df2 <- dfcasen2 %>% filter(comuna == input$eleccion2)
                    df2$dimension <- input$eleccion2
                    
                    pob2 <- as.data.frame(prop.table(table(filter(dfcasen2, comuna == input$eleccion2)$pobreza_multi_5d)))
                    colnames(pob2) <- c('Pobreza','Proporcion')
                    pob2$dimension <- input$eleccion2
                    pob2$poblacion <- filter(demo_comuna, comuna == input$eleccion2)$poblacion
                    pob2$n_pobres <- pob2$poblacion*pob2$Proporcion
                    
                    el2 <- input$eleccion2
                }
                
                # Distrib ingresos
                df <- rbind(df1,df2)
                
                ingresos <- df %>%
                    ggplot()+
                    geom_density_ridges(
                        aes(x = ytotcorh/1e6,
                            y = dimension,
                            fill = dimension),
                        scale = 1.5,
                        alpha = .8
                    ) +
                    scale_fill_manual(
                        values = c('#CC6600', '#FF9933'), 
                        name = '', 
                        breaks = c(df1$dimension[1], 
                                   df2$dimension[2]), 
                        labels = c(df1$dimension[1], 
                                   df2$dimension[2])
                    ) +
                    guides(fill = F) +
                    xlab('Millones CLP') +
                    ggtitle('Ingresos totales del hogar') +
                    theme_minimal() +
                    theme(axis.title.y = element_blank(),
                          axis.text.y = element_text(size = 14),
                          plot.title = element_text(hjust = 0.5))
                
                # Proporcion pobreza
                pobreza1 <- ggplot(filter(pob1, Pobreza == 'Pobre')) +
                    geom_text(
                        aes(x = 1,
                            y = 1,
                            label = paste(round(n_pobres,0), ' pobres', '\n(', round(Proporcion*100,1),'%)',sep = '')),
                        size = 10) +
                    ggtitle(input$eleccion1) +
                    theme_void() +
                    theme(plot.title = element_text(hjust = 0.5))
                
                pobreza2 <- ggplot(filter(pob2, Pobreza == 'Pobre')) +
                    geom_text(
                        aes(x = 1,
                            y = 1, 
                            label = paste(round(n_pobres,0), ' pobres', '\n(', round(Proporcion*100,1),'%)',sep = '')),
                        size = 10) +
                    ggtitle(el2) +
                    theme_void() +
                    theme(plot.title = element_text(hjust = 0.5))
                
                # Bar carencias
                prop_carencia_com <- as.data.frame(matrix(ncol = 3, nrow = 0))
                colnames(prop_carencia_com) <- c('prop_carencia', 'var','dimension')
                table_levels_factor <- function(x) table(factor(x, levels = 0:1))
                
                # Proporciones eleccion 1
                df1 <- as.data.frame(prop.table(apply(df1[,-c(1,2,3,4,5,25,26,27)], 
                                                      FUN = table_levels_factor, 
                                                      MARGIN = 2),
                                                margin = 2)[2,])
                colnames(df1) <- c('prop_carencia')
                df1$var <- rownames(df1)
                df1$dimension <- 'eleccion1'
                
                # Proporciones eleccion 2
                df2 <- as.data.frame(prop.table(apply(df2[,-c(1,2,3,4,5,25,26,27)], 
                                                      FUN = table_levels_factor, 
                                                      MARGIN = 2),
                                                margin = 2)[2,])
                colnames(df2) <- c('prop_carencia')
                df2$var <- rownames(df2)
                df2$dimension <- 'eleccion2'
                
                # Unimos dfs
                df <- rbind(df1, df2)
                prop_carencia <- left_join(df, var_names_grps, by = 'var')
                
                # Plot
                prop_carencia <- prop_carencia %>%
                    spread(key = dimension, value = prop_carencia) %>%
                    mutate('difference' = eleccion1 - eleccion2) %>%
                    gather(key = 'dimension', value = 'prop_carencia', c('eleccion1', 'eleccion2'))
                
                n <- nrow(filter(prop_carencia, dimension == 'eleccion1' & difference > 0)) + 0.5
                n2 <- nrow(prop_carencia)/2+0.5
                
                bar_carencias <- ggplot(prop_carencia) +
                    geom_bar(aes(reorder(real_name, n), prop_carencia, fill = dimension),
                             stat = 'identity', position = 'dodge', width = 0.5) +
                    geom_vline(xintercept = filter(var_names_grps, change_cat == 1)$n[1]-0.5, linetype = 'dashed') +
                    geom_vline(xintercept = filter(var_names_grps, change_cat == 1)$n[2]-0.5, linetype = 'dashed') +
                    geom_vline(xintercept = filter(var_names_grps, change_cat == 1)$n[3]-0.5, linetype = 'dashed') +
                    geom_vline(xintercept = filter(var_names_grps, change_cat == 1)$n[4]-0.5, linetype = 'dashed') +
                    geom_vline(xintercept = filter(var_names_grps, change_cat == 1)$n[5]-0.5, linetype = 'dashed') +
                    geom_text(aes(2, max(prop_carencia)+0.02, label = 'Educación'), size = 7) +
                    geom_text(aes(5, max(prop_carencia)+0.01, label = 'Redes y \n cohesión social'), size = 7) +
                    geom_text(aes(8.5, max(prop_carencia)+0.02, label = 'Salud'), size = 7) +
                    geom_text(aes(12, max(prop_carencia)+0.02, label = 'Trabajo'), size = 7) +
                    geom_text(aes(16.5, max(prop_carencia)+0.02, label = 'Vivienda'), size = 7) +
                    scale_fill_manual(values = c('#CC6600', '#FF9933'),
                                      labels = c(input$eleccion1, el2), name = '') +
                    scale_y_continuous(labels = scales::percent_format()) +
                    ggtitle('Porcentaje de hogares carentes por dimensión') +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 45, size = 15, hjust = 1),
                          axis.text.y = element_text(size = 15),
                          legend.text = element_text(size = 15),
                          axis.title.x = element_blank(),
                          legend.position = 'top',
                          plot.margin = unit(c(0,0,0,0), 'cm'),
                          plot.title = element_text(hjust = 0.5, size = 18),
                          axis.title.y = element_blank())
                
                # Patchwork
                design <- "
        123
        ###
        444"
                
                heights <- c(1,0.5,3)
                widths <- c(1,0.5,0.5,2)
                
                ingresos + pobreza1 + pobreza2 + bar_carencias + plot_layout(design = design, heights = heights, widths = widths)
            })
        }
    )
}

shinyApp(ui = ui, server = server)
