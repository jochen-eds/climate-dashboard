# Libraries
library = "shiny"
library = "shinydashboard"
library = "shinyjs"
library = "tidyverse"
library = "scales"
library = "tidyquant"
library = "forecast"
library = "plotly" 

# Load data ----
dat <- read_csv(file = "KA_Climate_year_clean.csv") #%>%

#Helper functions ----
metric_choices <- colnames(dat)[2:ncol(dat)]
metric_names <- gsub("_", " ", metric_choices)
metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names,2,nchar(metric_names)))
metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names

name_fix <- function(x){
  s1 <- gsub("_", " ", x)
  s2 <- paste0(toupper(substr(s1,1,1)), substr(s1,2,nchar(s1)))
  return(s2)
  }

# UI ----
ui <- dashboardPage(
  
  skin = "green",
  
  #### Header ----
  dashboardHeader(
    title = "Kaiserslautern Climate Dashboard",
    titleWidth = 350,
    tags$li(a(href = 'https://zubrod-eds.de', target="_blank",
              img(src = 'Mail_Logo.png',
                  title = "Zubrod EDS", height = "30px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")
  ),
  
  #### Sidebar ----
  dashboardSidebar(
    
    shinyjs::useShinyjs(),
    
    width = 350,
    br(),
    h4("Select your inputs ", style = "padding-left:20px"),
    
    uiOutput("sidebar")
    
  ),
  
  #### Panels ----
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      id = "tab_selected",
      tabPanel(
        title = "Temperature",
        plotlyOutput("temp_plot"),
        uiOutput("temp_forecast_panel")
      ),
      tabPanel(
        # 2 - Regional View ----
        title = "Precipitation",
        plotlyOutput("prec_plot"),
        uiOutput("prec_forecast_panel")
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  
  #### Control forecast ----
  make_forecast_temp <- reactiveValues(value=0)
  make_forecast_prec <- reactiveValues(value=0)
  
  observeEvent(input$tab_selected, {
    make_forecast_temp$value <- 0
    make_forecast_prec$value <- 0
  })
  
  #### Clean data ----
  clean_data <- reactive({
    req(input$year_range)
    dat %>%
    filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
    select(year, input$metric) %>%
    arrange(year)
  })
  
  #### Plot data ----
  plot_data <- function(data){
    L <- colnames(data)[2:ncol(data)]
    x <- list(title = "Year")
    ifelse(input$tab_selected == "Temperature", y <- list( title = "Temperature in °C"),
           y <- list( title = "Precipitation in mm"))
    coleurs <- c("orange", "blue", "green", "red", "purple")
    plt <- plot_ly(data = data)
    for(k in 1:length(L)) {
      dfk <- data.frame(y=data[[L[k]]], year=data$year)
      plt <- add_trace(plt, y=~y, x=~year, data=dfk, 
                       type="scatter", mode="lines+markers", name = name_fix(L[k]),
                       color = coleurs[k], hovertemplate = paste(
                         paste0('<extra>Actuals</extra>',name_fix(L[k]),': %{y}\nYear: %{x}'))
      )
      if( input$moving_average == TRUE & !is.null(input$moving_average_years) ){
        dfk <- data.frame(y=rollmean(data[[L[k]]],ma_years(),mean,align='right',fill=NA), 
                          year = data$year)
        
        plt <- add_trace(plt, y=~y, x=~year, data=dfk, 
                         type="scatter", mode='lines', line=list(dash="dash"), showlegend=F,
                         name = name_fix(L[k]), color = coleurs[k], hovertemplate = paste(
                           paste0('<extra>Moving average</extra>',name_fix(L[k]),': %{y}\nYear: %{x}'))
        )
      }
      if( make_forecast_temp$value == 1 | make_forecast_prec$value == 1 ){
        auto_forecast <-  forecast(auto.arima(data[L[k]]),forecast_years())
        new_years <- max(data$year) + c(1:forecast_years())
        dfk <- data.frame(mean=auto_forecast$mean, lcl=auto_forecast$lower[2], 
                          ucl=auto_forecast$upper[2], year = new_years)
        
        plt <- add_trace(plt, y=~mean, x=~year, data= dfk, 
                         type="scatter", mode='lines', line=list(dash="dot"), showlegend=F,
                         name = name_fix(L[k]), color = coleurs[k], hovertemplate = paste(
                           paste0('<extra>Forecast mean</extra>',name_fix(L[k]),': %{y}\nYear: %{x}'))
        )
        plt <- add_trace(plt, y=~lcl, x=~year, data= dfk, 
                         type="scatter", mode='lines', line=list(dash="dot", width=0.5), showlegend=F,
                         name = name_fix(L[k]), color = coleurs[k], hovertemplate = paste(
                           paste0('<extra>Forecast LCL</extra>',name_fix(L[k]),': %{y}\nYear: %{x}'))
        )
        plt <- add_trace(plt, y=~ucl, x=~year, data= dfk, 
                         type="scatter", mode='lines', line=list(dash="dot", width=0.5), showlegend=F,
                         name = name_fix(L[k]), color = coleurs[k], hovertemplate = paste(
                           paste0('<extra>Forecast UCL</extra>',name_fix(L[k]),': %{y}\nYear: %{x}'))
        )
        
      }
      
    }
    plt <- layout(plt, title = '', yaxis = y, xaxis = x)
    highlight(plt)
  }
  
  ##### Render plots ----
  output$temp_plot <- renderPlotly({
    req( input$metric ) 
    if( input$tab_selected == "Temperature"){
    return(plot_data( clean_data() ) )
    }
  })
  
  output$prec_plot <- renderPlotly({
    req( input$metric ) 
    if( input$tab_selected == "Precipitation"){
      return(plot_data( clean_data() ) )
    }
  })
  
  #### Buttons ----
  ma_years <- eventReactive(input$moving_average_bttn,{
    req(input$moving_average_years)
    input$moving_average_years
  },ignoreNULL = FALSE)
  
  observeEvent(input$temp_forecast_bttn, {
    if(input$tab_selected=="Temperature"){
    make_forecast_temp$value <- 1
    }
  })
  
  observeEvent(input$prec_forecast_bttn, {
    if(input$tab_selected=="Precipitation"){
      make_forecast_prec$value <- 1
    }
  })
  
  observeEvent(input$temp_remove_forecast_bttn, {
    if(input$tab_selected=="Temperature"){
      make_forecast_temp$value <- 0
    }
  })
  
  observeEvent(input$prec_remove_forecast_bttn, {
    if(input$tab_selected=="Precipitation"){
      make_forecast_prec$value <- 0
    }
  })
  
  temp_forecast_years <- eventReactive(input$temp_forecast_bttn,{
    input$temp_forecast
  })
  
  prec_forecast_years <- eventReactive(input$prec_forecast_bttn,{
    input$prec_forecast
  })
  
  forecast_years <- reactive( {
    if (input$tab_selected == "Temperature" & make_forecast_temp$value == 1){
      forecast_years <- temp_forecast_years()
    } else if (input$tab_selected == "Precipitation" & make_forecast_prec$value == 1){
      forecast_years <- prec_forecast_years()
    }
  })
  
  #### Fade-in moving average ----
  observeEvent(input$moving_average, {
    if( input$moving_average == TRUE )
      shinyjs::show(id = "moving_average_years", anim = TRUE, animType = "fade") 
    else {
      shinyjs::hide(id = "moving_average_years", anim = TRUE, animType = "fade")
    }
  })
  
  #### Inputs ----
  
  output$metric_temp <- renderUI({
    selectInput(
      inputId = "metric", 
      label = strong("Select metrics", style = "font-family: 'arial'; font-si28pt"),
      choices =  metric_list[1:5],
      selected = metric_list[1],
      multiple = TRUE
    )
  })
  
  output$metric_prec <- renderUI({
    selectInput(
      inputId = "metric", 
      label = strong("Select metrics", style = "font-family: 'arial'; font-si28pt"),
      choices =  metric_list[6:7],
      selected = metric_list[6],
      multiple = TRUE
    )
  })
  
  output$year_range <- renderUI({
    sliderInput(
      inputId = "year_range",
      label = "Select year range",
      min = 1901,
      max   = 2020,
      value = c(1901, 2020),
      sep = ""
    )
  })
  
  output$moving_average <- renderUI({
    checkboxInput(
      inputId = "moving_average",
      label = div("Include moving average", style = "font-size: 12pt"),
      #style = "font-size: 28pt",
      value = FALSE
    )
  })
  
  output$moving_average_years <- renderUI({
    div(
      numericInput(
        inputId = "moving_average_years",
        label = "Number of years for moving average",
        value = 5,
        min = 0,
        max = 30,
        step = 1
      ),
      actionButton(inputId = "moving_average_bttn",
                   style = "color: white;",
                   label = "Update moving average",
                   class = "btn-success"
      )
    )
  })  
  
  output$temp_forecast <- renderUI({
    numericInput(
      inputId = "temp_forecast",
      label = "Number of years to forecast",
      value = 20, min = 0, max = 100, step = 1
    )
  })
  
  output$prec_forecast <- renderUI({
    numericInput(
      inputId = "prec_forecast",
      label = "Number of years to forecast",
      value = 20, min = 0, max = 100, step = 1
    )
  })
  
  output$temp_forecast_bttn <- renderUI({
    actionButton(inputId = "temp_forecast_bttn",
                 icon = icon("sun", lib = "font-awesome"),
                 style = "color: white;", 
                 label = " Make forecast",
                 class = "btn btn-lg btn-success"
    )
  })
  
  output$prec_forecast_bttn <- renderUI({
    actionButton(inputId = "prec_forecast_bttn",
                 icon = icon("cloud-rain", lib = "font-awesome"),
                 style = "color: white;", 
                 label = " Make forecast",
                 class = "btn btn-lg btn-success"
    )
  })
  
  output$temp_remove_forecast_bttn <- renderUI({
    actionButton(inputId = "temp_remove_forecast_bttn",
                 icon = icon("ban", lib = "font-awesome"),
                 style = "color: white;", 
                 label = "Stop forecast",
                 class = "btn btn-lg btn-danger"
    )
  })
  
  output$prec_remove_forecast_bttn <- renderUI({
    actionButton(inputId = "prec_remove_forecast_bttn",
                 icon = icon("ban", lib = "font-awesome"),
                 style = "color: white;", 
                 label = "Stop forecast",
                 class = "btn btn-lg btn-danger"
    )
  })

  text <- div( 
        br(),
        br(),
        strong("This dashboard is part of the ", 
                a("Environmental Data Science Playground", 
                href="https://zubrod-eds.de/en/playground/",
                target="_blank")),
        br(),
        br(),
        strong("Data was retrieved  from ", a("NOAA", 
                                              href="https://www.noaa.gov",
                                              target="_blank")),
        br(),
        br(),
        br()
      )
  
  output$temp_forecast_panel <- renderUI({
    div(
      class = "jumbotron",
      div(
        class = "container bg-success",
        br(),
        p(strong("Make a forecast for temperature")),
        uiOutput("temp_forecast"),
        uiOutput("temp_forecast_bttn"),
        br(),
        uiOutput("temp_remove_forecast_bttn"),
        br(),
        text
      )
    )
  })

  output$prec_forecast_panel <- renderUI({
    div(
      class = "jumbotron",
      div(
        class = "container bg-success",
        br(),
        p(strong("Make a forecast for precipitation")),
        uiOutput("prec_forecast"),
        uiOutput("prec_forecast_bttn"),
        br(),
        uiOutput("prec_remove_forecast_bttn"),
        br(),
        text
        )
    )
  })
  
  output$sidebar <- renderUI({
    if( input$tab_selected == "Temperature"){
      div(
        uiOutput("metric_temp"),
        uiOutput("year_range"),
        uiOutput("moving_average"),
        uiOutput("moving_average_years") %>% hidden()
      )
    } else if ( input$tab_selected == "Precipitation" ) {
      div(
        uiOutput("metric_prec"),
        uiOutput("year_range"),
        uiOutput("moving_average"),
        uiOutput("moving_average_years") %>% hidden()
      )
    }
  })
  
}

shinyApp(ui, server)