library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(forecast)
library(plotly)
library(quantmod)
library(shinydashboard)
library(shinydashboardPlus)
# Prepare the dataset we are going to use
automakers_data = read.csv('./Final project data/Largest automakers by market capitalization.csv')
stock_price = read.csv('./Final project data/automakers stocks 2010-2022.csv')
svg_vector <- c('0175.HK', '000270.KS', '000625.SZ', '003620.KS', '7201.T', '7202.T', '7261.T', 
                '7269.T', '7270.T', 'ARVL', 'BMW.DE', 'DAI.DE', 'F', 'GM', 'GOEV', 'Honda_logo', 
                'HYMTF', 'LCID', 'M&M.NS', 'MARUTI.NS', 'NIO', 'NKLA', 'PII', 'RACE',
                'RIVN', 'RNSDF', 'SEV', 'STLA', 'TM', 'TSLA', 'TTM', 'VOLCAR-B.ST', 'VOW3.DE', 
                'REE', 'CENN')

png_vector <- c('0489.HK', '000800.SZ', '002594.SZ', '600104.SS', '601633.SS', 
                'FSR', 'FUV', 'LI', 'RIDE', 'SOLO', 'XPEV', 'A5SA.F', '2207.TW')

rect_vector <- c('0175.HK', '000270.KS', '000625.SZ', '003620.KS', '7201.T', '7202.T',
                 '0489.HK', '000800.SZ', '7270.T', '601633.SS', 'A5SA.F',
                 'ARVL', 'DAI.DE', 'F', 'GM', 'GOEV', 'Honda_logo', 
                 'HYMTF', 'LCID', 'M&M.NS', 'MARUTI.NS', 'NKLA', 'PII',
                 'RIVN', 'RNSDF', 'SEV', 'STLA', 'TM', 'TTM', 'XPEV', 'FUV',
                 'REE', 'CENN')

square_vector <- c('FSR', 'RIDE', '7261.T', '7269.T', '600104.SS', 'BMW.DE',
                   'NIO', 'RACE', 'TSLA', 'VOLCAR-B.ST', 'VOW3.DE', '2207.TW',
                   'LI', '002594.SZ')




symbol_vector <- automakers_data %>% distinct(Symbol)
company_vector <- automakers_data %>% distinct(Name)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  skin="midnight", 
  dashboardHeader(title = "Stock Market Analysis"),
  dashboardSidebar(
    sidebarMenu(id = 'menu1', 
                menuItem("Statistical Overview", tabName = "overview", icon = icon("globe")),
                menuItem("Stock Price Analysis", tabName = "EDA", icon = icon("chart-line")),
                menuItem("Comparsion Analysis", tabName = "com", icon = icon("code-compare")),
                menuItem("Stock Price Forcasting", tabName = "TSA", icon = icon("robot"))
    ),
    conditionalPanel(condition = "input.menu1 == 'overview'",
                     tags$hr(),
                     h3("Automakers"),
                     p("Select a continent you want to overview"),
                     selectInput(inputId = "automakers",
                                 label = "Different Continents",
                                 choices = c("Europe", "Asia", "North America", "Middle East"))),
    
    conditionalPanel(condition = "input.menu1 == 'EDA'",
                     tags$hr(),
                     h3("Automakers"),
                     p("Select a automakers symbol you want to analysis"),
                     selectInput(inputId = "symbol",
                                 label = "Different Automakers Symbol",
                                 choices = symbol_vector),
                     dateRangeInput(inputId = 'dateRange',
                                    label = 'Date range input',
                                    start = '2009-12-31', end = '2022-02-08'
                     )), 
    conditionalPanel(condition = "input.menu1 == 'com'",
                     tags$hr(),
                     h3("Compare"),
                     p("You can get the table by selecting any automakers and any date"),
                     selectInput(inputId = "company", 
                                 label = "Compare different companys",
                                 choices = symbol_vector, 
                                 multiple = TRUE),
                     selectInput(inputId = "price",
                                 label = "Different price",
                                 choices = c("Close", "High", "Low", "Open", "Volume")),
                     dateInput(inputId = "date",
                               label = "Select date",
                               value = "2010-01-04")),
    conditionalPanel(condition = "input.menu1 == 'TSA'",
                     tags$hr(),
                     p("Select a automakers symbol you want to forecast"),
                     selectInput(inputId = "symbol1",
                                 label = "Different Automakers Symbol",
                                 choices = symbol_vector),
                     selectInput(inputId = "DateInput",
                                 label = "How many days you want to forecast",
                                 choices = c(5, 10, 50, 100, 500))),
    width = 250
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h3("Overview of each continents"),
              p("For this section, we can overiew the automakers from different continents. We can see for each continents,
               how many automakers company it is in the top 48"),
              tags$br(),
              p("Control here"),
              fluidRow(
                tabBox(width=12,
                       id = "tabset1", #ID lets us use input$tabset1 on the server to find the current tab.
                       tabPanel("Overview", "Number of automakers in each country of selected continents", 
                                tableOutput("crossTab"), 
                                p("The following barchart are showing the difference between these automakers"),
                                plotOutput("barplot1", height=300)),
                       tabPanel("Summary", "Summary statistics of each variable.", 
                                tableOutput("Sum"),
                                p("The following piechart are showing the percentage of each country"),
                                plotlyOutput('pie')))
              ) ## End of first tabBox
              ## End of first fluidRow
              
              #fluidRow(
              #box(width=12, plotOutput("barplot1", height=300)))
              #box(width=7, plotOutput("scatter1", height=250)) #Scatterplot of two covariates
              
      ),## End of tabItem
      tabItem(tabName = "EDA",
              imageOutput('logo', width = "100%", height = 160 ),
              textOutput("txt"),
              tags$br(),
              h3("The Candlestick Chart"),
              p("For this section, you can see the candlestick chart by selecting different automakers"),
              tags$br(),
              plotlyOutput("Candlestick", height = 500)
      ),
      tabItem(tabName = "com",
              h3("Comparison of selected company"),
              p("For this section, you can compare any companys on a particular day."),
              tags$br(),
              p("You can selected different companys and particular date to overview the basic stock price information. 
                If the information of the selected companys do not show up, it means there is no stock price 
                information of that company in that particular day"),
              tableOutput("comptable"),
              tags$br(),
              p("The following line chart are showing the particular price of particular automakers. 
                You have to be careful for this section, you can only compare two companies each time."),
              fluidRow(
                box(width = 12, plotlyOutput("Linechart", height = 300))
              )),
      tabItem(tabName = "TSA",
              h3("Close Price Forecasting"),
              p("For this section, we will forecast the trends of the close price for selected company"),
              plotOutput('Autoplot'))
    ) ## End of dashboardBody
  )) ## End of dashboardPage

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$crossTab = renderTable({automakers_data %>%
      filter(Continents == input$automakers) %>%
      dplyr::select(country, Name, Symbol, marketcap, Rank) %>%
      arrange(Rank)
  })
  output$Sum = renderTable({automakers_data %>%
      dplyr::filter(Continents  == input$automakers) %>%
      group_by(country) %>%
      dplyr::summarise(Number = n()) %>%
      arrange(desc(Number))})
  
  output$pie = renderPlotly({
    sum_table <- automakers_data %>%
      dplyr::filter(Continents  == input$automakers) %>%
      group_by(country) %>%
      dplyr::summarise(Number = n()) %>%
      arrange(desc(Number))
    
    fig <- plot_ly(sum_table, labels = ~country, values = ~Number, type = 'pie') %>% 
      layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           fig_bgcolor   = "rgba(0, 0, 0, 0)")
    fig
  })
  
  
  output$barplot1 = renderPlot({automakers_data %>%
      filter(Continents  == input$automakers) %>%
      ggplot(aes(x = reorder(Name, -marketcap), y = marketcap)) +
      geom_bar(stat = 'identity')
  })
  
  output$Candlestick = renderPlotly({
    data <- stock_price %>%
      filter(Symbol  == input$symbol) %>%
      filter(Date > input$dateRange[1]) %>%
      filter(Date < input$dateRange[2])
    
    # colors column for increasing and decreasing
    for (i in 1:length(data[,1])) {
      if (data$Close[i] >= data$Open[i]) {
        data$direction[i] <- 'Increasing'
      } else {
        data$direction[i] <- 'Decreasing'
      }
    }
    
    i <- list(line = list(color = '#17BECF'))
    d <- list(line = list(color = '#7F7F7F'))
    
    # plot candlestick chart
    
    fig <- data %>% plot_ly(x = ~Date, type="candlestick",
                          open = ~Open, close = ~Close,
                          high = ~High, low = ~Low, name = input$symbol,
                          increasing = i, decreasing = d)
    fig <- fig %>% layout(yaxis = list(title = "Price"))
    
    # plot volume bar chart
    fig2 <- data %>% plot_ly(x=~Date, y=~Volume, type='bar',
                             from = input$dateRange[1],
                             to = input$dateRange[2],
                             color = ~direction, colors = c('#17BECF','#7F7F7F')) 
    fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
    
    # subplot with shared x axis
    fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                   shareX = TRUE, titleY = TRUE)
    fig <- fig %>% layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                        xanchor = 'center', yref = 'paper',
                                        font = list(size = 10),
                                        bgcolor = 'transparent'))
    
    fig
  })
  
  output$Linechart = renderPlotly({
    date <- as.data.frame(unique(stock_price$Date))
    colnames(date) <- 'Date'
    for(item in input$company){
      data <- stock_price %>% filter(Symbol == item) %>% dplyr::select(Date, input$price)
      colnames(data) <- c('Date', item)
      #print(data)
      date <- merge(date, data, by = 'Date')
    }
    #print(symbol_vector[1])
    fig <- date %>%
      plot_ly(x = ~Date, y = ~get(input$company[1]), type = 'scatter', mode = 'lines')%>%
      layout(showlegend = F)
    for(item in input$company[2:length(c(input$company))]){
      fig <- fig %>% add_trace(y = ~get(item))
    }
    fig
  })
  
  output$comptable = renderTable({stock_price %>%
      filter(Date == input$date) %>%
      filter(Symbol %in% c(input$company)) %>%
      dplyr::select(Symbol, Adj.Close, Close, Open, High, Low, Volume) %>%
      arrange(desc(Volume))
  })
  
  output$logo = renderImage({
    if (input$symbol %in% svg_vector) {
      if (input$symbol %in% rect_vector){
        return(list(
          src = file.path(".", "All_images", paste(input$symbol, 'svg', sep =".")),
          contentType = "image/svg+XML",
          width = 300,
          height = 150
        ))
      }else if (input$symbol %in% square_vector){
        return(list(
          src = file.path(".", "All_images", paste(input$symbol, 'svg', sep =".")),
          contentType = "image/svg+XML",
          width = 150,
          height = 150
        ))
      }
    }else if(input$symbol %in% png_vector){
      if(input$symbol %in% rect_vector){
        return(list(
          src = file.path(".", "All_images", paste(input$symbol, 'png', sep =".")),
          contentType = "image/png",
          width = 300,
          height = 100
        ))
      }else if (input$symbol %in% square_vector){
        return(list(
          src = file.path(".", "All_images", paste(input$symbol, 'png', sep =".")),
          contentType = "image/png",
          width = 150,
          height = 150))
      }
    }
  }, deleteFile = FALSE)
  
  output$txt = renderText({
    new_data <- automakers_data %>%
      filter(Symbol == input$symbol)
    new_data$History
  })
  output$Autoplot = renderPlot({
    BMW <- stock_price %>% 
      filter(Symbol == input$symbol1) %>%
      dplyr::select('Date', 'Close')
    
    BMW$Date <- as.Date(BMW$Date, "%d/%m/%Y")
    tsData = ts(BMW)
    fit<-forecast::auto.arima(BMW$Close,trace=TRUE)
    autoplot(forecast(fit,input$DateInput)) + xlab("Time") + ylab("Close")
  })
}

shinyApp(ui = ui, server = server)