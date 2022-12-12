automakers_data = read.csv('/Users/chenzhang/Desktop/UG/F22 Tony/DATA6200/Final Project/Final project data/Largest automakers by market capitalization.csv')
stock_price = read.csv('/Users/chenzhang/Desktop/UG/F22 Tony/DATA6200/Final Project/Final project data/automakers stocks 2010-2022.csv')
library(plotly)
library(scales)
library(quantmod)


continent_table <- automakers_data %>%
  filter(Continents  == 'Asia') %>%
  group_by(country) %>%
  summarise(count = n())


continent_table <- automakers_data %>%
  filter(Continents  == 'Asia') %>%
  dplyr::select(country, Name, Symbol, marketcap, Rank)

barplot1 <- automakers_data %>%
  filter(Continents  == 'Asia') %>%
  arrange(desc(marketcap)) %>%
  ggplot(aes(x = Name, y = marketcap)) +
  geom_bar(stat = 'identity')


# annotation
a <- list(text = "Stock Split",
          x = '2016-03-04',
          y = 1.02,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)

# use shapes to create a line
l <- list(type = line,
          x0 = '2016-03-04',
          x1 = '2016-03-04',
          y0 = 0,
          y1 = 1,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 0.5)
)


fig <- stock_price %>%
  filter(Symbol == 'TSLA') %>% 
  plot_ly(x = ~Date, type="candlestick",
          open = ~Open, close = ~Close,
          high = ~High, low = ~Low) %>% layout(title = "Candlestick Chart")
fig <- fig %>% layout(title = "Basic Candlestick Chart")

fig


stock_price %>%
  filter(Symbol  == 'TSLA') %>%
  plot_ly(type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~Close) %>%
  layout(showlegend = F, xaxis = list(zerolinecolor = '#282f30',
                                      zerolinewidth = 2,
                                      gridcolor = 'ffff'),
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'), plot_bgcolor='#777c7d')
stock_price$Date = as.Date(stock_price$Date)

stock_price %>%
  filter(Symbol == 'TSLA') %>%
  ggplot(aes(Date, Close)) + 
  geom_line() +
  scale_x_date (labels = date_format("%b-%Y")) + 
  xlab("") + ylab("Close")







TSLA_data = stock_price %>%
  filter(Symbol  == 'TSLA')

# create Bollinger Bands
bbands <- BBands(TSLA_data[,c("High","Low","Close")])

# join and subset data
df <- subset(cbind(TSLA_data, data.frame(bbands[,1:3])))

# colors column for increasing and decreasing
for (i in 1:length(TSLA_data[,1])) {
  if (df$Close[i] >= df$Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~Open, close = ~Close,
                      high = ~High, low = ~Low, name = "TSLA",
                      increasing = i, decreasing = d)

# plot volume bar chart
fig2 <- df 
fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume",
                         color = ~direction, colors = c('#17BECF','#7F7F7F')) 
fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))

# subplot with shared x axis
fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)
fig <- fig %>% layout(legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'), annotations = a, shapes = l)

fig



stock_price %>%
  group_by(Symbol) %>%
  summarise(average_close = mean(Close, na.rm = TRUE)) %>%
  filter(Symbol %in% c('TSLA','BMW.DE'))

stock_price %>% 
  ggplot(aes(Date, Close, group=c('TSLA', "BMW.DE"))) + 
  geom_line()

fig <- stock_price %>%
  plot_ly(type = 'scatter', mode = 'lines')

add_trace(fig, x = ~Date, y = ~Close, name = 'TSLA')%>%
  layout(showlegend = F)



plot_ly()%>%
  add_trace(data = stock_price, type = 'scatter', mode = 'lines', fill = 'tozeroy',
            x = ~Date, y = ~Close, name = '0489.HK') %>%
  layout(showlegend = F, yaxis = list(zerolinecolor = '#ffff',
                                      zerolinewidth = 2,
                                      gridcolor = 'ffff'),
         xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6')




stock_price %>%
  filter(Date == '2010-01-04') %>%
  filter(Symbol %in% c('TM', 'F')) %>%
  select(Symbol, Adj.Close, Close, Open, High, Low)


sum1 <- automakers_data %>%
  dplyr::filter(Continents  == 'Asia') %>%
  group_by(country) %>%
  dplyr::summarise(Number = n()) %>%
  arrange(desc(Number))

fig <- plot_ly(sum1, labels = ~country, values = ~Number, type = 'pie')
fig


a <- list(text = "Stock Split",
          x = input$date,
          y = 1,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)

# use shapes to create a line
l <- list(type = line,
          x0 = input$date,
          x1 = input$date,
          y0 = 0,
          y1 = 1,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 1)
)
data = stock_price %>%
  filter(Symbol  == input$symbol) %>%
  filter(Date > input$dateRange[1]) %>%
  filter(Date < input$dateRange[2])

# create Bollinger Bands
bbands <- BBands(data[,c("High","Low","Close")])

# join and subset data
df <- subset(cbind(data, data.frame(bbands[,1:3])))

# colors column for increasing and decreasing
for (i in 1:length(data[,1])) {
  if (df$Close[i] >= df$Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~Open, close = ~Close,
                      high = ~High, low = ~Low, name = input$symbol,
                      from = input$dateRange[1],
                      to = input$dateRange[2],
                      increasing = i, decreasing = d)

# plot volume bar chart
fig2 <- df 
fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume",
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
                                    bgcolor = 'transparent'), annotations = a, shapes = l)

fig
