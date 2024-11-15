
#install.packages("shinyBS", repos = "https://cloud.r-project.org")

library(plotly)
library(shiny)
library(shinyalert)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(scales)
library(kableExtra)
library(htmlwidgets)
library(dplyr)
library(shinyWidgets)
library(quantmod)
library(RColorBrewer)
library(highcharter)
library(shinyBS)
library(shinycssloaders)
library(reactable)
library(reactablefmtr)
library(rvest)
library(purrr)


options(shiny.maxRequestSize=100*1024^2)

nyse <- read.csv("data/nyse.csv", stringsAsFactors = FALSE)
nasdaq  <- read.csv("data/nasdaqcsv.csv", stringsAsFactors = FALSE)
constituents <- read.csv("data/constituents.csv", stringsAsFactors = FALSE)
other <- read.csv("data/other-listed.csv", stringsAsFactors = FALSE)
sticker <- rbind(nasdaq, nyse[,c(1,2)], constituents[, c(1,2)], other[,c(1,2)])
sticker <- sticker[duplicated(sticker$symbol),]
sticker$symbol[sticker$symbol == "FB"] <- "META"
sticker$choices <- paste0("<span style = 'color:#d91e49;font-style:italic;font-weight:bold; font-size:15px;'>",sticker$symbol,"</span>", 
                          "<span style = 'color:#003b9a;font-style:italic;font-size:12px;font-weight:bold;'> (",sticker$name, ")</span>")
sticker <- sticker[!sticker$symbol %in% c("ABMD","ATVI"),]

#============================

canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")

get_stock <- function(symbol = "GOOG", date_from = Sys.Date() - (365*2), date_to = Sys.Date(),periodicity= "daily"){
  
  if(weekdays(date_to) == "Saturday"){
    date_to = date_to - 1
  }else if(weekdays(date_to) == "Sunday"){
    date_to = date_to - 2
  }else{
    date_to = date_to
  }
  dt <- getSymbols(symbol,from = date_from,to = date_to,warnings = FALSE,verbose = FALSE,symbol.lookup = TRUE,auto.assign = FALSE,src = 'yahoo',periodicity = periodicity)
  dt <- na.omit(dt)
  previous_day <- getSymbols(symbol, from = Sys.Date()-5,to = Sys.Date(),warnings = FALSE,verbose = FALSE,symbol.lookup = TRUE,auto.assign = FALSE,src = 'yahoo',periodicity = "daily")
  
  if(weekdays(Sys.Date()) %in% c("Saturday","Sunday")){
    present_day <- previous_day[nrow(previous_day), ]
    previous_day <- previous_day[nrow(previous_day)-1, ]
  }else{
    present_day <- getSymbols(symbol,from = Sys.Date(),to = Sys.Date()+1,warnings = FALSE,verbose = FALSE,symbol.lookup = TRUE,auto.assign = FALSE,src = 'yahoo',periodicity = "daily")
    previous_day <- previous_day[nrow(previous_day), ]
  }
  last_trade <- data.frame(Time = Sys.Date(),
                           Last = round(present_day[,4],2),
                           Close = previous_day[,4],
                           Change = as.numeric(present_day[,4])- as.numeric(previous_day[,4]), 
                           P.Change = (as.numeric(present_day[,4])- as.numeric(previous_day[,4]))/as.numeric(previous_day[,4]))
  colnames(last_trade) <- c("Time","Last","Close","Change","P.Change")

  last_trade <- last_trade %>% mutate(Time = format(last_trade$Time, "%b %d, %Y"),
                                      change_out = ifelse(Change >= 0, 
                                                           paste0("<span style='color:#32907c;'>", "+", sprintf("%.2f",Change), " ( ", sprintf("%.2f",P.Change), "%) &#11014; </span>"),
                                                           paste0("<span style='color:#990000;'>",sprintf("%.2f",Change), " ( ", sprintf("%.2f",P.Change), "%) &#11015;</span>")),
                                      change_out_plot = ifelse(Change >= 0, 
                                                                paste0( "+", sprintf("%.2f",Change), " ( ", sprintf("%.2f",P.Change), "%)"),
                                                                paste0( sprintf("%.2f",Change), " ( ", sprintf("%.2f",P.Change), "%)")))
  
  data_return <- data.frame(periodReturn(dt[,6],period = "monthly", type = "arithmetic"),dt[endpoints(dt,'months'), 5]) 
  data_return$Name <- symbol
  data_return$Date <- row.names(data_return)
  data_return <- data_return[,c(3,4,1,2)]
  names(data_return) <- c("Name","Date","Return","Volume")
  data_return$Return <- round(data_return$Return, 2)
  
  dt <- data.frame(Name = symbol,Date=index(dt),coredata(dt))
  row.names(dt) <- NULL
  names(dt) <- c("Name","Date","Open","High","Low","Close","Volume","Adjusted")
  dt <- dt %>% mutate(ch = Close- lead(Close),
                      direction = ifelse(ch >= 0,'Increasing','Decreasing'),
                      pch = (Close- lead(Close))/lead(Close),
                      change = sprintf("%.2f",ch),
                      per_ch = sprintf("%.2f",pch *100),
                      change_out = ifelse(direction == "Increasing", 
                                           paste0("<span style='color:#32907c;'>", "+", change, " ( ", per_ch, "%) &#11014; </span>"),
                                           paste0("<span style='color:#FCF7B9;'>",change, " ( ", per_ch, "%) &#11015;</span>"))) %>% arrange(desc(Date))
  
  #create Bollinger Bands
  bbands <- BBands(dt[,c("High","Low","Close")])
  dt <- cbind(dt, bbands)
  return(list(dt,data_return,last_trade))
}
# Test function
# get_stock("GOOG", "2022-11-21","2024-11-10") # FACEBOOK
# get_stock("NVDA", "2022-11-21","2024-11-10") # NVIDIA Corp
# get_stock("GOOG", "2022-11-21","2024-11-10") # Intel Corp
# get_stock("MSFT", "2022-11-21","2024-11-10") # Microsoft Corp

mypalette <- c(brewer.pal(12,"Paired"),brewer.pal(8,"RdBu"))

trend_tickes <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/trending-tickers"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))
  return(out)
}
# trend_tickes()

sector <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/sectors"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  if(isTRUE(all(df[[1]][,2] == df[[1]][,3]))){
    df[[1]][,3] <- df[[1]][,4]
  }
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% dplyr::select(1:3) 
  names(out) <- c("Sector","Weight","Return")
  out$color <- ifelse(as.numeric(gsub("%","",out$Return)) < 0,"#d91e49", "#003b9a")
  return(out)
}
# sector()

gainers <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/gainers/?offset=0&count=100"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))
  return(out)
}
# gainers()

losers <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/losers/?offset=0&count=100"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))
  return(out)
}
# losers()

currences <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/currencies"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))
  return(out)
}
# currences()

crypto <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/crypto"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))
  
  return(out)
}
# crypto()

top_efts <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/etfs"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))

  return(out)
}
# top_efts()

top_mutual_funds <- function(url = "https://finance.yahoo.com/trending-tickers"){
  url <- "https://finance.yahoo.com/mutualfunds"
  df <- url %>%
    read_html() %>%
    html_table(header = TRUE)
  select_name <- names(df[[1]])[!names(df[[1]]) %in% c("", " ")]
  out <- df[[1]] %>% dplyr::select(all_of(select_name)) %>% select_if(~sum(!is.na(.)) > 0) %>% mutate(color = ifelse(Change < 0,"#d91e49", "#003b9a"))

  return(out)
}
# top_mutual_funds()

get_summary_table <- function(symbol){
  url <- paste0("https://finance.yahoo.com/quote/",symbol)
  df <- url %>%
    read_html() %>%
    html_table(header = FALSE) %>%
    map_df(bind_cols) %>%
    as_tibble()
  # names(df) <- c("name", "value")
  # df["stock"] <- symbol
  return(df)
}

plot_function <- function(stock,name_stock){
  # cutom colors
  i <- list(line = list(color = "#003b9a"))
  d <- list(line = list(color = '#d7d7d7'))

  p1 <- stock %>%
    plot_ly(x = ~Date,
            type = "candlestick",
            open = ~Open,
            close = ~Close,
            high = ~High,
            low = ~Low,
            increasing = i, decreasing = d,
            name = "price") %>%
    layout(
      legend = "none",
      xaxis = list(
         rangeselector = list(
          font = list(size = 12),
          y = 0.95,
          x = 0.2,
          buttons = list(
            list(
              count = 1,
              label = "1 month",
              step = "month",
              stepmode = "backward"),
            list(
              count = 3,
              label = "3 month",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6,
              label = "6 month",
              step = "month",
              stepmode = "backward"),
            list(
              count = 9,
              label = "9 month",
              step = "month",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 year",
              step = "year",
              stepmode = "backward"),
            list(count=1,label='RESET',step = "all"))),
        rangeslider = list(visible = TRUE)),
      yaxis = list(title = "Price ($)",
                   showgrid = TRUE,
                   showticklabels = TRUE))

  fig <- p1 %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                          line = list(color = '#ccc', width = 0.5),
                          legendgroup = "Bollinger Bands",
                          hoverinfo = "none", inherit = F)
  fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                           line = list(color = '#ccc', width = 0.5),
                           legendgroup = "Bollinger Bands", inherit = F,
                           showlegend = FALSE, hoverinfo = "none")
  fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                           line = list(color = '#E377C2', width = 0.5),
                           hoverinfo = "none", inherit = F)
  fig <- fig %>%  layout(legend = "none")

  # plot volume bar chart
  fig2 <-  stock
  fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume",
                           color = ~direction, colors = c("#003b9a",'#d7d7d7'))
  fig2 <- fig2 %>% layout(yaxis = list(title = "Volume",size = 12,color = "white",gridcolor = toRGB("white")), xaxis = list(size = 12,color = "white"),legend = "none")
  fig3 <- plotly::subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,shareX = TRUE, titleY = TRUE)
  fig3 <- fig3 %>% layout(
                        xaxis = list(size = 12,color = "white",linecolor = "#353c42"),
                        yaxis = list(size = 12,color = "white",linecolor = "#353c42",gridcolor = toRGB("gray20")),
                        legend = "none",
                        showlegend = FALSE,
                        plot_bgcolor  = "rgba(0, 0, 0, 0, 0.0)",
                        paper_bgcolor = "rgba(0, 0, 0, 0,0.0)",
                        fig_bgcolor   = "rgba(0, 0, 0, 0,0.0)")

  return(fig3)

}

#====================================





