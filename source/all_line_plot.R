
values <- reactiveValues()


observeEvent(input$submit,{
  req(input$stickers, input$date_from,input$date_to)
  #symbols
  symbols <-  input$stickers
  # Subtitle
  subtitle <- sticker %>% filter(symbol %in% input$stickers)
  date_from <- input$date_from
  date_to <- input$date_to
  
  # print(paste0("Sticker: ", symbols))
  # print(paste0("Date from: ", date_from))
  # print(paste0("Date to: ", date_to))
  
  Stocks <- lapply(symbols, function(i) get_stock(i, date_from, date_to, periodicity = "daily"))
  Stocks <- setNames(Stocks, symbols)
  list2env(Stocks, envir = .GlobalEnv)
  
  # stocks data
  stocks <- lapply(Stocks, function(i) i[[1]])
  values$stocks <- do.call("rbind", stocks )
  
  
  # return data
  returns <- lapply(Stocks, function(i) i[[2]])
  returns <- do.call("rbind", returns)
  
  # last trade data
  last_trade <- lapply(Stocks, function(i) i[[3]])
  
  
  lit <- list(stocks,symbols,subtitle, returns,last_trade)
  
  # values$data contained stocks data, Symbols, Subtitle, return data and last trade data
  values$data <- lit
  
  #print(str(values$data ))
  #=================================
  name <- values$data[[2]]
  dt <- values$data[[1]]
  subtitle  <- values$data[[3]]
 
  subtitle <- gsub("\\(.*","",subtitle$name)
  last_trade <- values$data[[5]]
  returns  <- values$data[[4]]
  values$plot_out_line_dt <- returns
  
  # Use return data to plot list of stocks
  output$plot_out_line <- renderHighchart({

    p <- highchart() %>%
      hc_yAxis_multiples(
        create_axis(2,  height = c(3.5, 1.5), title = list(list(text = "Returns"),list(text = "Volume")))
      ) %>%
      highcharter::hc_title(text = "",offset= 100) %>%
      highcharter::hc_add_series(data = returns ,
                                 "line",
                                 yAxis = 0,
                                 marker=list(enabled=F),
                                 showInLegend = FALSE,
                                 hcaes(x = Date, y = Return, group = Name)) %>%
      highcharter::hc_add_series(data = returns,
                                 "column",
                                 hcaes(x = Date, y = Volume, group = Name),
                                 yAxis = 1,
                                 showInLegend = TRUE) %>% 
      hc_colors(mypalette[1:length(unique(returns$Name))]) %>%
      highcharter::hc_tooltip(crosshairs = TRUE,
                              shared = TRUE,
                              borderWidth = 1,
                              headerFormat = "<b>{point.x}</b><br>") %>%
      highcharter::hc_xAxis(type = 'datetime', categories=unique(returns$Date), title=NULL,labels = list(format = '{value:%b %d}')) %>%
      highcharter::hc_exporting(enabled=TRUE) %>%
      highcharter::hc_plotOptions(marginBottom = "0px",
                                  marginTop = "100px",
                                  marginBottom = "0px",
                                  marginBottom = "0px",
                                  line =list(lineWidth=1.5),
                                  series = list(events = list(click = canvasClickFunction,
                                                legendItemClick = legendClickFunction)))
    values$plot_out_line <- p
    
    
    
  })
  
  title_line_out <- HTML(paste0("<span style='font-size:14px;'>",paste0(paste0("<span style='font-size:18px;color:#d91e49;font-weight:bold;'>",name,"</span>"), collapse = " vs "),"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;","","</span>",
                                    "<span style='font-size:17px;'>", date_from, " - ", date_to,"</span>"))
  values$title_line_out <- title_line_out
  
  # Title of the box
  output$title_line  <- renderText(
    values$title_line_out
  )
  
  # render everything in a box
  output$plot_line <- renderUI({
 
    tags$fieldset(tags$legend(div(id = "plot_line",htmlOutput("title_line"))),
                  bsTooltip(id = "plot_line", paste0(subtitle , collapse = "<br>")),
         dlUI("dl_plot_out_line"),        
        highchartOutput("plot_out_line", height = "300")
    )
  })
  #==========================
  
  output$boxes <- renderUI({
    fluidRow(
    lapply(1:length(name), function(a, name, dt, subtitle,last_trade) {

      output[[name[[a]]]] <- renderText(
        HTML(paste0("<span style='font-size:40px;'>",format(last_trade[[a]]$Last,nsmall=2,big.mark=","),"</span><span style='font-size:18px;'> USD</span>"))
      )

      output[[paste0("vol_",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:12px;'>Volume: ",format(dt[[a]]$Volume[1], nsmall=0,big.mark=","),"</span><span style='font-size:12px;'> </span>"))
      )

      output[[paste0("low_",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:12px;'>Low: ",sprintf("%.2f",dt[[a]]$Low[1]),"</span><span style='font-size:12px;'> </span>"))
      )

      output[[paste0("name_",name[[a]])]] <- renderText(
        HTML(paste0("<span style='font-size:20px;color:#d91e49;font-weight:bold;'>",name[[a]],"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;",subtitle[[a]],"</span>"))
      )

      output[[paste0("ch_",name[[a]])]] <- renderText(
        HTML(last_trade[[a]]$change_out)
      )

      output[[paste0("time_",name[[a]])]] <- renderText(
        HTML(last_trade[[a]]$Time)
      )
      column(width = 4,
      tags$fieldset(tags$legend(htmlOutput(paste0("name_",name[[a]]))),
        div(style = "padding-left:20px; padding-right:20px;",
        htmlOutput(name[[a]],style = "text-align:center;"),br(),
        splitLayout(cellWidths = c("70%", "30%"),cellArgs = list(style='min-height: 35px;'),
                    htmlOutput(paste0("ch_",name[[a]]),style = "text-align:left;padding-left:10px;font-size:22px;color:#32907c"),
                    div(style ="text-align:center;hight:35px;",
                        actionButton(name[[a]], label = "Explore", class = "css-selector",
                                     onclick = "Shiny.setInputValue('btnLabel', this.id);",  #onclick = "Shiny.setInputValue('btnLabel', this.this.innerText);", to capture label
                                     style = "text-align:center;text-color:white;background-color:#F6F4F3;border-color:#F6F4F3;box-shadow: 2px 2px 2px 0px #003b9a;"))
        ),
        htmlOutput(paste0("time_",name[[a]]),style = "text-align:left;padding-left:10px;font-size:15px;color: #003b9a;"),
        fluidRow(htmlOutput(paste0("vol_",name[[a]]),style = "text-align:right;padding-right:10px;font-size:15px;")))
      ))
    }, name = name, dt = dt, subtitle = subtitle,last_trade = last_trade)
   )
  })
  
})




observeEvent(values$plot_out_line , {
  callModule(dlServer, "dl_plot_out_line", file=reactive(values$plot_out_line ) ,data = reactive(values$plot_out_line_dt), zip = TRUE)
})



#===============================================

observeEvent(input$canvasClicked, {
  date = input$canvasClicked[2]
  data <- values$stocks %>% filter(Date == date) %>% dplyr::select(1:8)
  data[,-c(1,2)] <- lapply(data[,-c(1,2)], function(i) round(i, 1))
  row.names(data) <- NULL
  #print(data)


  showModal(modalDialog(
    title = paste0(paste0(data$Name, collapse = ", "), " Prices on ",date),
    reactable(data,
                    searchable = FALSE,
                    fullWidth = TRUE,
                    defaultPageSize = 10,
                    defaultColDef = colDef(
                      headerStyle = list(background = '#FFFFFF', color = '#012c70', align= "left")
                    ),
                    theme = reactableTheme(
                      borderColor = "#dfe2e5",
                      stripedColor = "#f6f8fa",
                      highlightColor = "#f0f5f9",
                      cellPadding = "7px 10px 7px 30px", #top, right, bottom, left
                      style = list(
                        fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
                        color = "#012c70",fontSize = 15
                      ),
                      searchInputStyle = list(width = "100%")),
                    columns = list(
                      Date = colDef(show = FALSE),
                      Symbol = colDef(
                        style = list(color = "#012c70",fontSize = 15)
                      ),
                      Name = colDef(
                        minWidth = 100,
                        style = list(color = "#d91e49", fontStyle= "italic", fontSize = 15, fontWeight = "bold")
                      ),
                      Volume = colDef(
                        minWidth = 170,
                        style = list(color = "#012c70", fontStyle= "italic", fontSize = 13),
                        cell = data_bars(data,
                                         bar_height = 8,
                                         text_position =  'outside-base',
                                         number_fmt = scales::comma_format(accuracy = 0.1),
                                         fill_color = "#012c70",
                                         box_shadow = TRUE,
                                         round_edges = TRUE,
                                         text_color = "#d91e49",
                                         background = "transparent")
                      )
                    )
    ),

    easyClose = TRUE,
    footer = NULL
  ))


})


#=========================================

observeEvent(input$dl_report,{
  
  
})







