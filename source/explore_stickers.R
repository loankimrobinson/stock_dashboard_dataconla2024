



observeEvent(input$btnLabel,{
  req(input$submit)
  stock <- values$data[[1]]
  stock <- stock[[input$btnLabel]]
  
  
  last_trade <- values$data[[5]]
  last_trade <- last_trade[[input$btnLabel]]
  
  names <- input$btnLabel
  subtitle <- sticker %>% filter(symbol %in% input$btnLabel)
  subtitle <- gsub("\\(.*","",subtitle$name)
  
  
  # ggplot2
  output$ggplot_area <- renderPlot({
    
    min <- min(stock$Open,na.rm = TRUE)-(min(stock$Open,na.rm = TRUE)/10)
    max <- max(stock$Open,na.rm = TRUE)
    grad_df <- data.frame(yintercept = seq(0, max(stock$Open,na.rm = TRUE), length.out =  max(stock$Open,na.rm = TRUE)*3),
                          alpha = seq(1,0.0, length.out =  max(stock$Open, na.rm = TRUE)*3))
    
    p <- ggplot2::ggplot(stock, aes(x= Date, y = Open)) +
      geom_area(fill = "#003b9a", alpha = 0.9) + 
      geom_hline(data = grad_df, aes(yintercept = yintercept, alpha = alpha),
                 size = 1, colour = "white") +
      geom_line(colour = "#003b9a", size = 1) +
      coord_cartesian(ylim = c(min, max))
    p <- p +labs(x ="Date", y = "Price ($)")
    p <- p + theme(legend.position = "none", 
                   panel.background = element_rect(fill = "transparent",
                                                   colour = NA_character_), # necessary to avoid drawing panel outline
                   panel.grid.major = element_blank(), # get rid of major grid
                   panel.grid.minor = element_blank(), # get rid of minor grid
                   plot.background = element_rect(fill = "transparent",
                                                  colour = NA_character_), # necessary to avoid drawing plot outline
                   legend.background = element_rect(fill = "transparent"),
                   legend.box.background = element_rect(fill = "transparent"),
                   legend.key = element_rect(fill = "transparent")) 
    print(p)
    
    
  })
  
  # plotly
  output$plot_out <- renderPlotly({
    plot_function(stock, names)
  })
  
  output$title  <-  output$title_1 <- renderText(
    HTML(paste0("<span style='font-size:20px;color:#d91e49;font-weight:bold;'>",names,"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;",subtitle[1],"</span>"))
  )
  
  output$dl <- renderUI({
    actionButton("dl",label = NULL, icon = icon("save"), style = "text-align:center;text-color:white;background-color:#F6F4F3;border-color:#F6F4F3;
                 box-shadow: 1px 2px 2px 0px #003b9a;")
  })
  
  # Plotly
  output$plot <- renderUI({
    
    tags$fieldset(tags$legend(htmlOutput("title")),
                  div(style = "padding-left:20px; padding-right:20px;",
                      plotlyOutput("plot_out", height = "300"))
    )
  })
  
  #ggplot2
  output$plot_area <- renderUI({
    tags$fieldset(tags$legend(htmlOutput("title_1")),
                  div(class = "legend2",tags$legend(uiOutput("dl"))),
      div(style = "padding-left:20px; padding-right:20px;",
      plotOutput("ggplot_area",height = "300"))
    )
  })
  
})



# ARIMA:   Autoregressive Integrating Moving Average Model -Forecasting -Arima Results
# GARCH:   Generalized Autoregressive Conditional Heteroscedastic Model -Garch Forecasting -Garch Results
# Prophet-Prophet Forecasting-Prophet Results
# K-NN regression time series forecasting
# Feed Foward Neural network

# observeEvent(input$btnLabel,{
#   req(input$submit)
#   
#   names <- input$btnLabel
#   subtitle <- sticker %>% filter(symbol %in% input$btnLabel)
#   subtitle <- gsub("\\(.*","",subtitle$name)
#   
#   
# })


