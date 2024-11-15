
values <- reactiveValues(btnexp = "crypto")

observe({
  if(is.null(input$btnexp)){
    values$btnexp <- "crypto" 
  }else{
    values$btnexp <- input$btnexp
  }
})


observeEvent(values$btnexp,{

  if(values$btnexp == "trending"){
    dt <- trend_tickes()[1,]
    symbols <- dt$Symbol
    subtitle <- dt$Name
  }else if(values$btnexp == "gainers"){
    dt <- gainers()[1,]
    symbols <- dt$Symbol
    subtitle <- dt$Name
  }else if(values$btnexp == "losers"){
    dt <- losers()[1,]
    symbols <- dt$Symbol
    subtitle <- dt$Name
  }else{
    dt <- crypto()[1,]
    symbols <- dt$Symbol
    subtitle <- dt$Name
  }
  
  date_from <- Sys.Date()-720
  date_to <- Sys.Date()
  Stocks <- lapply(symbols, function(i) get_stock(i, date_from, date_to, periodicity = "daily"))
  Stocks <- setNames(Stocks, symbols)
  list2env(Stocks, envir = .GlobalEnv)
  
  
  stock <- Stocks[[1]][[1]]
  stock <- stock[!is.na(stock$Open),]
  last_trade <- Stocks[[1]][[3]]
  names <- symbols

  values$stock_ggplot_area_exp <- stock
  # ggplot2
  output$ggplot_area_exp <- renderPlot({

    min <- min(stock$Open,na.rm = TRUE)-(min(stock$Open,na.rm = TRUE)/10)
    max <- max(stock$Open,na.rm = TRUE)
    
    grad_df <- data.frame(yintercept = seq(0, max(stock$Open,na.rm = TRUE), length.out = 500),
                          alpha = seq(1,0.0, length.out = 500))

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
  output$plot_out_exp <- renderPlotly({
    p <- plot_function(stock, names)
    values$plot_out_exp <- p

  })

  output$title_exp  <-  output$title_1_exp <- renderText(
    HTML(paste0("<span style='font-size:20px;color:#d91e49;font-weight:bold;'>",names,"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;",subtitle,"</span>"))
  )

  # Plotly
  output$plot_exp <- renderUI({
    tags$fieldset(tags$legend(htmlOutput("title_exp")),
                  dlUI("dl_exp"),
                  div(style = "padding-left:20px; padding-right:20px;",
                      plotlyOutput("plot_out_exp", height = "300"))
    )
  })

  #ggplot2
  output$plot_area_exp <- renderUI({
    tags$fieldset(tags$legend(htmlOutput("title_1_exp")),
      div(style = "padding-left:20px; padding-right:20px;",
      plotOutput("ggplot_area_exp",height = "300"))
    )
  })
  
})

observeEvent(values$plot_out_exp, {
  callModule(dlServer, "dl_exp", file=reactive(values$plot_out_exp) ,data = reactive(values$ggplot_area_exp), zip = TRUE)
})

