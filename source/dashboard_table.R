

trending_table_dt <- reactive({
  dt <- trend_tickes(url = "https://finance.yahoo.com/trending-tickers")
  dt$Price <- as.numeric(gsub("\\-.*","",(gsub("\\+.*","",dt$Price))))
  dt <- dt %>% arrange(desc(Price))
  #dt$color_check <- stringr::str_extract(dt$Change, "^\\D+")
  dt$Change  <- as.numeric(gsub("+","",dt$Change, fixed = TRUE))
  dt$color <- ifelse(is.na(dt$Change), "gray",
                     ifelse(dt$Change > 0, "#012c70",
                            ifelse(dt$Change < 0, "#c81c43",
                                   ifelse(dt$Change == 0, "gray","gray"))))
  tb <- reactable(dt,
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
                      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                    ),
                    searchInputStyle = list(width = "100%")),
                  columns = list(
                    color = colDef(show = FALSE),
                    Symbol = colDef(
                      style = list(color = "#012c70",fontSize = 15)
                    ),
                    Name = colDef(
                      minWidth = 250,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13)
                    ),
                    Price = colDef(
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13),
                      cell = data_bars(dt,
                                       bar_height = 8,
                                       text_position =  'outside-base',
                                       number_fmt = scales::comma_format(accuracy = 0.1),
                                       fill_color_ref = "color",
                                       box_shadow = TRUE,
                                       round_edges = TRUE, 
                                       background = "transparent")
                    ),
                    Change = colDef(
                      style = function(value,index) {
                        if(is.na(value)){
                          color = "gray"
                        }else if(value <0 ) {
                          color = "#c81c43"
                        }else if(value > 0) {
                          color = "#012c70"
                        }else if(value == 0){
                          color = "gray"
                        }else{
                          color = "gray"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    ),
                    `Change %` = colDef(
                      style = function(value,index) {
                        if(is.na(dt$Change[index])){
                          color = "gray"
                        }else if(dt$Change[index] <0 ) {
                          color = "#c81c43"
                        }else if(dt$Change[index] >0){
                          color = "#012c70"
                        }else if(dt$Change[index] == 0){
                          color = "gray"
                        }else{
                          color = "gray"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                  )
  )
  return(tb)
  
})

output$trending_table <- renderReactable({
  req(trending_table_dt())
  trending_table_dt()
})

currences_table_dt <- reactive({
  dt <- currences(url = "https://finance.yahoo.com/trending-tickers")
  dt$Price <- as.numeric(gsub("\\-.*","",(gsub("\\+.*","",dt$Price))))
  dt <- dt %>% arrange(desc(Price))
  tb <- reactable(dt,
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
                      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                    ),
                    searchInputStyle = list(width = "100%")),
                  columns = list(
                    color = colDef(show = FALSE),
                    Symbol = colDef(
                      minWidth = 150,
                      style = list(color = "#012c70",fontSize = 15)
                    ),
                    Name = colDef(
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13)
                    ),
                    Price = colDef(
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13),
                      cell = data_bars(dt,
                                       bar_height = 8,
                                       text_position =  'outside-base',
                                       number_fmt = scales::comma_format(accuracy = 0.1),
                                       fill_color_ref = "color",
                                       box_shadow = TRUE,
                                       round_edges = TRUE, 
                                       background = "transparent")
                    ),
                    Change = colDef(
                      style = function(value,index) {
                        if(value <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    ),
                    `Change %` = colDef(
                      style = function(value,index) {
                        if(dt$Change[index] <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                  )
  )
  return(tb)
  
})

output$currences_table <- renderReactable({
  req(currences_table_dt())
  currences_table_dt()
})

output$dl_currences_table_ui <- renderUI({
  dlUI("dl_currences_table")
})


observeEvent(currences_table_dt(),{
  callModule(dlServer, "dl_currences_table", file=currences_table_dt ,data = NULL, zip = FALSE)
})

#====================================================

sector_table_dt <- reactive({
  dt <- sector(url = "https://finance.yahoo.com/trending-tickers")
  dt$Weight <- as.numeric(gsub("%","",dt$Weight))
  dt$Return<- as.numeric(gsub("%","",dt$Return))
  dt$Sector[-1] <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;",dt$Sector[-1])
  tb <- reactable(dt,
                  searchable = FALSE,
                  fullWidth = TRUE,
                  
                  defaultPageSize = nrow(dt),
                  defaultColDef = colDef(
                    headerStyle = list(background = '#FFFFFF', color = '#012c70', align= "left")
                  ),
                  theme = reactableTheme(
                    borderColor = "#dfe2e5",
                    stripedColor = "#f6f8fa",
                    highlightColor = "#f0f5f9",
                    cellPadding = "7px 10px 7px 30px", #top, right, bottom, left
                    style = list(
                      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                    ),
                    searchInputStyle = list(width = "100%")),
                  columns = list(
                    color = colDef(show = FALSE),
                    Sector= colDef(
                      minWidth = 250,
                      style = list(color = "#012c70",fontSize = 15),
                      html = TRUE
                    ),
                    Weight = colDef(
                      "Market Weight",
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13),
                      cell = data_bars(dt,
                                       bar_height = 8,
                                       text_position =  'outside-base',
                                       number_fmt = scales::comma_format(accuracy = 0.1,suffix = '%'),
                                       fill_color_ref = "color",
                                       box_shadow = TRUE,
                                       round_edges = TRUE, 
                                       background = "transparent")
                    ),
                    Return = colDef(
                      "YTD Return",
                      minWidth = 150,
                      format = colFormat( suffix = '%', digits = 1),
                      style = function(value,index) {
                        if(value <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                  )
  )
  return(tb)
})

output$sector_table <- renderReactable({
  req(sector_table_dt())
  sector_table_dt()
})

#=============================================

sector_plot_dt <- reactive({
  dt <- sector(url = "https://finance.yahoo.com/trending-tickers")
  dt <- dt[-1,]
  dt$Weight <- as.numeric(gsub("%","",dt$Weight))
  dt$parent <- ""
  dt$label <- paste0(dt$Sector, "<br> Market Weight: ",
                     dt$Weight, "%", "<br>YTD Return: ", dt$Return)
  colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(14)
  colr <- rev(colors[1:length(unique(dt$Sector))])
  red <- dt[as.numeric(gsub("%","",dt$Return)) <0, ]
  
  colors = setNames(colr, unique(dt$Sector))
  colors[names(colors) %in% red$Sector] <- "#c81c43"
  
  p <- plotly::plot_ly(type = "treemap",
                       data = dt,
                       labels = ~label,
                       parents = ~parent,
                       ids = ~Sector,
                       values = ~Weight,
                       hoverinfo = "text",
                       hovertemplate = "<b>%{label}</b><br><extra></extra>",
                       textposition = "inside",
                       insidetextanchor = "middle",
                       marker = list(colors = colors),
                       textfont = list(size = 15)) %>%
    plotly::layout(
      uniformtext = list(minsize = 10),
      showlegend = TRUE,
      paper_bgcolor='rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)',
      margin = list(l = 0, r = 0, b = 0, t = 0)
    ) %>% plotly::config(displayModeBar = FALSE)
  
  return(p)
})

output$sector_plot <- renderPlotly({
  req(sector_plot_dt())
  sector_plot_dt()
})

output$dl_sector_plot_ui <- renderUI({
  dlUI("dl_sector_plot")
})

observeEvent(sector_plot_dt(),{
  callModule(dlServer, "dl_sector_plot", file=sector_plot_dt ,data = NULL, zip = FALSE)
})

#========================================



