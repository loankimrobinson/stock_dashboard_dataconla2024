#==================

output$gainers_stickers <- renderUI({
  
  dt <- gainers()[1,]
  dt$Change <- as.numeric(gsub("+|,","",dt$Change))
  dt$change_out = ifelse(dt$Change >= 0, 
                        paste0("<span style='color:#32907c;'>", "+", sprintf("%.2f",dt$Change), " ( ", dt$`Change %`, " ) &#11014; </span>"),
                        paste0("<span style='color:#990000;'>",sprintf("%.2f",dt$Change), " ( ", dt$`Change %`, ") &#11015;</span>"))
  
  output$gainers_last <- renderText(
    HTML(paste0("<span style='font-size:40px;'>",format(gsub("\\-.*","",(gsub("\\+.*","",dt$Price))),nsmall=2,big.mark=","),"</span><span style='font-size:18px;'> USD</span>"))
  )
  
  output$gainers_vol <- renderText(
    HTML(paste0("<span style='font-size:12px;'>Volume: ",format(dt$Volume, nsmall=0,big.mark=","),"</span><span style='font-size:12px;'> </span>"))
  )
  
  output$gainers_mc <- renderText(
    HTML(paste0("<span style='font-size:12px;'>Market Cap: ",format(dt$`Market Cap`, nsmall=0,big.mark=","),"</span><span style='font-size:12px;'> </span>"))
  )
  
  output$gainers_title <- renderText(
    HTML(paste0("<span style='font-size:20px;color:#d91e49;font-weight:bold;'>",dt$Symbol,"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;",dt$Name,"</span>"))
  )
  
  output$gainers_change <- renderText(
    HTML(dt$change_out)
  )
  
  tags$fieldset(tags$legend("Top Gainer Sticker"),
                div(style = "padding-left:20px; padding-right:20px;",
                    htmlOutput("gainers_title",style = "text-align:center;"),br(),
                    htmlOutput("gainers_last",style = "text-align:center;"),br(),
                    splitLayout(cellWidths = c("70%", "30%"),cellArgs = list(style='min-height: 35px;'),
                                htmlOutput("gainers_change",style = "text-align:left;padding-left:10px;font-size:22px;color:#32907c"),
                                div(style ="text-align:center;hight:35px;",
                                    actionButton("gainers", label = "Explore", class = "css-selector",
                                                 onclick = "Shiny.setInputValue('btnexp', this.id);",  #onclick = "Shiny.setInputValue('btnLabel', this.this.innerText);", to capture label
                                                 style = "text-align:center;text-color:white;background-color:#F6F4F3;border-color:#F6F4F3;box-shadow: 2px 2px 2px 0px #003b9a;"))
                    ),
                    htmlOutput("gainers_mc",style = "text-align:left;padding-left:10px;font-size:15px;color: #003b9a;"),
                    fluidRow(htmlOutput("gainers_vol",style = "text-align:right;padding-right:10px;font-size:15px;")))
  )
  
})