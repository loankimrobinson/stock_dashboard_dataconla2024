


output$sticker_choices <- renderUI({
  pickerInput(inputId = "stickers",
              label = NULL,
              choices = sticker$symbol,
              multiple = TRUE,
              selected = c("AMZN", "AAPL", "GOOG" ),
              width = "100%",
              options = list(
                title = "Select Stickers",
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `virtual-scroll` = TRUE,
                `multiple-seperator` = "\n",
                showTick = TRUE,
                `live-search-placeholder` = "Searching Stickers ...",
                size = 20
              ),
              choicesOpt = list(
                style = rep(("color: black; background:#f2f2f2; font-style: italic;"),length(sticker$symbol)+1),
                content = sticker$choices))

})

output$date_from <- renderUI({
  dateInput(
    "date_from",
    "Date From:",
    value = Sys.Date() - 720,
    max = Sys.Date() - 360,
    width = "100%"
    
  )
})

output$date_to <- renderUI({
  dateInput(
    "date_to",
    "Date To:",
    value = Sys.Date(),
    min = Sys.Date() - 360,
    width = "100%"
  )
})

output$submit_bt <- renderUI({
  tagList(
    br(),
    actionButton("submit", "Get Symbols", icon("paper-plane"), style = "display: inline-block;align-items:left;width:48%;text-align:center;text-color:white;background-color:#F6F4F3;border-color:#F6F4F3;box-shadow: 1px 2px 2px 0px #003b9a;"),
    downloadButton("dl_report", "Download Report", icon("download"), style = "display: inline-block;align-items:right;width:48%;text-align:center;text-color:white;background-color:#F6F4F3;border-color:#F6F4F3;box-shadow: 1px 2px 2px 0px #003b9a;"),
    br(),
    br()
  )
})


