ui <- tagList(
  tags$head(includeCSS(file.path('www', 'style.css'))),
  navbarPage(
    id = "nav",
    selected = "dashboard",
    windowTitle = "Market Dashboard",
    title = div(
      div(
        id = "logo",
        img(src = "logo-horizontal.svg", height = 25),
        align = "left",
        style = "position:fixed; left:10px; top:10px"
      ),
      div("Market Dashboard")
    ),
    tabPanel(
      "Dashboard",
      value = "dashboard",
      div(style = "padding-left:10px; padding-right:10px;",
          fluidRow(column(width = 3,
                          uiOutput("trending_stickers")),
                  column(width = 3,
                          uiOutput("gainers_stickers")),
                  column(width = 3,
                         uiOutput("losers_stickers")),
                  column(width = 3,
                         uiOutput("crypto_stickers"))
                  ),
          br(),
          fluidRow(column(width = 6,uiOutput("plot_exp")),
                   column(width = 6,uiOutput("plot_area_exp"))),
          br(),
          fluidRow(column(width = 7,
                          tags$fieldset(tags$legend("Trending Stickers"),
                          withSpinner(reactableOutput("trending_table"),type=8, size=1, color= "#d91e49"))),
                   column(width = 5,
                          tags$fieldset(tags$legend("Currencies"),
                                        uiOutput("dl_currences_table_ui"),
                                        withSpinner(reactableOutput("currences_table"),type=8, size=1, color= "#d91e49")))
                   ),
          br(),
          fluidRow(
                   column(width = 5,
                          tags$fieldset(tags$legend("Sectors"),
                                        withSpinner(reactableOutput("sector_table"),type=8, size=1, color= "#d91e49"))),
                   column(width = 7,
                          tags$fieldset(tags$legend("Sectors"),
                                        uiOutput("dl_sector_plot_ui"),
                                        withSpinner(plotlyOutput("sector_plot", height = 400),type=8, size=1, color= "#d91e49")))
          ),
          fluidRow( div(style = "padding-left:20px; padding-right:20px;height:50px;",)),
          fluidRow( div(style = "padding-left:20px; padding-right:20px;",source(file.path("source", "footer.R"), local = TRUE)$value))
          )
    ),
    tabPanel(
      "Explore",
      value = "explore",
      div(style = "padding-left:10px; padding-right:10px;padding-top:20px;",
          fluidRow(column(width = 4,
                          tags$fieldset(tags$legend("Stickers and Trading Dates"),
                                        div(id = "sublegend",style = "padding-left:20px; padding-right:20px;height:300px;",
                                        br(),
                                        uiOutput("sticker_choices"),
                                        uiOutput("date_from"),
                                        uiOutput("date_to"),
                                        uiOutput("submit_bt"),
                                        helpText("Using 'Quantmod' Package to pull data from Yahoo"),
                                        br())
                          )),
                   column(width = 8,uiOutput("plot_line"))
          ),
          fluidRow( div(style = "padding-left:20px; padding-right:20px;",uiOutput("boxes")),br()),
          fluidRow(column(width = 6,uiOutput("plot")),
                   column(width = 6,uiOutput("plot_area"))),
          fluidRow( div(style = "padding-left:20px; padding-right:20px;height:50px;",)),
          fluidRow( div(style = "padding-left:20px; padding-right:20px;",source(file.path("source", "footer.R"), local = TRUE)$value))
      )
    )
  )
)
