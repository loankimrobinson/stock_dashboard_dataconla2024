server <- function(input, output, session){
  
  source(file.path("source", "inputs.R"), local = TRUE)$value
  source(file.path("source", "all_line_plot.R"), local = TRUE)$value
  source(file.path("source", "explore_stickers.R"), local = TRUE)$value
  
  
  
  source(file.path("source", "trending.R"), local = TRUE)$value
  source(file.path("source", "gainers.R"), local = TRUE)$value
  source(file.path("source", "losers.R"), local = TRUE)$value
  source(file.path("source", "crypto.R"), local = TRUE)$value
  source(file.path("source", "explore_trending_stickers.R"), local = TRUE)$value

  source(file.path("source", "dashboard_table.R"), local = TRUE)$value


  source(file.path("source", "download_md.R"), local = TRUE)$value
  source(file.path("source", "download_report.R"), local = TRUE)$value
}
