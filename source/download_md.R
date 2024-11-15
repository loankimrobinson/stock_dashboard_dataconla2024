dlUI <- function(id, download_text = "Download the Figures", email_text = "Email the Figures",class = "legend_download") {
  ns <- NS(id)
  div(class = class,
      tags$legend(
        div(id = ns("fav_md"),style = "display:inline-block;vertical-align:center; horizontal-align:center",
            actionButton(ns("fav"),label = NULL, icon = icon("heart"), style = "box-shadow: 3px 3px 2px 1px rgba(0,0,0,0.1);width:30px;height:30px;background-color:#fac8d3;border-color:#fac8d3;color:#c81c43!important;")),
        bsTooltip(id = ns("fav_md"),"Add the figure to your favorite list"),
        
        
        div(id = ns("download_md"),style = "display:inline-block;vertical-align:center; horizontal-align:center",
            downloadButton(ns("download"),label = NULL, icon = icon("download"), style = "box-shadow: 3px 3px 2px 1px rgba(0,0,0,0.1);width:30px;height:30px;background-color:#f499cd;border-color:#f499cd;color:white!important;")),
        bsTooltip(id = ns("download_md"),download_text),
        
        
        div(id = ns("email_md"),style = "display:inline-block;vertical-align:center; horizontal-align:center",
            actionButton(ns("email"),label = NULL, icon = icon("envelope"), style = "box-shadow: 3px 3px 2px 1px rgba(0,0,0,0.1);width:30px;height:30px;background-color:#fac8d3;border-color:#fac8d3;color:#c81c43!important;")),
        bsTooltip(id = ns("email_md"),email_text)
      ))
  
}


dlServer <- function(input, output, session, file ,data = NULL, zip = FALSE){
  
  rv <- reactiveValues(download_flag = 0)

  ns <- session$ns
  
  GetUser <-reactive({
    user <<- session$user %>%
      ifelse(is_empty(.),
             yes = Sys.getenv("USER"),
             no = .)
    return(user)
  })
  
  observe({
    print(GetUser())
  })
  
  observeEvent(data(),{
    print(data())
    print(file())
  })
  
  x <- session$ns('tmp')  # make an ID string
  mod_id <- substr(x, 1, nchar(x)-4)  # remove last 3 characters, ie "-tmp"
  
  
  output$download <- downloadHandler(
    
    filename = function() {
      if(isTRUE(zip)){
        filename =paste0(mod_id,"_",Sys.Date(), ".zip", sep="")
      }else{
        filename = paste0(mod_id,"_",Sys.Date(), ".html", sep="")
      }
      filename
    },
    content = function(file){
      
      start_time <- Sys.time()
      rv$download_flag <- rv$download_flag + 1
      
      if(rv$download_flag > 0){  # trigger event whenever the value of rv$download_flag changes
        showModal(
          tags$div(id = "thankyou",
                   modalDialog(
                     title = NULL,
                     tagList(
                       h3("Thank you"),
                       icon("download", class = "fa-5x", style = "color: #f499cd;"),
                       #icon("envelope-circle-check", class = "fa-5x", style = "color: #f499cd;"),
                       br(),
                       h4("Your figures start to download")
                     ),
                     easyClose = TRUE,
                     footer = NULL
                   )
          ))
      }
      
      Sys.sleep(1)
      currentTime <- Sys.time()
      dt = as.numeric(currentTime - start_time)
      if(dt > 1){
        removeModal()
      }
      withProgress(message = "Writing ", value = 0, {
        
        if(isTRUE(zip)){
          n = 2
          incProgress(1/n, message = paste0("Writing the Figures ..."))
          
          file_html_name <- paste0(mod_id,"_",Sys.Date(),".html")
          file_html <- file.path(tempdir(),file_html_name)
          htmlwidgets::saveWidget(file(), file = file_html)
          
          Sys.sleep(0.1)
          incProgress(1/n, message = paste0("Writing the Data ..."))
          
          file_csv_name <- paste0(mod_id,"_",Sys.Date(),".csv")
          file_csv <- file.path(tempdir(),file_csv_name)
          write.csv(data(),file_csv, row.names = FALSE)
          
          
          zip(zipfile = file, files = c(file_html, file_csv),flags = "-r9Xj") # remove zip in download
          Sys.sleep(0.1)
        }else{
          n = 1
          incProgress(1/n, message = paste0("Writing the Figures ..."))
          htmlwidgets::saveWidget(file(), file = file)
          Sys.sleep(0.1)
        }
      })
    }
  )
  
  #==============================================================
  observeEvent(input$email,{
    start_time <- Sys.time()
    showModal(
      tags$div(id = "thankyou",
               modalDialog(
                 title = NULL,
                 tagList(
                   h3("Thank you"),
                   # icon("download", class = "fa-5x", style = "color: #f499cd;"),
                   icon("envelope-circle-check", class = "fa-5x", style = "color: #f499cd;"),
                   br(),
                   h4("You will receive the email shortly. ")
                 ),
                 easyClose = TRUE,
                 footer = NULL
               )
      ))
    
    Sys.sleep(1)
    currentTime <- Sys.time()
    dt = as.numeric(currentTime - start_time)
    print(dt)
    if(dt > 1){
      removeModal()
    }
    
    withProgress(message = "Writing ", value = 0, {
      
      if(isTRUE(zip)){
        
        n = 2
        incProgress(1/n, message = paste0("Writing the Figures ..."))
        
        file_html_name <- paste0(mod_id,"_",Sys.Date(),".html")
        file_html <- file.path(tempdir(),file_html_name)
        htmlwidgets::saveWidget(file(), file = file_html)
        
        Sys.sleep(0.1)
        incProgress(1/n, message = paste0("Writing the Data ..."))
        
        file_csv_name <- paste0(mod_id,"_",Sys.Date(),".csv")
        file_csv <- file.path(tempdir(),file_csv_name)
        write.csv(data(),file_csv, row.names = FALSE)
        
        attach_file <- file.path(tempdir(),paste0(mod_id,"_",Sys.Date(), ".zip", sep=""))
        zip(zipfile =  attach_file , files = c(file_html, file_csv),flags = "-r9Xj") # remove zip in download
        Sys.sleep(0.1)
      }else{
        n = 1
        incProgress(1/n, message = paste0("Writing the Figures ..."))
        
        file_html_name <- paste0(mod_id,"_",Sys.Date(),".html")
        attach_file <- file.path(tempdir(),file_html_name)
        htmlwidgets::saveWidget(file(), file =  attach_file)
        
        Sys.sleep(0.1)
      }
    })
    
    title_email <- "Outputs from Market Dashboard"
    email_address <- c(paste0(GetUser(), "@astellas.com"))
    # content email
    content <- paste(
      "Market Dashboard Outputs", "\n", "\n",
      paste("The following outputs were sent from the Market Dashboard Application", ""), "\n",
      "Please review the details below:\n",
      "- Requested By:", GetUser(), "\n",
      paste0("- Analysis Date: ",Sys.Date()),"\n",
      paste0("Thank you")
      ,sep=" "
    )
    
    file.create(paste0(tempdir(),"/content.txt"))
    write(content, file = paste0(tempdir(),"/content.txt"))
    
    title <- paste0("\"",title_email,"\"")
    emaillist <-  paste0(unlist(strsplit(gsub("\\s", "", email_address)  , ",")), collapse = ", ")
    
    out <- paste0("mailx -S smtp=usva-prd-mr-01.astellasrwi.us:25", " -a ", attach_file , " -s ",title, " ",  emaillist , " < ",paste0(tempdir(), "/content.txt"))
    
    system(out)
    file.remove(paste0(tempdir(),"/content.txt"))
    file.remove(attach_file)
    
  })
  
}
