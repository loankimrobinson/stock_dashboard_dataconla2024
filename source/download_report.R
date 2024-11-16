data_dl <- reactive({
  req(values$stocks)
  values$stocks
})



observeEvent(input$dl_report,{
  print("==========Download Report==================")
  
  req(sector_table_dt(), sector_plot_dt(), sector_table_dt(), values$title_line_out, values$plot_out_line)
  
  #========= Pop Up Thank you====================
  showModal(div(
    #id = "thankyou",
    modalDialog(
      title = NULL,
      tagList(
        tags$fieldset(tags$legend("Download the Stock Market Report"),
                      div(style = "padding-left:30px;padding-right:30px;color:drakgray;",
                          uiOutput("dl_inputs"),
                          br(),
                          br()
                      ))
      ),
      easyClose = TRUE,
      footer = NULL
    )
  ))
  
  
  
  output$dl_inputs <- renderUI({
    fluidRow(column(width = 12,
                    textInput("report_title", "Report Title", value =  "This is Report Title", width = "98%"),
                    textInput("sub_title", "Sub Title", value =  "This is Report Sub Title", width = "98%"),
                    textInput("author", "Author's Name", value =  "This is Author's Name", width = "98%"),
                    textInput("zip_file_name", "Zip File Name", value = paste0("Stock Market Report ", Sys.Date()), width = "98%"),
                    textAreaInput("abstract", "Information", value = "Add the Information", width = "98%", height = "150px"),
                    br(),
                    downloadButton("submit_dl_report", "Download", icon= icon("download"), style = "float:left;display:inline-block;width:44%;margin-left:2px;color:white!important;background-color:#003b9a;border-color:#003b9a;"),
                    
                    actionButton("cancel_dl", "Cancel",icon= icon("cancel"), style = "float:right;display:inline-block;width:44%;margin-left:2px;color:white!important;background-color:#003b9a;border-color:#003b9a;"),
                    br(),
                    br()
    ))
  })
  
  observeEvent(input$cancel_dl,{
    removeModal() 
  })
  
  
  rv <- reactiveValues(download_flag_dl_demographic = 0)
  
  output$submit_dl_report <- downloadHandler(
    
    filename = function() {
      paste0(input$zip_file_name, ".zip", sep="")
    },
    content = function(file){
      
      start_time <- Sys.time()
      rv$download_flag_dl_demographic <- rv$download_flag_dl_demographic + 1
      
      if(rv$download_flag_dl_demographic > 0){  # trigger event whenever the value of rv$download_flag changes
        showModal(
          tags$div(id = "thankyou",
                   modalDialog(
                     title = NULL,
                     tagList(
                       h3("Thank you"),
                       icon("download", class = "fa-5x", style = "color: #f499cd;"),
                       br(),
                       h4("The report starts to download")
                     ),
                     easyClose = TRUE,
                     footer = NULL
                   )
          ))
      }
      
      Sys.sleep(1)
      currentTime <- Sys.time()
      dt = as.numeric(currentTime - start_time)
   
      
      
      if(dt >= 1){
        removeModal()
      }
      
      withProgress(message = "Writing ", value = 0, {
        
        n = 2
        
        incProgress(1/n, message = paste0("Writing Files to Disk. Please hanging there..."))
        
        
        tempReport <- file.path(tempdir(), "Stock_Market_Dashboard.Rmd")
        templogo <- file.path(tempdir(), "logo-horizontal.svg")
        tempafter <- file.path(tempdir(), "after.html")
        
        file.copy("report/Stock_Market_Dashboard.Rmd", tempReport, overwrite = TRUE)
        file.copy("www/logo-horizontal.svg", templogo, overwrite = TRUE)
        file.copy("report/after.html", tempafter, overwrite = TRUE)
        
        file_html_name <- paste0(input$zip_file_name,".html")

          params <- list(report_title = input$report_title,
                         sub_title = input$sub_title,
                         author = input$author,
                         abstract = input$abstract,
                         plot_out_line = values$plot_out_line,
                         title_line_out = values$title_line_out,
                         sector_table_dt = sector_table_dt(),
                         sector_plot_dt = sector_plot_dt(),
                         trending_table_dt = trending_table_dt()
                         
          )

        file_html <-rmarkdown::render(tempReport,
                                      output_file = file_html_name,
                                      params = params,
                                      envir = new.env(parent = globalenv()),
                                      clean = TRUE)
        
        
        
        incProgress(1/n, message = paste0("Writing Stock Data ..."))
        
        
        # list_data <- list(enrollment_periods()[[3]], cohort_details_gender_bar_dt(),cohort_details_index_year_dt(), geography()[[3]], insurance_group()[[3]])
        # list_data_name <- list("enrollment_periods_table", "cohort_details_gender_bar_table", "cohort_details_index_year_table", "geography","insurance")
        # 
        # file_csv_list <- list()
        # file_csv_name_list <- list()
        # 
        # for(i in 1:length(list_data)){
        #   file_csv_name_list[[i]] <- paste0(list_data_name[[i]],"_",Sys.Date(),".csv")
        #   file_csv_list[[i]] <- file.path(tempdir(),file_csv_name_list[[i]])
        #   write.csv(list_data[[i]], file = file_csv_list[[i]], row.names = F)
        # }
        
        file_csv_name <- paste0("Stock_Market_Data_",Sys.Date(),".csv")
        file_csv <- file.path(tempdir(),file_csv_name)
        write.csv(data_dl(),file_csv, row.names = FALSE)
        
        
        zip(zipfile = file, files = c(file_html, file_csv),flags = "-r9Xj") # remove zip in download
        Sys.sleep(0.1)
      }, style="old")
    }
  )
  
  
  
})









# rv <- reactiveValues(download_flag = 0)
# 
# output$dl_report <- downloadHandler(
#   
#   filename = function() {
#       filename = paste0("Stock_Market_",Sys.Date(), ".html", sep="")
#   },
#   content = function(file){
#     
#     start_time <- Sys.time()
#     rv$download_flag <- rv$download_flag + 1
#     
#     if(rv$download_flag > 0){  # trigger event whenever the value of rv$download_flag changes
#       showModal(
#         tags$div(id = "thankyou",
#                  modalDialog(
#                    title = NULL,
#                    tagList(
#                      h3("Thank you"),
#                      icon("download", class = "fa-5x", style = "color: #f499cd;"),
#                      br(),
#                      h4("Your figures start to download")
#                    ),
#                    easyClose = TRUE,
#                    footer = NULL
#                  )
#         ))
#     }
#     
#     Sys.sleep(1)
#     currentTime <- Sys.time()
#     dt = as.numeric(currentTime - start_time)
#     if(dt > 1){
#       removeModal()
#     }
#     withProgress(message = "Writing ", value = 0, {
#         n = 2
#         incProgress(1/n, message = paste0("Writing the Figures/Tables ..."))
#         
#         file_html_name <- paste0("Stock_Market_",Sys.Date(),".html")
#         file_html <- file.path(tempdir(),file_html_name)
#         htmlwidgets::saveWidget(file(), file = file_html)
#         
#         Sys.sleep(0.1)
#         incProgress(1/n, message = paste0("Writing the Data ..."))
#         
#         file_csv_name <- paste0("Stock_Market_Data_",Sys.Date(),".csv")
#         file_csv <- file.path(tempdir(),file_csv_name)
#         write.csv(data(),file_csv, row.names = FALSE)
#         
#         
#         zip(zipfile = file, files = c(file_html, file_csv),flags = "-r9Xj") # remove zip in download
#         Sys.sleep(0.1)
#     })
#   }
# )
