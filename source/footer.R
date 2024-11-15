footer <- tagList(
  fluidRow(column(12,offset = 0, div(style = "text-align: center;position:relative;","")),
           tags$span(style="color:grey", 
                     tags$div(tags$footer("Using R Studio and Shiny Application."),
                              tags$footer(("May 2nd, 2024"),
                                          tags$a(
                                            href="https://mail.google.com/mail/?view=cm&fs=1&to=loankimrobinson@gmail.com",
                                            target="_blank",
                                            "Loan Robinson."))),
                     align="center")),
  br(),
  fluidRow(column(12,div(style = "text-align: center", width="100%",
                         p(HTML('<a target="_blank" title="find us on Facebook" href="http://www.facebook.com"><img alt="follow me on facebook" src="//login.create.net/images/icons/user/facebook_30x30.png" border=0></a>
                                                      <a target="_blank" title="follow me on twitter" href="http://www.twitter.com"><img alt="follow me on twitter" src="//login.create.net/images/icons/user/twitter-b_30x30.png" border=0></a>
                                                      <a target="_blank" title="follow me on youtube" href="http://www.youtube.com"><img alt="follow me on youtube" src="youtube.png" border=0 width = 30 hight=30 ></a>'))))),
  br()
)