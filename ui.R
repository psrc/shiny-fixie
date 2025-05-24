shinyUI(

  fluidPage(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "AppID", # Shiny App ID
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = "Shiny-Fixie", # Internal Shiny title
    
    theme = "styles.css", # Loads in the custom CSS
    
    # This section adds the PSRC logo on the top left of the page and the Page Title
    fluidRow(class = "title-logo-section",
      column(12,
             div(
               div(class = "psrc-logo",
                   tags$a(
                     tags$img(
                       src = 'RegionalGem2016.png',
                       width = "85%"
                       # width = "100%"
                     ), 
                     href="https://www.psrc.org", target="_blank"
                   )
               ),
               
               div(class = "mainpage_title", 
                   "Shiny-Fixie"),
               
               style = "display: flex;"
             ) # end div
      ) # end column
    ),
    
    fluidRow(column(12, style='padding-left:25px; padding-right:50px;',
                    tabsetPanel(type = "pills",
                                tabPanel("overview", overview_ui('tab_overview')),
                                tabPanel("all persons", edit_interface_ui('tab_error1')),
                                tabPanel("all errors", edit_interface_ui('tab_error2')),
                                tabPanel("too long at dest?", edit_interface_ui('tab_error3')),
                                tabPanel("mode_1 missing", edit_interface_ui('tab_error4')))
                    )),
    
    # tags$footer(footer_ui('psrcfooter'))
  
    ) # End of fluid page
) # end of shiny app
