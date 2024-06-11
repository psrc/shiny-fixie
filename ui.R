shinyUI(

  fluidPage(
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
    
    id = "AppID", # Shiny App ID
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = "My Shiny App", # Internal Shiny title
    
    theme = "styles.css", # Loads in the custom CSS
    
    # This section adds the PSRC logo on the top left of the page and the Page Title
    fluidRow(column(2, tags$a(div(tags$img(src='psrc-logo.png',
                                           # style="margin-top: 10px;",
                                           height = "60")
                                  ), href="https://www.psrc.org", target="_blank")),
             column(10, br(), strong(tags$div(class="mainpage_title", "Fixie Shiny")))),
    
    hr(style = "border-top: 1px solid #000000;"),
    
    fluidRow(column(12, style='padding-left:25px; padding-right:50px;',
                    tabsetPanel(type = "pills",
                                tabPanel("Overview", transit_overview_ui('OVERVIEWtransit')),
                                
                                ## Option B: "dtedit_ui" is already a module, straight from DTedit package ----
                                tabPanel("Mode Error", dtedit_ui('dtedit_example')),
                                
                                ## Option A: the module below is how to place dtedit inside a module ----
                                tabPanel("Mode Error 2", transit_region_ui('REGIONtransit')),
                                
                                tabPanel("PUDO, no +/- traveler", transit_mode_ui('MODEtransit')))
                    )),
    
    tags$footer(footer_ui('psrcfooter'))
  
    ) # End of fluid page
) # end of shiny app
