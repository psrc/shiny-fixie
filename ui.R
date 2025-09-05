shinyUI(
  
  navbarPage(
    id = "shiny-fixie",
    title = div(tags$img(src='RegionalGem2016.png',
                                style="margin-top: -4px; padding-right: 20px;",
                                height = "30"),
                div("Shiny-Fixie", style=" font-size: 2rem; font-weight: 700;"),
                style = "display: flex; "
    ),
    windowTitle = "Shiny-Fixie", 
    theme = "styles.css",
    position = "fixed-top",
    
    tabPanel(title=HTML("Overview"),
             overview_ui('tab_overview')
    ), # end tabPanel
    
    tabPanel(title=HTML("Trip Editor - All Users"),
             div(error_dropdown_ui('dropdown_errortype'),
                 style = "display: flex; align-items: flex-end;"),
             
             fluidRow(column(12, edit_interface_ui('main_page') ))
    ), # end tabPanel
    
    tabPanel(title=HTML("Joanne"),
             div(error_dropdown_ui('dropdown_errortype-J'),
                 style = "display: flex; align-items: flex-end;"),
             
             fluidRow(column(12, edit_interface_ui('main_page') ))
    ), # end tabPanel
    
    tabPanel(title=HTML("Mike"),
             div(error_dropdown_ui('dropdown_errortype-Mi'),
                 style = "display: flex; align-items: flex-end;"),
             
             fluidRow(column(12, edit_interface_ui('main_page') ))
    ), # end tabPanel
    
    tabPanel(title=HTML("Mohammad"),
             div(error_dropdown_ui('dropdown_errortype-Mo'),
                 style = "display: flex; align-items: flex-end;"),
             
             fluidRow(column(12, edit_interface_ui('main_page') ))
    ), # end tabPanel
    
  ) # End of navbarPage
  
) # End of shinyUI