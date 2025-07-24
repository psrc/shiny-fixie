person_panel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("personpanel"))
  )
}

# person control panel
person_panel_server <- function(id, view_name) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(error_type = NULL, all_person_data = NULL, person_list = NULL)
    observe({
      rval$error_type <- view_name()
      rval$all_person_data <- get_data(view_name=rval$error_type) # TODO: check back to make sure that this can be rerun when error is fixed
      rval$person_list <- unique(rval$all_person_data[["personid"]])
    })
    
    
    
    # show table: basic summary of selected person
    output$persontable <- DT::renderDT(
      rval$all_person_data %>% filter(personid == input$personID), 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single'))
    
    output$personpanel <- renderUI({
      tagList(
        wellPanel(style ='padding-left:25px; padding-right:25px;',
                  fluidRow(column(6, 
                                  selectInput( inputId = ns("personID"),  
                                               label="Select Person:",  
                                               choices=rval$person_list,  
                                               selected = rval$person_list[1] )
                                  ) # end column
                           ), # end fluidRow
                  fluidRow(column(12, 
                                  DT::DTOutput(ns("persontable"))
                                  ) # end column
                           ) # end fluidRow
                  ) # end wellPanel
      ) # end tagList
    }) 
    return(reactive({input$personID}))
  })  # end moduleServer
}