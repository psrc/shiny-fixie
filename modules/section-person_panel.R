person_panel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("personpanel"))
  )
}

# person control panel
person_panel_server <- function(id, edit_persons) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    all_person_data <- get_data(view_name=edit_persons) # TODO: check back to make sure that this can be rerun when error is fixed
    person_list <- reactive({ all_person_data %>% select(personid) %>% unique() })
    
    # show table: basic summary of selected person
    output$persontable <- DT::renderDT(
      all_person_data %>% filter(personid == input$personID), 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single'))
    
    output$personpanel <- renderUI({
      tagList(
        wellPanel(style ='padding-left:25px; padding-right:25px;',
                  fluidRow(column(3, selectInput( inputId = ns("personID"),  label="Select Person:",  choices=person_list(),  selected = person_list()[1]))),
                  fluidRow(column(6, DT::DTOutput(ns("persontable")))))
      )
    }) 
    return(reactive({input$personID}))
  })  # end moduleServer
}