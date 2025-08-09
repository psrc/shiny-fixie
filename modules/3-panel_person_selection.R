person_panel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("personpanel"))
  )
}

# person control panel
person_panel_server <- function(id, selected_error_type) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    person_list <- reactive(get_error_flag_person_list(selected_error_type()))
    all_person_data <- reactive(get_data(view_name = "person_all", person_id = person_list()))
    
    # show table: basic summary of selected person
    output$persontable <- DT::renderDT(
      
      all_person_data() %>% 
        filter(person_id == input$personID), 
      
      rownames = FALSE,
      selection = 'none',
      options = list(ordering = F, 
                     dom = 't')
      )
    
    output$personpanel <- renderUI({
      
      tagList(
        wellPanel(
          style ='padding-left:25px; padding-right:25px;',
          
          fluidRow(
            column(6, selectInput(inputId = ns("personID"),
                                  label = "Select Person:",
                                  choices = person_list(),
                                  selected = person_list()[1])
                   )
            ), # end fluidRow
          
          fluidRow(
            column(12, DT::DTOutput(ns("persontable")) )
            ) # end fluidRow
          
          ) # end wellPanel
      ) # end tagList
      
    }) 
    
    return(reactive({input$personID}))
  })  # end moduleServer
}