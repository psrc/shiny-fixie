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
    
    observeEvent(input$previous_person, {
      # update person selection dropdown with previous option
      
      current_index <- which(person_list() == input$personID)
      # find previous person
      new_personID <- person_list()[max(1,current_index-1)]
      
      updateSelectInput(inputId = "personID",
                        label = "Select Person:",
                        choices = person_list(),
                        selected = new_personID)
    })
    
    observeEvent(input$next_person, {
      # update person selection dropdown with next option
      
      current_index <- which(person_list() == input$personID)
      # find next person
      new_personID <- person_list()[min(length(person_list()),current_index+1)]
      
      updateSelectInput(inputId = "personID",
                        label = "Select Person:",
                        choices = person_list(),
                        selected = new_personID)
    })
    
    output$personpanel <- renderUI({
      
      tagList(
        wellPanel(
          style ='padding-left:25px; padding-right:25px;',
          
          fluidRow(
            column(6, selectInput(inputId = ns("personID"),
                                  label = "Select Person:",
                                  choices = person_list(),
                                  selected = person_list()[1])
                   ),
            
            column(6, 
                   style = "display: flex; margin-top: 30px; padding-left:0px;",
                   actionButton(class = "back-forth-buttons",
                                inputId = ns("previous_person"),
                                label = "Previous Person |"),
                   actionButton(class = "back-forth-buttons",
                                inputId = ns("next_person"),
                                label = "Next Person")
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