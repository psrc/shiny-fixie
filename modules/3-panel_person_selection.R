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
    
    # compact summary of selected person
    output$person_summary <- renderUI({
      df <- all_person_data()
      req(df, input$personID)
      row <- df %>% filter(person_id == input$personID) %>% slice(1)
      if (nrow(row) == 0) return(NULL)
      # Expect columns: person_id, Age, Works, Studies, HHGroup (fallbacks shown as '-')
      getv <- function(x) if (x %in% names(row)) row[[x]] else "-"
      span_list <- list(
        tags$span(style = "font-weight:600;", paste0("Person ", getv("person_id"))),
        tags$span(" · Age ", getv("Age")),
        tags$span(" · Works ", getv("Works")),
        tags$span(" · Studies ", getv("Studies")),
        tags$span(" · HHGroup ", getv("HHGroup"))
      )
      div(class = "person-summary-line",
          style = "margin-top:6px; color:#333; display:flex; gap:8px; flex-wrap:wrap;",
          span_list)
    })
    
    output$personpanel <- renderUI({
      
      tagList(
        wellPanel(
          style ='padding-left:16px; padding-right:16px; padding-top:12px; padding-bottom:8px;',
          fluidRow(
            column(12, selectInput(inputId = ns("personID"),
                                   label = "Select Person:",
                                   choices = person_list(),
                                   selected = person_list()[1]))
          ),
          uiOutput(ns("person_summary"))
        ) # end wellPanel
      ) # end tagList
      
    }) 
    
    return(reactive({input$personID}))
  })  # end moduleServer
}