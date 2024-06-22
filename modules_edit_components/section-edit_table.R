# feed in the data of a person already filtered

edit_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("edittable"))
  )
}

edit_table_server <- function(id, person_id) {
  moduleServer(id, function(input, output, session){
    
    person_data <- reactive({get_data2fixie(person_id())})
    table <- DT::renderDT(
      person_data()[,view.cols], 
      options =list(ordering = F, # disable sorting
                    dom = 't',  # show only the table (no search and no page length dropdown)
                    selection = 'single',
                    rownames = FALSE))
    output$edittable <- renderUI({
      tagList(
        fluidRow(column(12, DT::DTOutput({table()})))
      )
    }) 
  })  # end moduleServer
}