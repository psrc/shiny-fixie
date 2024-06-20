edit_platform_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editplatform"))
  )
}

edit_platform_server <- function(id, thedata, error_type="too long at dest?") {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    person_list <- reactive({get_person_list()})
    
    # the table ----
    # person data from database
    person_data <- reactive({get_data2fixie(input$personID)})
    output$thetable <- DT::renderDT(
      person_data()[,view.cols], 
      options =list(ordering = F, # disable sorting
                    dom = 't',  # show only the table (no search and no page length dropdown)
                    selection = 'single',
                    rownames = FALSE))
    
    output$editplatform <- renderUI({
      tagList(
        fluidRow(column(3, selectInput( inputId = ns("personID"),  label="Select Person:",  choices=person_list(),  selected = person_list()[1]))                 ),
        fluidRow(column(12, DT::DTOutput(ns("thetable"))))
        )
      }) 
    })  # end moduleServer
}

