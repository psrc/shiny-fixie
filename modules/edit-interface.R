edit_interface_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editplatform"))
  )
}

#' HTS data editor user interface
edit_interface_server <- function(id, edit_persons) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # person control panel ----
    person_list <- reactive({get_person_list(view_name=edit_persons)})
    person_data <- reactive({get_data(view_name=edit_persons, person_id = input$personID)})
    output$persontable <- DT::renderDT(
      person_data(), 
      rownames = FALSE,
      options =list(ordering = F, # disable sorting
                    dom = 't',  # show only the table (no search and no page length dropdown)
                    selection = 'single',
                    stripe = FALSE))
    
    # the table ----
    # person data from database
    edit_dt <- reactive({get_data(person_id = input$personID)})
    
    output$thetable <- DT::renderDT(
      edit_dt()[,view.cols],
      rownames = FALSE, 
      options =list(ordering = F, # disable sorting
                    dom = 't',  # show only the table (no search and no page length dropdown)
                    selection = 'single'))
    
    # the output ----
    output$editplatform <- renderUI({
      tagList(
        # Center Selection
        fluidRow(
          wellPanel(style ='padding-left:25px; padding-right:25px;',
            fluidRow(column(3, selectInput( inputId = ns("personID"),  label="Select Person:",  choices=person_list(),  selected = person_list()[1]))),
            fluidRow(column(6, DT::DTOutput(ns("persontable")))))
          ),
        # fluidRow(column(3, selectInput( inputId = ns("personID"),  label="Select Person:",  choices=person_list(),  selected = person_list()[1]))),
        # fluidRow(column(6, DT::DTOutput(ns("persontable")))),
        fluidRow(column(3, actionButton(paste0(id, '_edit'), "Edit trip"))),
        fluidRow(column(12, DT::DTOutput(ns("thetable"))))
        )
      }) 
    
    })  # end moduleServer
}

