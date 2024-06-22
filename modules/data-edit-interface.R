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
    personID <- person_panel_server("panel-person", edit_persons)
    
    # data cleaning tools ----
    edit_modal_server("button-edit", label_name="Edit trip")
    edit_modal_server("button-new", label_name="Add new trip")
    
    # the trip table ----
    # person data from database
    edit_dt <- reactive({get_data(person_id = personID())})
    
    output$thetable <- DT::renderDT(
      edit_dt()[,view.cols],
      rownames = FALSE, 
      options =list(ordering = F, dom = 't', selection = 'single'))
    
    # the output ----
    output$editplatform <- renderUI({
      tagList(
        fluidRow(column(12, person_panel_ui(ns("panel-person")))),
        fluidRow(column(12, div(edit_modal_ui(ns('button-new')),
                                edit_modal_ui(ns('button-edit'))
                               ))),
        fluidRow(column(12, DT::DTOutput(ns("thetable"))))
        )
      }) 
   
    })  # end moduleServer
}

