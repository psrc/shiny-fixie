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
    personID <- person_panel_server("panel-person", view_name=edit_persons)
    
    # the trip table ----
    # person data from database
    edit_dt <- reactive({get_data(person_id = personID())})
    output$thetable <- DT::renderDataTable(
      edit_dt()[,view.cols],
      options =list(ordering = F, dom = 't'), 
      selection = 'single',
      rownames = FALSE, 
      server=TRUE)
    
    # data cleaning tools ----
    selected_recid <- reactive({ edit_dt()[input$thetable_rows_selected,"recid"] })
    modal_new_trip_server("button-new", label_name="Add new trip", selected_row = selected_recid())
    modal_edit_trip_server("button-edit", label_name="Edit trip", selected_row = selected_recid())
    delete_trip_server("button-delete", label_name="Delete trip", selected_row = selected_recid())
    
    # the output ----
    output$editplatform <- renderUI({
      tagList(
        fluidRow(column(12, person_panel_ui(ns("panel-person")))),
        br(),
        fluidRow(column(12, div(
          modal_new_trip_ui(ns('button-new')),
          modal_edit_trip_ui(ns('button-edit')),
          delete_trip_ui(ns('button-delete'))
                               ))),
        br(),
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
        )
      }) 
   
    })  # end moduleServer
}

