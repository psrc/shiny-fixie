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
    ## button to add new trip
    modal_new_trip_server("button_new",       selected_row = reactive(selected_recid()))
    ## button to edit trip
    modal_edit_trip_server("button_edit",     selected_row = reactive(selected_recid()))
    ## button to delete trip
    modal_delete_trip_server("button_delete", selected_row = reactive(selected_recid()))
    ## trip linking interface
    modal_trip_linking_server("button_link", selected_row = reactive(selected_recid()))
    
    # platform layout ----
    output$editplatform <- renderUI({
      tagList(
        # person panel
        fluidRow(column(12, person_panel_ui(ns("panel-person")))),
        br(),
        # trip editing panel
        fluidRow(column(6, wellPanel(style ='padding-left:25px; padding-right:25px;',
                                     modal_new_trip_ui(ns('button_new')),
                                     modal_edit_trip_ui(ns('button_edit')),
                                     modal_delete_trip_ui(ns('button_delete'))
                                     )
                        ),
                 column(6, wellPanel(style ='padding-left:25px; padding-right:25px;',
                                     modal_trip_linking_ui(ns('button_link'))
                                     )
                        )
                 ),
        br(),
        # person trip table
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
        )
      }) 
   
    }) 
}

