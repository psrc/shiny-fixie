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
    edit_dt <- reactive({
      get_data(person_id = personID())
    })
    
    output$thetable <- DT::renderDataTable(
      edit_dt()[,view.cols],
      options =list(ordering = F, dom = 't'), 
      selection = 'single',
      rownames = FALSE, 
      server=TRUE)
    
    # data cleaning tools ----
    
    selected_recid <- eventReactive(input$edit_button, {
      # browser()
      edit_dt()[input$thetable_rows_selected, "recid"]
    })
    
    observeEvent(input$edit_button, {
      if(is.null(input$thetable_rows_selected)) {
        showModal(
          modalDialog(
            title = "0 records have been select",
            easy_close = FALSE,
            "Please select a record to continue"
          )
        )
      }
    })
   
    
    ## button to add new trip
    modal_new_trip_server("button_new", selected_row = reactive(selected_recid()))
    
    ## activate Edit Trip modal
    observeEvent(input$edit_button, {
      ### separate button from the rest of the edit modal 
      if(!is.null(input$thetable_rows_selected))
      modal_edit_trip_server("button_edit",
                             selected_row = reactive(selected_recid())
      )
    })
   
    ## button to delete trip
    modal_delete_trip_server("button_delete", selected_row = reactive(selected_recid()))
    ## trip linking interface
    modal_trip_linking_server("button_link", selected_row = reactive(selected_recid()))
    
    # platform layout ----
    output$editplatform <- renderUI({
      tagList(
        fluidRow(# person panel
                 column(6, person_panel_ui(ns("panel-person"))),
                 # trip editing panel
                 column(2, wellPanel(style ='padding-left:25px; padding-right:25px;',
                                     "Select one trip in trip table below to edit: ",
                                     modal_new_trip_ui(ns('button_new')),
                                     actionButton(ns('edit_button'),
                                                  label = "Edit Trip"),
                                     # modal_edit_trip_ui(ns('button_edit')),
                                     modal_delete_trip_ui(ns('button_delete'))
                                     )),
                 # person trip table
                 column(4, wellPanel(style ='padding-left:25px; padding-right:25px;',
                                     "Select multiple consecutive trips in trip table below to link: ",
                                     modal_trip_linking_ui(ns('button_link'))
                                     ))
        ),
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
        )
      }) 
   
    }) 
}

