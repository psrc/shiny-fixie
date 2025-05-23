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
    
    observeEvent(input$edit_button, {
      # display modal if 0 records were selected
      
      if(is.null(input$thetable_rows_selected)) {
        showModal(
          modalDialog(
            title = "0 records have been selected",
            easy_close = FALSE,
            "Please select a record from the table below to continue."
          )
        )
      }
    })
    
    # data cleaning tools ----
    
    selected_row_recid <- eventReactive(input$edit_button, {
      edit_dt()[input$thetable_rows_selected, "recid"]
    })
    
    ## button to add new trip
    modal_new_trip_server("button_new",       
                          selected_recid = reactive(selected_row_recid()))

    ## activate Edit Trip modal
    observeEvent(input$edit_button, {
      ## button is separated from the rest of the edit modal 
      ## show modal if no records were selected
      
      if(!is.null(input$thetable_rows_selected))
        
        modal_edit_trip_server("button_edit",
                               selected_recid = reactive(selected_row_recid())
        )
    })
    
    
    ## button to delete trip
    modal_delete_trip_server("button_delete", 
                             selected_recid = reactive(selected_row_recid()))
    ## trip linking interface
    modal_trip_linking_server("button_link",  
                              selected_recid = reactive(selected_row_recid()))
    
    # platform layout ----
    
    output$editplatform <- renderUI({
      tagList(
        fluidRow(class = "page-format",
          
          # person panel
          column(8, person_panel_ui(ns("panel-person"))),
          
          # trip editing panel
          column(4,
                 fluidRow(
                   wellPanel(
                     p("Select one trip in trip table below to edit"),
                     div(class = "trip-buttons-panel",
                         modal_new_trip_ui(ns('button_new')),
                         
                         div(actionButton(ns('edit_button'),
                                          label = "Edit Trip")),
                         
                         modal_delete_trip_ui(ns('button_delete'))
                     ) # end div
                   ), # end wellpanel
                   wellPanel(
                     p("Select multiple consecutive trips in trip table below to link"),
                     modal_trip_linking_ui(ns('button_link'))
                   ) # end wellpanel
                 ) # end fluidRow  
          ), # end column
          
        ), # end fluidrow
        
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
      
      ) # end taglist
    }) 
    
  }) 
}

