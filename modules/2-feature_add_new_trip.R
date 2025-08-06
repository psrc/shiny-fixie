# NOTES: if no trip selected, insert new trip at very beginning of trip list

modal_new_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_new_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # create objects ----
    # values that share across multiple observeEvents
    rval <- reactiveValues(updated_trip = NULL)
    trip_record <- reactive(get_trip_record(selected_recid()))
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      # if a row is selected in table: show Trip Record Editor
      if(!identical(selected_recid(),integer(0))){
        
        rval$updated_trip <- trip_record() %>%
          filter(row_number() != 1)
        
        # create trip summary panel ----
        trip_summary_panel_server("trip_summary_panel", trip_record(), incl_poi = TRUE)
        
        showModal(
          modalDialog(title = "Trip Record Generator",
                      
                      # editor top panel: trip summary table and point of interest buttons ----
                      trip_summary_panel_ui(ns("trip_summary_panel")),
                      
                      footer = column(12,
                                      class = "trip-buttons-panel",
                                      modalButton('Insert trip before selected trip'),
                                      modalButton('Insert trip after selected trip'),
                                      actionButton(ns("clickreversetrip"), "Add reverse trip"),
                                      actionButton(ns("clickreturnhome"), "Add return home trip"),
                                      modalButton('Cancel')
                      ),
                      size = "l"
          )
        )
      }
      # if no row is selected
      else{
        notification_warning_select_row()
      }
      
      }) # end observeEvent
    
    
    # return home ----
    
    observeEvent(input$clickreturnhome, { 
      
      # create trip summary panel ----
      trip_summary_panel_server("trip_summary_panel", trip_record(), incl_poi = TRUE)
      
      showModal(
        modalDialog(title = "Trip Record Generator: Add Return Trip",
                    
                    div("Adding return home trip after this selected trip:"),
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    trip_summary_panel_ui(ns("trip_summary_panel")),
                    
                    fluidRow(
                      column(12,
                             dateTimeInput(ns("return_home-depart_timestamp"), 
                                           label_name = "Enter the departure date and time for return home trip:",
                                           # show arrival time of previous trip as placeholder
                                           datetime_val = trip_record()[1,'arrival_time_timestamp'])
                             
                      ) # end column
                    ),
                    
                    
                    footer = column(12,
                                    class = "trip-buttons-panel",
                                    modalButton('Preview new trip'),
                                    modalButton('Cancel')
                    ),
                    size = "l"))
      })
    
    
    # reverse trip ----
    
    observeEvent(input$clickreversetrip, { 
      
      # create trip summary panel ----
      trip_summary_panel_server("trip_summary_panel", trip_record(), incl_poi = TRUE)
      
      showModal(
        modalDialog(title = "Trip Record Generator: Add Reverse Trip",
                    
                    div("Adding reverse trip after this selected trip:"),
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    trip_summary_panel_ui(ns("trip_summary_panel")),
                    
                    fluidRow(
                      column(12,
                             dateTimeInput(ns("reverse_trip-depart_timestamp"), 
                                           label_name = "Enter the departure date and time for reverse trip:",
                                           # show arrival time of previous trip as placeholder
                                           datetime_val = trip_record()[1,'arrival_time_timestamp'])
                             
                      ) # end column
                    ),
                    
                    footer = column(12,
                                    class = "trip-buttons-panel",
                                    modalButton('Preview new trip'),
                                    modalButton('Cancel')
                    ),
                    size = "l"))
    })

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "Add new trip") })
    
  })  # end moduleServer
}