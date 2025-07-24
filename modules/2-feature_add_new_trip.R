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
    rval <- reactiveValues(recid = NULL, 
                           trip_record = NULL, 
                           trip_summary_table = NULL,
                           poi_latlong = NULL,
                           poi_config = NULL,
                           updated_trip = NULL)
    
    # rval$updated_trip <- rval$trip_record %>%
    #   filter(row_number() != 1)
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      # assign rval ----
      rval$recid <- selected_recid()
      
      # trip data, trip summary and datatable widget
      rval$trip_record <- get_trip_record(rval$recid)
      rval$trip_summary_table <- get_trip_summary(rval$trip_record)
      output$trip_summary <- render_trip_summary(rval$trip_summary_table)
      
      # prep point of interest buttons
      rval$poi_latlong <- prep_poi_buttons(poi_ids, rval$trip_record)
      rval$poi_config <- tibble(inputId = ns(poi_ids), # config for actionButton_google_poi
                                latlong = rval$poi_latlong, 
                                icon = poi_icons)
      # browser()
      
      # if a row is selected in table: show Trip Record Editor
      if(!identical(rval$recid,integer(0))){
        
        showModal(
          modalDialog(title = "Trip Record Generator",
                      
                      # editor top panel: trip summary table and point of interest buttons ----
                      tripeditor_top_panel(ns("trip_summary"), rval$poi_config),
                      
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
      
      showModal(
        modalDialog(title = "Trip Record Generator: Add Return Trip",
                    
                    div("Adding return home trip after this selected trip:"),
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    tripeditor_top_panel(ns("trip_summary"), rval$poi_config),
                    
                    fluidRow(
                      column(12,
                             dateTimeInput(ns("data_edit-arrival_time_timestamp"), df = rval$updated_trip)
                             
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
      
      showModal(
        modalDialog(title = "Trip Record Generator: Add Reverse Trip",
                    
                    div("Adding reverse trip after this selected trip:"),
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    tripeditor_top_panel(ns("trip_summary"), rval$poi_config),
                    
                    fluidRow(
                      column(12,
                             dateTimeInput(ns("data_edit-arrival_time_timestamp"), df = rval$updated_trip)
                             
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