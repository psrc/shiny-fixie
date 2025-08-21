# NOTES: if no trip selected, insert new trip at very beginning of trip list

modal_new_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_new_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(blank_trip_record = NULL,
                           insert_trip = NULL)
    
    # create objects ----
    trip_record <- reactive(get_trip_record(selected_recid()))
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      # if a row is selected in table: show Trip Record Editor
      if(length(selected_recid())==1){
        
        # trip summary panel
        trip_summary_panel_server("trip_summary_panel", selected_recid(), incl_poi = TRUE)
        
        showModal(
          modalDialog(title = "Trip Record Generator",
                      
                      # editor top panel: trip summary table and point of interest buttons ----
                      trip_summary_panel_ui(ns("trip_summary_panel")),
                      
                      div("Select a type of trip to insert below the selected trip:"),
                      
                      footer = column(12,
                                      class = "trip-buttons-panel",
                                      actionButton(ns("clickblank"), "(Blank Trip)"),
                                      actionButton(ns("clickreversetrip"), "Reverse Trip"),
                                      actionButton(ns("clickreturnhome"), "(Return Home Trip)"),
                                      modalButton('Cancel')
                      ),
                      size = "l"
          ))
      }
      # if no row is selected
      else{
        notification_warning_select_row()
      }
      
      }) # end observeEvent
    
    # blank trip ----
    observeEvent(input$clickblank, { 
      
      trip_summary_panel_server("trip_summary_panel", selected_recid(), incl_poi = TRUE)
      
      # generate blank trip from prev trip (used to infer values for some columns)
      rval$blank_trip_record <- generate_blank_trip(selected_recid()) 
      
      # enable data validation ----
      iv <- editdata_datavalidation(input)

      # browser()
      showModal(
        modalDialog(title = "Trip Record Editor: Add Blank Trip",
                    
                    div("Adding reverse trip after this selected trip:"),
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    trip_summary_panel_ui(ns("trip_summary_panel")),
                    # trip editor: all input boxes
                    trip_editor_input_block(id = ns("data_edit"), trip_record = rval$blank_trip_record),
                    
                    footer = div(
                      # column(12,
                      style = "display: flex; justify-content: space-between;",
                      # elevate comment
                      div(
                        textInputSimple(ns("data_edit-psrc_comment"),
                                        df = rval$trip_record,
                                        label_name = "Add comment",
                        ) # end div
                      ),
                      div(style = "margin-top: 30px;",
                          actionButton(ns("clickupdate"), label = "Preview Trip"),
                          modalButton('Cancel')
                      ) # end div
                    ),
                    size = "l"))
    })
    
    ## Show Preview Pane & Apply Changes ----
    observeEvent(input$clickupdate, {
      
      # trip summary panel
      trip_summary_panel_server("trip_summary_panel_update", selected_recid())
      
      # create compare table for visualization
      compare_table <- generate_compare_table(input, rval$blank_trip_record)
      # generate insert trip for database table update
      rval$insert_trip <- generate_insert_trip(input, rval$blank_trip_record)
      
      output$print_cols <- show_compare_table(compare_table)
      
      ## Update Trip Record Preview ----
      showModal(
        modalDialog(title = "Insert Trip Preview",
                    
                    div("The new trip will be inserted after this trip:"),
                    
                    # editor top panel: trip summary table ----
                    trip_summary_panel_ui(ns("trip_summary_panel_update")),
                    
                    div("New Trip Record:"),
                    div( DTOutput(ns('print_cols')) ),
                    
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("pushblank"), label = "Insert Trip"),
                      modalButton('Close')
                    ),
                    size = "l"
        )
      )
      
    })
    
    ## Update Data in Database ----
    observeEvent(input$pushblank, {
      
      # write update query
      sproc_insert_blank_trip(selected_recid(), rval$blank_trip_record[['person_id']], rval$insert_trip)
      
    })
    
    # return home ----
    observeEvent(input$clickreturnhome, { 
      
      # create trip summary panel ----
      trip_summary_panel_server("trip_summary_panel", selected_recid(), incl_poi = TRUE)
      
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
                                    modalButton('Add Return Home Trip'),
                                    modalButton('Cancel')
                    ),
                    size = "l"))
      })
    
    
    # reverse trip ----
    observeEvent(input$clickreversetrip, { 
      
      # create trip summary panel ----
      trip_summary_panel_server("trip_summary_panel", selected_recid(), incl_poi = TRUE)
      
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
                                    actionButton(ns("pushaddreverse"), 'Add Reverse Trip'),
                                    modalButton('Cancel')
                    ),
                    size = "l"))
    })
    
    ## Update reverse trip in Database ----
    observeEvent(input$pushaddreverse, {
      
      reverse_datetime <- combine_datetime(input[["reverse_trip-depart_timestamp_date"]],
                                           input[["reverse_trip-depart_timestamp_time"]])
      sproc_insert_reverse_trip(selected_recid(), reverse_datetime)
      
      })

    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "Insert Trip") })
    
  })  # end moduleServer
  
}