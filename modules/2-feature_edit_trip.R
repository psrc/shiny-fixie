# buttons lead to sql sprocs: https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql

modal_edit_trip_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("editbutton"))

}

modal_edit_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # create objects ----
    # values that share across multiple observeEvents
    rval <- reactiveValues(recid = NULL,
                           compare_table = NULL,
                           updated_trip = NULL,
                           trip_record = NULL)



    # Trip Record Editor ----
    observeEvent(input$clickedit, {
      
      rval$recid <- selected_recid()
      
      
      # if a row is selected in table: show Trip Record Editor
      if(!identical(rval$recid,integer(0))){

        # trip data, trip summary and datatable widget
        rval$trip_record <- get_trip_record(rval$recid)
        trip_summary_table <- get_trip_summary(rval$trip_record)
        
        # create trip summary panel ----
        trip_summary_panel_server("trip_summary_panel", rval$trip_record, incl_poi = TRUE)
        
        # enable data validation ----
        iv <- add_datavalidation(input)
        

        observe({
          # grey out dismiss flag button if this trip has no error flag
          toggleState(id = "clickdissmissflag", condition = !is.na(trip_summary_table[['error_flag']]))
        })
        
        
        showModal(
          modalDialog(title = "Trip Record Editor",

                      tagList(
                        
                        # editor top panel: trip summary table and point of interest buttons ----
                        trip_summary_panel_ui(ns("trip_summary_panel")),
                        
                        
                        ## timestamps ----
                        fluidRow(
                          column(6,
                                 div(class = "modal-header", "Trip Origin"),
                                 dateTimeInput(ns("data_edit-depart_time_timestamp"), df = rval$trip_record)
                          ), # end column
                          column(6,
                                 div(class = "modal-header", "Trip Destination"),
                                 dateTimeInput(ns("data_edit-arrival_time_timestamp"), df = rval$trip_record)
                                 
                          ) # end column
                        ), # fluidRow
                        
                        ## purposes ----
                        fluidRow(class = "bottom-spacing",
                                 column(6, selectInputSingle(ns("data_edit-origin_purpose"), df = rval$trip_record) ),
                                 column(6,
                                        selectInputSingle(ns("data_edit-dest_purpose"), df = rval$trip_record),
                                        textInputSimple(ns("data_edit-dest_purpose_other"), df = rval$trip_record)
                                 ) # end column
                        ), # fluidRow
                        
                        ## locations ----
                        fluidRow(class = "bottom-spacing",
                          column(6,
                                 fluidRow(
                                   column(4, numericInputSimple(ns("data_edit-origin_lat"), df = rval$trip_record)),
                                   column(4, numericInputSimple(ns("data_edit-origin_lng"), df = rval$trip_record)),
                                   column(4, 
                                          class = "google-buttons",
                                          actionButton_google_place("open_origin",
                                                                    df = rval$trip_record,
                                                                    lat_var_name = "origin_lat",
                                                                    long_var_name = "origin_lng"))
                                 )
                          ), # end column
                          column(6,
                                 fluidRow(
                                   column(4, numericInputSimple(ns("data_edit-dest_lat"), df = rval$trip_record)),
                                   column(4, numericInputSimple(ns("data_edit-dest_lng"), df = rval$trip_record)),
                                   column(4, 
                                          class = "google-buttons",
                                          actionButton_google_place("open_dest",
                                                                    df = rval$trip_record,
                                                                    lat_var_name = "dest_lat",
                                                                    long_var_name = "dest_lng"))
                                   ),     
                          ) # end column
                        ), # fluidRow
                        
                        ## distance ----
                        fluidRow(class = "bottom-spacing",
                                 column(6,
                                        style = "display: flex;", 
                                        div(numericInputSimple(ns("data_edit-distance_miles"), df = rval$trip_record, min = 0), 
                                            style = "padding-right: 2rem;"),
                                        div(class = "google-buttons",
                                            actionButton_google_direction("get_directions", df = rval$trip_record))
                                        )
                                 ), # fluidRow
                        

                        ## mode type ----

                        fluidRow(
                          column(9,
                                 div(class = "modal-header", "Travel Modes"),
                                 column(7,
                                        selectInputSingle(ns("data_edit-mode_1"), df = rval$trip_record),
                                        selectInputSingle(ns("data_edit-mode_2"), df = rval$trip_record),
                                        selectInputSingle(ns("data_edit-mode_3"), df = rval$trip_record),
                                        selectInputSingle(ns("data_edit-mode_4"), df = rval$trip_record)),
                                 column(5,
                                        textInputSimple(ns("data_edit-mode_other_specify"), df = rval$trip_record),
                                        selectInputSingle(ns("data_edit-mode_acc"), df = rval$trip_record),
                                        selectInputSingle(ns("data_edit-mode_egr"), df = rval$trip_record))
                          ), # end column
                          column(3,
                                 div(class = "modal-header", "Travelers"),
                                 selectInputSingle(ns("data_edit-driver"), df = rval$trip_record),
                                 selectInputSingle(ns("data_edit-travelers_hh"), df = rval$trip_record),
                                 selectInputSingle(ns("data_edit-travelers_nonhh"), df = rval$trip_record)
                                 ) # end column
                          ), # fluidRow

                      ), # end tagList
                      br(),
                      footer = div(
                        # column(12,
                        style = "display: flex; justify-content: space-between;",
                        # class = "trip-buttons-panel",
                        # elevate comment
                        div(
                          textInputSimple(ns("data_edit-psrc_comment"),
                                          df = rval$trip_record,
                                          label_name = "Add comment",
                          ) # end div
                        ),
                        div(style = "margin-top: 30px;",
                          actionButton(ns("clickupdate"), label = "Preview Edits"),
                          actionButton(ns("clickdissmissflag"), label = "Dismiss Flag"),
                          modalButton('(Unlink trip)'),
                          modalButton('Cancel')
                        ) # end div
                      ),
                      # easyClose = TRUE,
                      size = "l"
          ))}
      # if no row is selected
      else{
          notification_warning_select_row()
        }
      })
    
    
    # ---- Show Preview Pane & Apply Changes ----
    observeEvent(input$clickupdate, {
      
      # create trip summary panel ----
      trip_summary_panel_server("trip_summary_panel_update", rval$trip_record)
      
      # generate compare table and updated trip record ----
      rval$compare_table <- generate_compare_table(input, rval$trip_record)
      rval$updated_trip <- generate_updated_trip(input, rval$trip_record)
      
      
      ## ---- print all comparison table ----
      output$print_cols <- renderDT({
        
        datatable(rval$compare_table,
                  options =list(ordering = F,
                                dom = 't',
                                pageLength = -1,
                                # hide mod column
                                columnDefs = list(list(targets = 4,visible = FALSE)))
        ) %>%
          formatStyle(
            'mod',
            target = 'row',
            backgroundColor = styleEqual(c(0, 1), c('white', '#00A7A0'))
          )
        
      })
      
      ## ---- modal dialog: show update preview pane ----
      showModal(
        modalDialog(title = "Update Trip Record Preview",
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    trip_summary_panel_ui(ns("trip_summary_panel_update")),
                    
                    div(
                      DTOutput(ns('print_cols'))
                    ),
                    
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("pushedit"), label = "Apply Changes"),
                      modalButton('Close')
                    ),
                    easyClose = TRUE,
                    size = "l"
        )
      )
      
    })
    
    # ---- Update Data in Database ----
    observeEvent(input$pushedit, {
      
      # create a named list of all edits
      df <- rval$compare_table
      
      edit_list <- df[df$mod == 1, "Updated Value"]
      names(edit_list) <- df[df$mod == 1, "Variable"]
      
      # write update query
      sproc_update_data(rval$recid, rval$trip_record[["person_id"]], edit_list)
      
    })
    
    # ---- Dismiss Flag ----
    observeEvent(input$clickdissmissflag, {
      
      # create trip summary panel ----
      trip_summary_panel_server("trip_summary_panel", rval$trip_record)
      
      showModal(
        modalDialog(title = "Are you sure you want to dismiss this error flag?",
                    
                    # editor top panel: trip summary table and point of interest buttons ----
                    trip_summary_panel_ui(ns("trip_summary_panel")),
                    
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("pushdismissflag"), label = 'Yes'),
                      modalButton('No')
                    ),
                    size = "l") 
        )
      
    })
    
    ## ---- Confirm Dismiss Flag ----
    observeEvent(input$pushdismissflag, {
      
      # executes dismiss flag and show success message
      sproc_dismiss_flag(rval$recid, rval$trip_record[["person_id"]])
      
    })
    


    output$editbutton <- renderUI({  actionButton(ns("clickedit"), "Edit trip") })

  })  # end moduleServer
}