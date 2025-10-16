trip_editor_input_block <- function(id, trip_record){
  
  tagList(
  # timestamps ----
    fluidRow(
      column(6,
             div(class = "modal-header", "Trip Origin"),
             dateTimeInput(NS(id, "depart_time_timestamp"), df = trip_record)
      ), # end column
      column(6,
             div(class = "modal-header", "Trip Destination"),
             dateTimeInput(NS(id, "arrival_time_timestamp"), df = trip_record)
             
      ) # end column
    ), # fluidRow
    
    ## purposes ----
    fluidRow(class = "bottom-spacing",
             column(6, selectInputSingle(NS(id, "origin_purpose"), df = trip_record) ),
             column(6,
                    selectInputSingle(NS(id, "dest_purpose"), df = trip_record),
                    textInputSimple(NS(id, "dest_purpose_other"), df = trip_record)
             ) # end column
    ), # fluidRow
    
    ## locations ----
    fluidRow(class = "bottom-spacing",
             column(6,
                    fluidRow(
                      column(4, numericInputSimple(NS(id, "origin_lat"), df = trip_record)),
                      column(4, numericInputSimple(NS(id, "origin_lng"), df = trip_record)),
                      column(4, 
                             class = "google-buttons",
                             actionButton_google_place("open_origin",
                                                       df = trip_record,
                                                       lat_var_name = "origin_lat",
                                                       long_var_name = "origin_lng"))
                    )
             ), # end column
             column(6,
                    fluidRow(
                      column(4, numericInputSimple(NS(id, "dest_lat"), df = trip_record)),
                      column(4, numericInputSimple(NS(id, "dest_lng"), df = trip_record)),
                      column(4, 
                             class = "google-buttons",
                             actionButton_google_place("open_dest",
                                                       df = trip_record,
                                                       lat_var_name = "dest_lat",
                                                       long_var_name = "dest_lng"))
                    ),     
             ) # end column
    ), # fluidRow
    
    ## distance ----
    fluidRow(class = "bottom-spacing",
             column(6,
                    style = "display: flex;", 
                    div(numericInputSimple(NS(id, "distance_miles"), df = trip_record, min = 0), 
                        style = "padding-right: 2rem;"),
                    div(class = "google-buttons",
                        actionButton_google_direction("get_directions", df = trip_record))
             )
    ), # fluidRow
    
    
    ## mode type ----
    
    fluidRow(
      column(9,
             div(class = "modal-header", "Travel Modes"),
             column(7,
                    selectInputSingle(NS(id, "mode_1"), df = trip_record),
                    selectInputSingle(NS(id, "mode_2"), df = trip_record),
                    selectInputSingle(NS(id, "mode_3"), df = trip_record),
                    selectInputSingle(NS(id, "mode_4"), df = trip_record)),
             column(5,
                    textInputSimple(NS(id, "mode_other_specify"), df = trip_record),
                    selectInputSingle(NS(id, "mode_acc"), df = trip_record),
                    selectInputSingle(NS(id, "mode_egr"), df = trip_record))
      ), # end column
      column(3,
             div(class = "modal-header", "Travelers"),
             selectInputSingle(NS(id, "driver"), df = trip_record),
             selectInputSingle(NS(id, "travelers_hh"), df = trip_record),
             selectInputSingle(NS(id, "travelers_nonhh"), df = trip_record)
      ) # end column
    ) # fluidRow
  )
  
}

show_compare_table <- function(compare_table){
  renderDT({
    
    datatable(compare_table,
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
}
