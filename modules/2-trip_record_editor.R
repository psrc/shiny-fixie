# buttons lead to sql sprocs: https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql

modal_edit_trip_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("editbutton"))

}

modal_edit_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    rval <- reactiveValues(compare_table = NULL,
                           updated_trip = NULL)


    # Trip Record Editor ----
    observeEvent(input$clickedit, {
      # browser()   
      
      recid <- selected_recid()
      
      # get trip data from database
      trip_record <- get_trip_record(recid)
      # trip summary
      trip_summary_table <- get_trip_summary(trip_record)
      output$trip_summary <- render_trip_summary(trip_summary_table)
      
      
      # if a row is selected in table: show Trip Record Editor
      if(!identical(recid,integer(0))){
        
        # enable data validation
        iv <- add_datavalidation(input)
        
        # get point of interest locations
        
        home_geog <- get_poi_geog("home_geog", hhid = trip_record['hhid'])
        work_geog <- get_poi_geog("work_geog", person_id = trip_record['person_id'])
        school_geog <- get_poi_geog("school_geog", person_id = trip_record['person_id'])

        observe({
          # grey out dismiss flag button if this trip has no error flag
          toggleState(id = "clickdissmissflag", condition = !is.na(trip_summary_table[['error_flag']]))
          # grey out poi location buttons if this no valid location
          toggleState(id = "open_home_geog", condition = home_geog!="NA, NA")
          toggleState(id = "open_work_geog", condition = work_geog!="NA, NA")
          toggleState(id = "open_school_geog", condition = school_geog!="NA, NA")
        })
        
        poi_config <- tribble(
          ~ inputId,               ~ latlong,   ~ icon,
          ns("open_home_geog"),    home_geog,   "house",
          ns("open_work_geog"),    work_geog,   "briefcase",
          ns("open_school_geog"),  school_geog, "school-flag",
        )
        
        showModal(
          modalDialog(title = "Trip Record Editor",

                      tagList(
                        
                        fluidRow(
                          column(10,
                                 ## show trip table ----
                                 div(
                                   class = "bottom-spacing",
                                   DT::DTOutput(ns("trip_summary"))
                                 ),
                          ), # end column
                          column(2,
                                 ## show home, work and school location ----
                                 pmap(poi_config, actionButton_google_poi)
                                 
                          ) # end column
                        ), # fluidRow
                        
                        
                        ## timestamps ----
                        fluidRow(
                          column(6,
                                 div(class = "modal-header", "Trip Origin"),
                                 dateTimeInput(ns("data_edit-depart_time_timestamp"), df = trip_record)
                          ), # end column
                          column(6,
                                 div(class = "modal-header", "Trip Destination"),
                                 dateTimeInput(ns("data_edit-arrival_time_timestamp"), df = trip_record)
                                 
                          ) # end column
                        ), # fluidRow
                        
                        ## purposes ----
                        fluidRow(class = "bottom-spacing",
                                 column(6, selectInputSingle(ns("data_edit-origin_purpose"), df = trip_record) ),
                                 column(6,
                                        selectInputSingle(ns("data_edit-dest_purpose"), df = trip_record),
                                        textInputSimple(ns("data_edit-dest_purpose_other"), df = trip_record)
                                 ) # end column
                        ), # fluidRow
                        
                        ## locations ----
                        fluidRow(class = "bottom-spacing",
                          column(6,
                                 fluidRow(
                                   column(4, numericInputSimple(ns("data_edit-origin_lat"), df = trip_record)),
                                   column(4, numericInputSimple(ns("data_edit-origin_lng"), df = trip_record)),
                                   column(4, 
                                          actionButton_google_place("open_origin",
                                                                    df = trip_record,
                                                                    lat_var_name = "origin_lat",
                                                                    long_var_name = "origin_lng"))
                                 )
                          ), # end column
                          column(6,
                                 fluidRow(column(4, numericInputSimple(ns("data_edit-dest_lat"), df = trip_record)),
                                          column(4, numericInputSimple(ns("data_edit-dest_lng"), df = trip_record)),
                                          column(4, 
                                                 actionButton_google_place("open_dest",
                                                                           df = trip_record,
                                                                           lat_var_name = "dest_lat",
                                                                           long_var_name = "dest_lng"))
                                          ),     
                          ) # end column
                        ), # fluidRow
                        
                        ## distance ----
                        fluidRow(class = "bottom-spacing",
                                 column(12, 
                                        div(numericInputSimple(ns("data_edit-distance_miles"), df = trip_record, min = 0)),
                                        div(actionButton_google_direction("get_directions", df = trip_record))
                                        )
                                 ), # fluidRow
                        

                        ## mode type ----

                        fluidRow(
                          column(9,
                                 div(class = "modal-header", "Travel Modes"),
                                 column(7,
                                        selectInputSingle(ns("data_edit-mode_1"), df = trip_record),
                                        selectInputSingle(ns("data_edit-mode_2"), df = trip_record),
                                        selectInputSingle(ns("data_edit-mode_3"), df = trip_record),
                                        selectInputSingle(ns("data_edit-mode_4"), df = trip_record)),
                                 column(5,
                                        textInputSimple(ns("data_edit-mode_other_specify"), df = trip_record),
                                        selectInputSingle(ns("data_edit-mode_acc"), df = trip_record),
                                        selectInputSingle(ns("data_edit-mode_egr"), df = trip_record))
                          ), # end column
                          column(3,
                                 div(class = "modal-header", "Travelers"),
                                 selectInputSingle(ns("data_edit-driver"), df = trip_record),
                                 selectInputSingle(ns("data_edit-travelers_hh"), df = trip_record),
                                 selectInputSingle(ns("data_edit-travelers_nonhh"), df = trip_record)
                                 ) # end column
                          ), # fluidRow

                        ## elevate comment ----

                        fluidRow(
                          column(5,
                                 textInputSimple(ns("data_edit-psrc_comment"),
                                                 df = trip_record,
                                                 label_name = "Add comment:")
                                 )
                        )

                      ), # end tagList
                      br(),
                      fluidRow(column(12,
                                      div(
                                        class = "trip-buttons-panel",
                                        actionButton(ns("clickupdate"), label = "Preview Edits")
                                          )
                                      )),
                      footer = column(12,
                                      class = "trip-buttons-panel",
                                      actionButton(ns("clickdissmissflag"), label = "Dismiss Flag"),
                                      modalButton('(Delete trip)'),
                                      modalButton('(Unlink trip)'),
                                      modalButton('Cancel')
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
      
      # data validation: Don't proceed if any input is invalid
      if(!iv$is_valid()) {
        showNotification(
          "Please fix the errors in the form before continuing",
          type = "warning"
        )
      }
      else{
        # get all editable variables
        input_tripeditor.cols <- paste0("data_edit-", tripeditor.cols)
        
        ## ---- create compare table ----
        compare_table <- NULL
        for(i in 1:length(tripeditor.cols)){
          # variable name
          var_name <- tripeditor.cols[i]
          var_input_name <- input_tripeditor.cols[i]
          
          # updated value
          if(input[[var_input_name]] == ""){
            # if text inputs like dest_purpose_other, psrc_comment are kept empty, the output would be ""
            # change to NA to match DB value
            updated_value <- NA
          }
          else{
            updated_value <- input[[var_input_name]]
          }
          
          compare_var <- as.data.frame(
            cbind(Variable = var_name,
                  # original value
                  `Original Value` = trip_record[[var_name]],
                  # updated value
                  `Updated Value` = updated_value
            )
          )
          
          # create df with original and updated values
          compare_table <- rbind(compare_table,
                                 compare_var)
        }
        ## ---- process datetime ----
        
        # updated timestamps
        depart_datetime <- paste(input[["data_edit-depart_time_timestamp_date"]],
                                 strftime(input[["data_edit-depart_time_timestamp_time"]], format="%H:%M:%S"))
        arrival_datetime <- paste(input[["data_edit-arrival_time_timestamp_date"]],
                                  strftime(input[["data_edit-arrival_time_timestamp_time"]], format="%H:%M:%S"))
        
        compare_var <- as.data.frame(
          cbind(Variable = c("depart_time_timestamp", "arrival_time_timestamp"),
                # original value
                `Original Value` = c(as.character(format(trip_record[["depart_time_timestamp"]], "%Y-%m-%d %H:%M:%S")),
                                     as.character(format(trip_record[["arrival_time_timestamp"]], "%Y-%m-%d %H:%M:%S"))),
                # updated value
                `Updated Value` = c(depart_datetime, arrival_datetime)
          )
        )
        
        compare_table <- rbind(compare_var, compare_table)
        
        # detect if values are modified
        rval$compare_table <- compare_table %>%
          mutate(mod=case_when(`Original Value`==`Updated Value`~0,
                               is.na(`Original Value`) & is.na(`Updated Value`)~0, # for empty text inputs
                               TRUE~1))
        
        ## ---- generate updated trip record ----
        trip <- NULL
        for(var_name in names(trip_record)){
          if(var_name %in% tripeditor.cols){
            row <- as.data.frame(input[[paste0("data_edit-",var_name)]])
          } else{
            row <- as.data.frame(trip_record[[var_name]])
          }
          
          if(is.null(trip)){
            trip <- row
          }
          else{
            trip <- cbind(trip, row)
          }
          
        }
        names(trip) <- names(trip_record)
        rval$updated_trip <- trip
        
        
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
                      
                      # show trip table
                      div(
                        class = "bottom-spacing",
                        DT::DTOutput(ns("trip_summary"))
                      ),
                      #TODO: add trip summary
                      div(
                        DTOutput(ns('print_cols'))
                      ),
                      
                      footer = div(
                        style = "display: flex; justify-content: space-between;",
                        # push changes to database
                        actionButton(ns("clickpush"), label = "Apply Changes"),
                        modalButton('Close')
                      ),
                      easyClose = TRUE,
                      size = "l"
          )
        )
      }
      
    })
    
    # ---- Update Data in Database ----
    observeEvent(input$clickpush, {
      
      # create a named list of all edits
      df <- rval$compare_table
      
      edit_list <- df[df$mod == 1, "Updated Value"]
      names(edit_list) <- df[df$mod == 1, "Variable"]
      
      # write update query
      sproc_update_data(recid, edit_list)
      
    })
    
    # ---- Dismiss Flag ----
    observeEvent(input$clickdissmissflag, {
      
      showModal(
        modalDialog(title = "Are you sure you want to dismiss this error flag?",
                    
                    # show trip table
                    div(
                      class = "bottom-spacing",
                      DT::DTOutput(ns("trip_summary"))
                    ),
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("clickdissmissflag_action"), label = 'Yes'),
                      modalButton('No')
                    ),
                    easyClose = TRUE,
                    size = "l") 
        )
      
    })
    
    ## ---- Confirm Dismiss Flag ----
    observeEvent(input$clickdissmissflag_action, {
      
      # executes dismiss flag and show success message
      sproc_dismiss_flag(recid, trip_record[["person_id"]])
      
    })
    


    output$editbutton <- renderUI({  actionButton(ns("clickedit"), "Edit trip") })

  })  # end moduleServer
}