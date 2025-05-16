# buttons lead to sql sprocs: https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql

modal_edit_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_edit_trip_server <- function(id, selected_recid = NULL, updated_trip = NULL) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    trip_record <-  reactive({ get_data(view_name="Trip", recid=selected_recid()) })
    
    # featured buttons ----
    modal_copy_latlong_server("button-copy_origin", lat_input=input$`data_edit-origin_lat`, long_input=input$`data_edit-origin_lng`)
    modal_copy_latlong_server("button-copy_dest", lat_input=input$`data_edit-dest_lat`, long_input=input$`data_edit-dest_lng`)
    # modal_update_trip_server("button-update_db", all_input=input, recid=selected_recid())
    
    # show basic trip information ----
    output$trip_summary <- DT::renderDT(
      trip_record() %>% select(hhid,pernum,person_id,tripnum,recid), 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single', pageLength =-1))
    
    # Trip Record Editor ----
      observeEvent(input$clickedit, { 
        
        # if a row is selected in table: show Trip Record Editor
        if(!identical(selected_recid(),integer(0))){
          # browser()
          showModal(
            modalDialog(title = "Trip Record Editor",
                        
                        tagList(
                          # show trip table ----
                          DT::DTOutput(ns("trip_summary")),
                          br(),
        
                          # variable editing list ----
                          
                          ## time and distance ----
                          tags$div("time and distance"), hr(style = "border-top: 1px solid #000000;"),
                          ## ----------------------
                          fluidRow(
                            # timestamps
                            # TODO: find better way to select date and time
                            column(6, textInputSimple(df = trip_record(), var_name = ns("data_edit-depart_time_timestamp")),
                                      textInputSimple(df = trip_record(), var_name = ns("data_edit-arrival_time_timestamp"))),
                            # trip distance
                            column(6, numericInputSimple(df = trip_record(), var_name = ns("data_edit-distance_miles"), min = 0))),
                          br(),
                          
                          ## trip origin and destination ----
                          fluidRow(
                            column(8, tags$div("trip origin and destination")),
                            # button for mapping direction
                            # TODO: update lat/long if user edits
                            column(4, actionButton_google_direction("get_directions", df = trip_record()))
                            ),
                          hr(style = "border-top: 1px solid #000000;"),
                          ## --------------------------------
                          fluidRow(
                            # button for mapping origin
                            actionButton_google_place("open_origin", label = "Open origin location in Google Maps", 
                                                      df = trip_record(), lat_var_name = "origin_lat", long_var_name = "origin_lng")),
                          fluidRow(
                            # origin purpose
                            column(5, selectInputSingle(df = trip_record(), var_name = ns("data_edit-origin_purpose"))),
                            # origin label
                            # column(3, textInputSimple(df = trip_record(), var_name = ns("data_edit-origin_label"))),
                            # origin lat/long
                            column(4, 
                                   fluidRow(column(6, numericInputSimple(df = trip_record(), var_name = ns("data_edit-origin_lat"))),
                                            column(6, numericInputSimple(df = trip_record(), var_name = ns("data_edit-origin_lng")))),
                                   # button for copying origin lat/long to clipboard
                                   fluidRow(modal_copy_latlong_ui(ns('button-copy_origin'))))
                            ),
                          br(),
                          fluidRow(
                            # button for mapping destination
                            actionButton_google_place("open_dest", label = "Open destination location in Google Maps", 
                                                      df = trip_record(), lat_var_name = "dest_lat", long_var_name = "dest_lng")),
                          fluidRow(
                            # destination purpose
                            column(5, selectInputSingle(df = trip_record(), var_name = ns("data_edit-dest_purpose"))),
                            # destination label
                            # column(3, textInputSimple(df = trip_record(), var_name = ns("data_edit-dest_label"))),
                            # destination lat/long
                            column(4, 
                                   fluidRow(column(6, numericInputSimple(df = trip_record(), var_name = ns("data_edit-dest_lat"))),
                                            column(6, numericInputSimple(df = trip_record(), var_name = ns("data_edit-dest_lng")))),
                                   # button for copying destination lat/long to clipboard
                                   fluidRow(modal_copy_latlong_ui(ns('button-copy_dest'))))
                          ),
                          br(),
                          
                          ## mode type ----
                          
                          fluidRow(column(5, tags$div("mode type"), hr(style = "border-top: 1px solid #000000;"),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-mode_1")),# inputId = paste0("select_",var_name)
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-mode_2")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-mode_3")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-mode_4")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-mode_acc")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-mode_egr"))),
                                   column(7, tags$div("travelers"), hr(style = "border-top: 1px solid #000000;"),
                                          column(7,selectInputSingle(df = trip_record(), var_name = ns("data_edit-driver")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-travelers_total")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-travelers_hh")),
                                             selectInputSingle(df = trip_record(), var_name = ns("data_edit-travelers_nonhh"))),
                                          column(5, 
                                             tags$style("div {font-weight: bold;}"),
                                             tags$div("hhmembers"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember1"), label_name = "1"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember2"), label_name = "2"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember3"), label_name = "3"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember4"), label_name = "4"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember5"), label_name = "5"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember6"), label_name = "6"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember7"), label_name = "7"),
                                             numericInputSimple(df = trip_record(), var_name = ns("data_edit-hhmember8"), label_name = "8")))
                                   )
                        ),
                        br(),
                        fluidRow(modalButton('(Change Data)')),
                        footer = column(12, 
                                        modalButton('(Elevate: describe issue)'),
                                        modalButton('(Dismiss flag)'),
                                        modalButton('(Delete trip)'),
                                        modalButton('(Split from traces)'),
                                        modalButton('Cancel')),
                        # easyClose = TRUE,
                        size = "l"
            ))}
        # if no row is selected
          else{
            showModal(
              modalDialog(
                title = "0 records have been selected",
                easy_close = TRUE,
                "Please select a record from the table below to continue."
              )
            )
          }
        }
        
        )
      
      
    

    output$editbutton <- renderUI({  actionButton(ns("clickedit"), "Edit trip") }) 
    
  })  # end moduleServer
}