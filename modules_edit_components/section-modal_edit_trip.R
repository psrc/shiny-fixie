modal_edit_trip_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editbutton"))
  )
}

modal_edit_trip_server <- function(id, label_name, selected_row) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    trip_data <- reactive({ get_data(view_name="Trip", recid=selected_row()) })
    
    # show basic trip information ----
    output$triprecord <- DT::renderDT(
      trip_data() %>% select(hhid,pernum,person_id,tripnum,recid), 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single'))
    
    # Trip Record Editor ----
    observeEvent(input$clickedit, { showModal(
      modalDialog(title = "Trip Record Editor",
                  
                  tagList(
                    # show trip table ----
                    DT::DTOutput(ns("triprecord")),
                    br(),
  
                    # variable editing list ----
                    
                    ## time and distance ----
                    tags$div("time and distance"), hr(style = "border-top: 1px solid #000000;"),
                    fluidRow(column(6, textInputSimple(df = trip_data(), var_name = "depart_time_timestamp"),
                                       textInputSimple(df = trip_data(), var_name = "arrival_time_timestamp")),
                             column(6, numericInputSimple(df = trip_data(), var_name = "distance_miles"))),
                    br(),
                    
                    ## trip origin and destination ----
                    tags$div("trip origin and destination"), hr(style = "border-top: 1px solid #000000;"),
                    fluidRow(column(5, selectInputSingle(df = trip_data(), var_name = "origin_purpose"),
                                       selectInputSingle(df = trip_data(), var_name = "dest_purpose")),
                             column(3, textInputSimple(df = trip_data(), var_name = "origin_label"),
                                       textInputSimple(df = trip_data(), var_name = "dest_label")),
                             column(2, numericInputSimple(df = trip_data(), var_name = "origin_lat"),
                                       numericInputSimple(df = trip_data(), var_name = "dest_lat")),
                             column(2, numericInputSimple(df = trip_data(), var_name = "origin_lng"),
                                       numericInputSimple(df = trip_data(), var_name = "dest_lng"))
                             ),
                    br(),
                    
                    ## mode type ----
                    
                    fluidRow(column(5, tags$div("mode type"), hr(style = "border-top: 1px solid #000000;"),
                                       selectInputSingle(df = trip_data(), var_name = "mode_acc"),
                                       selectInputSingle(df = trip_data(), var_name = "mode_1"),# inputId = paste0("select_",var_name)
                                       selectInputSingle(df = trip_data(), var_name = "mode_2"),
                                       selectInputSingle(df = trip_data(), var_name = "mode_3"),
                                       selectInputSingle(df = trip_data(), var_name = "mode_4"),
                                       selectInputSingle(df = trip_data(), var_name = "mode_egr")),
                             column(7, tags$div("travelers"), hr(style = "border-top: 1px solid #000000;"),
                                    column(7,selectInputSingle(df = trip_data(), var_name = "driver"),
                                       selectInputSingle(df = trip_data(), var_name = "travelers_total"),
                                       selectInputSingle(df = trip_data(), var_name = "travelers_hh"),
                                       selectInputSingle(df = trip_data(), var_name = "travelers_nonhh")),
                                    column(5, 
                                       tags$style("div {font-weight: bold;}"),
                                       tags$div("hhmembers"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember1", label_name = "1"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember2", label_name = "2"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember3", label_name = "3"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember4", label_name = "4"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember5", label_name = "5"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember6", label_name = "6"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember7", label_name = "7"),
                                       numericInputSimple(df = trip_data(), var_name = "hhmember8", label_name = "8"))),
                             
                             )
                    
                  ),

                  footer = column(modalButton('Cancel'),
                                  modalButton('Update Trip'),
                                  width=12),
                  easyClose = TRUE,
                  size = "l"
      )
    ) })

    output$editbutton <- renderUI({
      tagList(
        fluidRow(column(12, actionButton(ns("clickedit"), label_name)))
      )
    }) 
  })  # end moduleServer
}