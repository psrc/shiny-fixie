modal_edit_trip_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editbutton"))
  )
}

modal_edit_trip_server <- function(id, label_name, selected_row) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$print_row <- renderPrint({
      cat('These rows were selected:\n\n')
      cat(selected_row())
    })
    
    trip_data <- reactive({ get_data(view_name="Trip", recid=selected_row()) })
    
    # test: show row data ----
    output$triprecord <- DT::renderDT(
      trip_data() %>% select(hhid,pernum,person_id,tripnum,recid), 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single'))
    
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { showModal(
      modalDialog(title = "Trip Editor",

                  # print trip recid ----
                  "This is where the trip editing panel going to be!",
                  div(verbatimTextOutput(ns('print_row'))),

                  # show trip table ----
                  DT::DTOutput(ns("triprecord")),

                  # variable editing list ----
                  numericInput("numinput_distance_miles", "distance_miles", value = trip_data()[1,c("distance_miles")]),
                  selectInputSingle(df = trip_data(), var_name = "mode_acc"),
                  selectInputSingle(df = trip_data(), var_name = "mode_1"), # inputId = paste0("select_",var_name)
                  selectInputSingle(df = trip_data(), var_name = "mode_2"),
                  selectInputSingle(df = trip_data(), var_name = "mode_3"),
                  selectInputSingle(df = trip_data(), var_name = "mode_4"),
                  selectInputSingle(df = trip_data(), var_name = "mode_egr"),

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