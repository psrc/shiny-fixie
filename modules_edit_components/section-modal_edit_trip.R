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
      cat(selected_row)
    })
    # print(class(selected_row()))
    trip_data <- reactive({get_data(view_name="Trip", recid=selected_row)})
    # mode_desc_value <- reactive({ifelse(is.na(trip_data()[1,c("mode_desc")]), '', trip_data()[1,c("mode_desc")])})
    # mode_desc_value <- list_mode_choice[1]
    
    # test: show row data ----
    output$triprecord <- DT::renderDT(
      trip_data(), 
      rownames = FALSE,
      options =list(ordering = F, dom = 't',  selection = 'single'))
    
    
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { showModal(
      modalDialog(title = "Trip Editor",
                  
                  "This is where the trip editing panel going to be!",
                  div(verbatimTextOutput(ns('print_row'))),
                  
                  # mode_1
                  # selectInput(inputId = "select_mode_desc",
                  #             label = "mode",
                  #             choices = list_mode_choice,
                  #             selected = mode_desc_value),
                  
                  DT::DTOutput(ns("triprecord")),
                  
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