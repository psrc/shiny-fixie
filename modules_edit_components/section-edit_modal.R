edit_modal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editbutton"))
  )
}

edit_modal_server <- function(id, label_name, selected_rows) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$printrow = renderPrint({
      s = selected_rows()
      if (length(s)) {
        cat('These rows were selected:\n\n')
        cat(s, sep = ', ')
      }
    })
    
    # data cleaning tools ----
    observeEvent(input$clickedit, {
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!",
        div(verbatimTextOutput(ns('printrow'))),
        easyClose = TRUE
      ))
    })

    output$editbutton <- renderUI({
      tagList(
        fluidRow(column(12, actionButton(ns("clickedit"), label_name)))
      )
    }) 
  })  # end moduleServer
}