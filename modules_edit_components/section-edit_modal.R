# editModal <- function(row) {
#   # output[[paste0(name, '_message')]] <- renderText('')
#   fields <- getFields('_edit_', values = result$thedata[row,])
#   shiny::modalDialog(title = title.edit,
#                      shiny::div(shiny::textOutput(paste0(name, '_message')), style='color:red'),
#                      fields,
#                      footer = column(shiny::modalButton('Cancel'),
#                                      shiny::actionButton(paste0(id, name, '_update'), 'Save'),
#                                      width=12),
#                      size = modal.size
#   )
# }
# 
# observeEvent(input$click_edit, {
#   row <- input[[paste0(name, 'dt_rows_selected')]]
#   if(!is.null(row)) {
#     if(row > 0) {
#       shiny::showModal(editModal(row))
#     }
#   }
# })


edit_modal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editbutton"))
  )
}

edit_modal_server <- function(id, label_name) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # data cleaning tools ----
    observeEvent(input$clickedit, {
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!",
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