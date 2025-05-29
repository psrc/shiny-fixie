# TODO: create comparison table with SQL sproc and show in modal
# old version with somparison table created in-house is in pause-error_flag-4-modal_edit_trip-update_preview-with_comparison_table.R

modal_update_trip_ui <- function(id) {
  ns <- NS(id)
  
    uiOutput(ns("updatebutton"))
  
}

modal_update_trip_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$clickupdate, { 
      
      
      showModal(
        modalDialog(title = "Update Trip Record Preview (Placeholder)",
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      
                      modalButton('Close')
                    ),
                    easyClose = TRUE,
                    size = "l"
        )
      ) 
    })
      
    output$updatebutton <- renderUI({
      actionButton(ns("clickupdate"),
                   "Apply Changes")
      })
    
  })  # end moduleServer
}