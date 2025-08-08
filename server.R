# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  
  # Dashboard Overview
  # overview_server('tab_overview')
  
  # main user interface
  edit_interface_server('main_page', selected_error_type = reactive(input$dropdown_error_type))
  # dropdown to select error types and refresh button
  error_dropdown_server('dropdown_errortype')
  
})    
