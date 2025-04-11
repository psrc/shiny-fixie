# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  
  # Dashboard Overview
  overview_server('tab_overview')
  
  # error tabs
  # TODO: is it possible to auto detect number of tabs to match the number of error type views in database?
  edit_interface_server('tab_error1',edit_persons=edit_persons_view_name[1])
  edit_interface_server('tab_error2',edit_persons=edit_persons_view_name[2])
  
})    
