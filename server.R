# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  
  # Dashboard Overview
  overview_server('tab-overview')
  
  # error tabs
  # TODO: is it possible to auto detect number of tabs to match the number of error type views in database?
  edit_interface_server('tab-error1',edit_persons="person_all")
  edit_interface_server('tab-error2',edit_persons="person_Abdi")
  
})    
