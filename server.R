# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  
  # Dashboard Overview
  overview_server('OVERVIEW')
  
  # Regional NTD metrics
  edit_interface_server('errortab_1',edit_persons="person_all")
  edit_interface_server('errortab_2',edit_persons="person_Abdi")
  
})    
