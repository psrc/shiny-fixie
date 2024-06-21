# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  transit_overview_server('OVERVIEWtransit')
  
  # Regional NTD metrics
  # transit_region_server('REGIONtransit')
  edit_interface_server('test-table',edit_persons="person_all")
  edit_interface_server('test-table2',edit_persons="person_Abdi")
  
  
  # Regional NTD metrics by Mode
  transit_mode_server('MODEtransit')
  
})    
