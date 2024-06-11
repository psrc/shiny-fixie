# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  transit_overview_server('OVERVIEWtransit')
  
  # Regional NTD metrics
  transit_region_server('REGIONtransit')
  
  # Regional NTD metrics by Mode
  transit_mode_server('MODEtransit')
  
})    
