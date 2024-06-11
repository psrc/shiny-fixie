# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Main Panel
  left_panel_server('leftMain', page_nm = "Main")
  
  # Dashboard Overview
  transit_overview_server('OVERVIEWtransit')
  
  # Regional NTD metrics
  ## Option A: the module below is how to place dtedit inside a module ----
  transit_region_server('REGIONtransit', mydata = df)
  
  ## Option B: "dtedit_server" is already a module, straight from the DTedit package. "df" is defined in global.R ----
  dtedit_server(id = 'dtedit_example',
                thedata = df,
                edit.cols = names(df),
                edit.label.cols = names(df))
  
  # Regional NTD metrics by Mode
  transit_mode_server('MODEtransit')
  
})    
