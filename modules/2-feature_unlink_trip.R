modal_unlink_trip_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("editbutton"))
  
}

modal_unlink_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # data cleaning tools ----
    observeEvent(input$clickedit, { 
      
      # get trip ingredients
      trip_link_ids <- paste(
        get_data(custom_query = glue("SELECT trip_link FROM HHSurvey.trip_ingredients_done WHERE recid = {selected_recid()}"))[['trip_link']], 
        collapse = ",")
      
      if(length(selected_recid())==1 & trip_link_ids!=""){
        
        trip_record <- get_trip_record(selected_recid())
        
        # browser()
        output$ingedients_table <-  DT::renderDT(
          
          # get trip ingredients table
          get_data(
            custom_query = glue("SELECT recid, DepartTime, ArriveTime, OriginPurpose, DestPurpose, mode_1 
                                 FROM HHSurvey.ingredient2fixie
                                 WHERE person_id = {trip_record['person_id']} and trip_link IN ({trip_link_ids})
                                 ORDER BY person_id,daynum,DepartTime,ArriveTime")
            ) %>%
            group_by(recid) %>%
            filter(ArriveTime == min(ArriveTime)),
          
          rownames = FALSE,
          selection = 'none',
          options =list(ordering = F,
                        dom = 't',
                        pageLength =-1)
          
        )
        
        
        
        # trip summary panel
        trip_summary_panel_server("trip_summary_panel", selected_recid())
        
        showModal(
          modalDialog(
            title = "Trip Unlinking Editor",
            
            div("Are you sure you want to unlink this trip?"),
            
            # editor top panel: trip summary table and point of interest buttons ----
            trip_summary_panel_ui(ns("trip_summary_panel")),
            
            div("The trip will be split into:"),
            
            div(
              class = "bottom-spacing",
              DT::DTOutput(ns("ingedients_table"))
            ),
            
            footer = div(
              style = "display: flex; justify-content: space-between;",
              # push changes to database
              actionButton(ns("pushunlink"), label = 'Yes'),
              modalButton('No'),
              width=12
            ),
            size = "l")
        ) 
        
      }
      else if(trip_link_ids==""){
        showNotification(
          "Cannot unlink trip. This trip hasn't been linked yet",
          type = "warning")
      }
      else{
        notification_warning_select_row()
      }
      
      
    })
    
    observeEvent(input$pushunlink, { 
      
      sproc_unlink_trip(selected_recid())
      
    })
    
    output$editbutton <- renderUI({ actionButton(ns("clickedit"), "Unlink trip") }) 
    
  })  # end moduleServer
}