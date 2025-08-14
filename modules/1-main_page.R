edit_interface_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editplatform"))
  )
}

#' HTS data editor user interface
edit_interface_server <- function(id, selected_error_type) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # person control panel ----
    
    personID <- person_panel_server("panel-person", selected_error_type=reactive(selected_error_type()))
    
    # the trip table ----
    
    # person data from database
    edit_dt <- reactive({ get_data(person_id = personID(), order_by=c("person_id", "tripnum")) })
    
    output$thetable <- DT::renderDataTable(
      
      edit_dt() %>% 
        # remove missing response pattern
        mutate(Modes = str_replace(Modes, ",?Missing Response,?", "")) %>%
        select(-c("person_id")),
      
      options =list(ordering = F, dom = 't'), 
      rownames = FALSE, 
      server=TRUE
      
      )
    
    
    # data cleaning tools ----
    
    selected_row_recid <- reactive({ edit_dt()[input$thetable_rows_selected, "recid"] })
    
    ## button to add new trip
    modal_new_trip_server("button_new", selected_recid = reactive(selected_row_recid()))
    ## activate Edit Trip modal
    modal_edit_trip_server("button_edit", selected_recid = reactive(selected_row_recid()))
    ## button to delete trip
    modal_delete_trip_server("button_delete", selected_recid = reactive(selected_row_recid()))
    ## link trips interface
    modal_link_trips_server("button_link", selected_recid = reactive(selected_row_recid()))
    ## unlink trip interface
    modal_unlink_trip_server("button_unlink", selected_recid = reactive(selected_row_recid()))
    
    # platform layout ----
    
    output$editplatform <- renderUI({
      tagList(
        fluidRow(class = "page-format",
          
          # person panel
          column(8, 
                 person_panel_ui(ns("panel-person"))
                 ),
          
          # trip editing panel
          column(4,
                 fluidRow(
                   wellPanel(
                     
                     p("Select one trip to edit"),
                     
                     div(class = "trip-buttons-panel",
                         modal_new_trip_ui(ns('button_new')),
                         modal_edit_trip_ui(ns('button_edit')),
                         modal_delete_trip_ui(ns('button_delete')),
                         modal_unlink_trip_ui(ns('button_unlink'))
                         ) # end div
                     
                     ), # end wellpanel
                   
                   wellPanel(
                     
                     p("Select multiple consecutive trips to link"),
                     modal_link_trips_ui(ns('button_link'))
                     
                     ) # end wellpanel
                 )
          ), # end column
          
        ), # end fluidrow
        
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
      
      ) # end taglist
    }) 
    
  }) 
}

