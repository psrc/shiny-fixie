edit_interface_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("editplatform"))
  )
}

# HTS data editor user interface
edit_interface_server <- function(id, selected_error_type) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(edit_dt = NULL)
    observe({
      # person trip table from database
      rval$edit_dt <- get_data(person_id = personID(), order_by=c("person_id", "tripnum"))
    })
    
    selected_row_recid <- reactive({ rval$edit_dt[input$thetable_rows_selected, "recid"] })
    
    
    # person control panel ----
    
    personID <- person_panel_server("panel-person", selected_error_type=reactive(selected_error_type()))
    
    # data cleaning tools ----
    
    # button to add new trip
    modal_new_trip_server("button_new", 
                          selected_recid = reactive(selected_row_recid()))
    # activate Edit Trip modal
    modal_edit_trip_server("button_edit", 
                           selected_recid = reactive(selected_row_recid()))
    # button to delete trip
    modal_delete_trip_server("button_delete", 
                             selected_recid = reactive(selected_row_recid()))
    # link trips interface
    modal_link_trips_server("button_link", 
                            selected_recid = reactive(selected_row_recid()))
    # unlink trip interface
    modal_unlink_trip_server("button_unlink", 
                             selected_recid = reactive(selected_row_recid()))
    
    
    # the trip table ----
    output$thetable <- DT::renderDataTable(
      
      rval$edit_dt %>% 
        # remove missing response pattern
        mutate(Modes = str_replace(Modes, ",?Missing Response,?", "")) %>%
        select(-c("person_id")),
      
      class = list('hover row-border order-column'),
      options =list(ordering = F, 
                    dom = 't',
                    pageLength = -1,
                    scroller = TRUE,
                    scrollY = '60vh',
                    scrollCollapse = TRUE),
      fillContainer = T,
      rownames = FALSE,
      server=TRUE
      
      )
    
    observeEvent(input$refresh_table, {
      # clicking refresh button: update data in trip table
      
      rval$edit_dt <- get_data(person_id = personID(), order_by=c("person_id", "tripnum"))
      
    })  
    
    # platform layout ----
    output$editplatform <- renderUI({
      tagList(
        fluidRow(#class = "page-format",
          
          # person panel
          column(6, 
                 person_panel_ui(ns("panel-person"))
                 ),
          
          # trip editing panel
          column(6,
                 fluidRow(style = "margin: 0 1rem;",
                   wellPanel(
                     
                     div(class = "trip-buttons-panel",
                         p("Select one trip to edit"),
                         div(
                           style="margin-left: 5px",
                           # refresh button: refresh trip table
                           actionButton_refresh(ns("refresh_table")))
                         ),
                     
                     div(class = "trip-buttons-panel",
                         # buttons: trip editing functions
                         modal_new_trip_ui(ns('button_new')),
                         modal_edit_trip_ui(ns('button_edit')),
                         modal_delete_trip_ui(ns('button_delete')),
                         modal_unlink_trip_ui(ns('button_unlink'))
                         )
                     
                     ), # end wellpanel
                   
                   wellPanel(
                     
                     p("Select multiple trips to", style = "padding-right: 10px;"),
                     # buttons: trip linking function
                     modal_link_trips_ui(ns('button_link')),
                     style = "display: flex;"
                     
                     ) # end wellpanel
                 )
          ), # end column
          
        ), # end fluidrow
        
        fluidRow(column(12, DT::dataTableOutput(ns("thetable"))))
      
      ) # end taglist
    }) 
    
  }) 
}

