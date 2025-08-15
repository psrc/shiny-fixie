# buttons lead to sql sprocs: https://github.com/psrc/travel-survey-QC-Clean/blob/main/survey_data_cleaning/fixie_sprocs.sql

modal_edit_trip_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("editbutton"))

}

modal_edit_trip_server <- function(id, selected_recid) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rval <- reactiveValues(trip_record = NULL,
                           edit_list = NULL,
                           test_dissmissflag = NULL)

    # grey out invalid buttons ----
    observe({
      # grey out dismiss flag button if this trip has no error flag
      toggleState(id = "clickdissmissflag", condition = nrow(rval$test_dissmissflag)>0)
      # grey out apply changes button if no edits were made
      toggleState(id = "pushedit", condition = length(rval$edit_list)>0 & !is.null(rval$edit_list))
    })

    # Trip Record Editor ----
    observeEvent(input$clickedit, {
      # browser()
      
      # if a single row is selected in table: show Trip Record Editor
      if(length(selected_recid())==1){

        rval$trip_record <- get_trip_record(selected_recid())
        rval$test_dissmissflag <- get_data(view_name = "trip_error_flags", recid = selected_recid())
        
        # trip summary panel
        trip_summary_panel_server("trip_summary_panel", selected_recid(), incl_poi = TRUE)
        
        # enable data validation ----
        iv <- editdata_datavalidation(input)
        
        # Trip Record Editor ---
        showModal(
          modalDialog(title = "Trip Record Editor",

                      tagList(
                        
                        # editor top panel: trip summary table and point of interest buttons ----
                        trip_summary_panel_ui(ns("trip_summary_panel")),
                        # trip editor: all input boxes
                        trip_editor_input_block(id = ns("data_edit"), trip_record = rval$trip_record)

                      ), # end tagList
                      br(),
                      footer = div(
                        # column(12,
                        style = "display: flex; justify-content: space-between;",
                        # elevate comment
                        div(
                          textInputSimple(ns("data_edit-psrc_comment"),
                                          df = rval$trip_record,
                                          label_name = "Add comment",
                          ) # end div
                        ),
                        div(style = "margin-top: 30px;",
                          actionButton(ns("clickupdate"), label = "Preview Edits"),
                          actionButton(ns("clickdissmissflag"), label = "Dismiss Flag"),
                          modalButton('Cancel')
                        ) # end div
                      ),
                      size = "l"
          ))}
      # if no row or multiple rows are selected
      else{
          notification_warning_select_row()
        }
      })
    
    
    # ---- Show Preview Pane & Apply Changes ----
    observeEvent(input$clickupdate, {
      # browser()
      # trip summary panel
      trip_summary_panel_server("trip_summary_panel_update", selected_recid())
      
      ## ---- print all comparison table ----
      
      # generate compare table and named list of all edits
      compare_table <- generate_compare_table(input, rval$trip_record)
      rval$edit_list <- compare_table[compare_table$mod == 1, "Updated Value"]
      names(rval$edit_list) <- compare_table[compare_table$mod == 1, "Variable"]
      
      output$print_cols <- show_compare_table(compare_table)
      
      ## Update Trip Record Preview ----
      showModal(
        modalDialog(title = "Update Trip Record Preview",
                    
                    # editor top panel: trip summary table ----
                    trip_summary_panel_ui(ns("trip_summary_panel_update")),
                    
                    div(
                      DTOutput(ns('print_cols'))
                    ),
                    
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("pushedit"), label = "Apply Changes"),
                      modalButton('Close')
                    ),
                    easyClose = TRUE,
                    size = "l"
        )
      )
      
    })
    
    # ---- Update Data in Database ----
    observeEvent(input$pushedit, {

      # write update query
      sproc_update_data(selected_recid(), rval$trip_record[["person_id"]], rval$edit_list)
      
    })
    
    # ---- Dismiss Flag ----
    observeEvent(input$clickdissmissflag, {
      
      # trip summary panel
      trip_summary_panel_server("trip_summary_panel", selected_recid())
      
      showModal(
        modalDialog(title = "Are you sure you want to dismiss this error flag?",
                    
                    ## editor top panel: trip summary table ----
                    trip_summary_panel_ui(ns("trip_summary_panel")),
                    
                    footer = div(
                      style = "display: flex; justify-content: space-between;",
                      # push changes to database
                      actionButton(ns("pushdismissflag"), label = 'Yes'),
                      modalButton('No')
                    ),
                    size = "l") 
        )
      
    })
    
    ## ---- Confirm Dismiss Flag ----
    observeEvent(input$pushdismissflag, {
      
      # executes dismiss flag and show success message
      sproc_dismiss_flag(selected_recid(), rval$trip_record[["person_id"]])
      
    })
    


    output$editbutton <- renderUI({  actionButton(ns("clickedit"), "Edit trip") })

  })  # end moduleServer
}