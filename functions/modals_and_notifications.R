# ---- Warning: select trip ----
modal_warning_select_row <- modalDialog(
  title = "0 records have been selected",
  easy_close = TRUE,
  "Please select a record from the table below to continue."
  )

# ---- Successful action ----
notification_confirm_action <- function(title){
  
  removeModal()
  
  # switch to notification
  showNotification(
    title,
    type = "message")
  
}