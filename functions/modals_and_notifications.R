# ---- Warning: select trip ----
notification_warning_select_row <- function(){
  
  # switch to notification
  showNotification(
    "Please select a record from the table below to continue",
    type = "warning")
  
}

# ---- Successful action ----
notification_confirm_action <- function(title){
  
  removeModal()
  
  # switch to notification
  showNotification(
    title,
    type = "message")
  
}