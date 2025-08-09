# ---- Warning: select single trip ----
notification_warning_select_row <- function(){
  
  # switch to notification
  showNotification(
    "Please select a single record from the table to continue",
    type = "warning")
  
}

# ---- Warning: select linking trip ----
notification_warning_select_linking <- function(){
  
  # switch to notification
  showNotification(
    "Please select two or more consecutive records from the table to continue",
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