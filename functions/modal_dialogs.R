# ---- Warning: select trip ----
modal_warning_select_row <- modalDialog(
  title = "0 records have been selected",
  easy_close = TRUE,
  "Please select a record from the table below to continue."
  )

# ---- Confirm action ----
modal_confirm_action <- function(title){
  
  modalDialog(title = title,
              footer = div(
                style = "display: flex; justify-content: space-between;",
                modalButton('Ok')
              ),
              easyClose = TRUE,
              size = "l")
  
}