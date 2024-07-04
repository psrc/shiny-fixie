
get_var_value_list <- function(value_labels = codebook_2023, var_name){
  
  val_list <- value_labels %>% filter(variable == var_name) %>%
    rowwise() %>%
    mutate(value_label = paste0(value, ": ", label)) %>%
    ungroup() %>%
    select(value, value_label)
  
  return(setNames(as.list(val_list$value), nm = val_list$value_label))
}

# dropdown for single selection
selectInputSingle <- function(df, var_name, ...) {
  
  selectInput(inputId = paste0("select_",var_name),
              label = var_name,
              choices = get_var_value_list(var_name=var_name), 
              selected = df[1,c(var_name)], 
              selectize = TRUE,
              ...)
}