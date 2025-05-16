# get list of unique values for a variable
get_var_value_list <- function(value_labels = codebook, var_name){
  
  val_list <- value_labels %>% filter(variable == var_name) %>%
    rowwise() %>%
    mutate(value_label = paste0(value, ": ", final_label)) %>%
    ungroup() %>%
    select(value, value_label)
  
  return(setNames(as.list(val_list$value), nm = val_list$value_label))
}

# dropdown for single selection
selectInputSingle <- function(df, var_name, label_name = NULL, ...) {
  
  var_str <- gsub("^.*-", "", var_name)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  selectInput(inputId = var_name,
              label = label_name,
              choices = get_var_value_list(var_name=var_str), 
              selected = df[1,c(var_str)], 
              selectize = TRUE,
              ...)
}

# text input
textInputSimple <- function(df, var_name, label_name = NULL, ...){
  
  var_str <- gsub("^.*-", "", var_name)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  textInput(inputId = var_name, 
            label = label_name, 
            value = df[1,c(var_str)],
            ...)
}

# number input
numericInputSimple <- function(df, var_name, label_name = NULL, min = NA, max = NA, ...){
  
  var_str <- gsub("^.*-", "", var_name)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  numericInput(inputId = var_name,
               label = label_name,
               value = df[1,c(var_str)],
               min = min, max = max,
               ...)
  
}
