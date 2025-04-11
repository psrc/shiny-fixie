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
  
  # numericInput(inputId = paste0("trip_record_",var_name),
  #              label = label_name, 
  #              value = df[1,c(var_name)],
  #              min = min, max = max, 
  #              ...)
}

# link to google maps showing direction from origin to destination
actionButton_google_direction <- function(inputId, df){
  
  origin <- reactive({ paste0(df[1,c("origin_lat")], ", ", df[1,c("origin_lng")]) })
  dest <- reactive({ paste0(df[1,c("dest_lat")], ", ", df[1,c("dest_lng")]) })
  
  url <- paste0("https://www.google.com/maps/dir/", origin(), "/", dest())
  
  actionButton(inputId=inputId, 
               label="Get directions in Google Maps", value = "Open popup",onclick =paste0("window.open('",url,"','_blank')"))
}

# link to google maps showing origin or destination locations
actionButton_google_place <- function(inputId, label, df, lat_var_name, long_var_name){
  
  latlong <- reactive({ paste0(df[1,c(lat_var_name)], ", ", df[1,c(long_var_name)]) })
  
  url <- paste0("https://www.google.com/maps/place/", latlong())
  
  actionButton(inputId=inputId, 
               label=label, value = "Open popup",onclick =paste0("window.open('",url,"','_blank')"))
}