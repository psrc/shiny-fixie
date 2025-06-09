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

# datetime input - specialized for timestamp columns with proper date/time controls
datetimeInputSimple <- function(df, var_name, label_name = NULL, ...){
  
  var_str <- gsub("^.*-", "", var_name)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  # Get the datetime value and parse it properly
  datetime_val <- df[1, c(var_str)]
  
  # Parse datetime value
  parsed_datetime <- if(is.null(datetime_val) || is.na(datetime_val)) {
    Sys.time()  # Default to current time if no value
  } else {
    tryCatch({
      as.POSIXct(datetime_val)
    }, error = function(e) {
      Sys.time()  # Default to current time if parsing fails
    })
  }
  
  # Extract date and time components
  date_part <- as.Date(parsed_datetime)
  time_part <- format(parsed_datetime, "%H:%M:%S")
  
  # Create a container with both date and time inputs
  div(
    style = "margin-bottom: 15px;",
    tags$label(class = "control-label", `for` = paste0(var_name, "_date"), label_name),
    br(),
    fluidRow(
      column(6,
        dateInput(inputId = paste0(var_name, "_date"),
                 label = "Date",
                 value = date_part,
                 format = "yyyy-mm-dd")
      ),
      column(6,
        textInput(inputId = paste0(var_name, "_time"),
                 label = "Time (HH:MM:SS)",
                 value = time_part,
                 placeholder = "HH:MM:SS")
      )
    )
  )
}

# Helper function to validate datetime format
validate_datetime <- function(datetime_string) {
  if(is.null(datetime_string) || datetime_string == "") {
    return(list(valid = TRUE, message = ""))
  }
  
  tryCatch({
    parsed_date <- as.POSIXct(datetime_string)
    if(is.na(parsed_date)) {
      return(list(valid = FALSE, message = "Invalid datetime format. Please use YYYY-MM-DD HH:MM:SS"))
    }
    return(list(valid = TRUE, message = ""))
  }, error = function(e) {
    return(list(valid = FALSE, message = "Invalid datetime format. Please use YYYY-MM-DD HH:MM:SS"))
  })
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
