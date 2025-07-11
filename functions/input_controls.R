# get list of unique values for a variable
get_var_value_list <- function(var_name, value_labels = codebook){
  
  val_list <- value_labels %>% filter(variable == var_name) %>%
    rowwise() %>%
    mutate(value_label = paste0(value, ": ", final_label)) %>%
    ungroup() %>%
    select(value, value_label)
  
  return(setNames(as.list(val_list$value), nm = val_list$value_label))
}

# dropdown for single selection
selectInputSingle <- function(inputId, df, label_name = NULL, ...) {
  
  var_str <- gsub("^.*-", "", inputId)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  selectInput(inputId,
              label = label_name,
              choices = get_var_value_list(var_name=var_str), 
              selected = df[1,c(var_str)], 
              selectize = TRUE,
              ...)
}

# text input
textInputSimple <- function(inputId, df, label_name = NULL, ...){
  
  var_str <- gsub("^.*-", "", inputId)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  textInput(inputId, 
            label = label_name, 
            value = df[1,c(var_str)],
            ...)
}

# number input
numericInputSimple <- function(inputId, df, label_name = NULL, min = NA, max = NA, ...){
  
  var_str <- gsub("^.*-", "", inputId)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  numericInput(inputId,
               label = label_name,
               value = df[1,c(var_str)],
               min = min, max = max,
               ...)
  
}

# datetime input - specialized for timestamp columns with proper date/time controls
dateTimeInput <- function(inputId, df, label_name = NULL){
  
  rval <- reactiveValues(datetime_string = NULL)
  
  var_str <- gsub("^.*-", "", inputId)
  if(is.null(label_name)){
    label_name = var_str
  }
  
  # Get the datetime value and parse it properly
  datetime_val <- df[1, c(var_str)]
  
  parsed_datetime <- as.POSIXct(datetime_val)
  # Truncate to seconds to match database precision
  parsed_datetime <- as.POSIXct(format(parsed_datetime, "%Y-%m-%d %H:%M:%S"))
  
  # Extract date and time components
  date_part <- strftime(parsed_datetime, format="%Y-%m-%d")
  time_part <- strftime(parsed_datetime, format="%H:%M:%S")
  
  # Create a container with both date and time inputs
  div(
    style = "margin-bottom: 15px;",
    tags$label(class = "control-label", label_name),
    br(),
    fluidRow(
      column(4,
             dateInput(inputId = paste0(inputId, "_date"),
                       label = "Date",
                       value = date_part,
                       format = "yyyy-mm-dd")
      ),
      column(8,
             timeInput(paste0(inputId, "_time"), "Time", value = hms::as_hms(time_part))
      )
    )
  )
}
