# link to google maps showing direction from origin to destination
actionButton_google_direction <- function(inputId, df){
  
  origin <- reactive({ paste0(df[1,c("origin_lat")], ", ", df[1,c("origin_lng")]) })
  dest <- reactive({ paste0(df[1,c("dest_lat")], ", ", df[1,c("dest_lng")]) })
  
  url <- paste0("https://www.google.com/maps/dir/", origin(), "/", dest())
  
  actionButton(inputId=inputId, 
               label="Get directions in Google Maps", 
               value = "Open popup",
               onclick =paste0("window.open('",url,"','_blank')"))
}

# link to google maps showing origin or destination locations
actionButton_google_place <- function(inputId, label, df, lat_var_name, long_var_name){
  
  latlong <- reactive({ paste0(df[1,c(lat_var_name)], ", ", df[1,c(long_var_name)]) })
  
  url <- paste0("https://www.google.com/maps/place/", latlong())
  
  actionButton(inputId=inputId, 
               label=label, 
               value = "Open popup",
               onclick =paste0("window.open('",url,"','_blank')"))
}