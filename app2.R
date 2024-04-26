library(shiny)
library(DTedit)
library(psrc.travelsurvey)
library(bslib)
library(shinyjs)

##### Create the Shiny server
server <- function(input, output) {
  
  trip_data_17_19 <- get_hhts("2017_2019", "t", vars=c("trip_id","driver","mode_1",'dest_purpose_cat', 'origin_purpose_cat',
                                                       "google_duration", 'trip_path_distance')) 
  mydata <- trip_data_17_19 %>% 
    dplyr::filter(household_id == 17100024) %>% 
    dplyr::select(c(13,2:8))
  mydata$mode_1 <- factor(mydata$mode_1, levels=unique(trip_data_17_19$mode_1))
  
  ##### Callback functions.
  my.insert.callback <- function(data, row) {
    mydata <- rbind(data, mydata)
    return(mydata)
  }
  
  my.update.callback <- function(data, olddata, row) {
    mydata[row,] <- data[1,]
    return(mydata)
  }
  
  my.delete.callback <- function(data, row) {
    mydata <- mydata[-row,]
    return(mydata)
  }
  
  ##### Create the DTedit object
  DTedit::dtedit_server(
    id = 'HTS_2023',
    thedata = mydata,
    edit.cols = c('driver', 'mode_1', 'dest_purpose_cat', 'origin_purpose_cat','google_duration','trip_path_distance'),
    edit.label.cols = c('Driver', 'Transport Mode', 'Destination Purpose', 'Origin Purpose', 'Travel Time', 'Travel Distance'),
    callback.update = my.update.callback,
    callback.insert = my.insert.callback,
    callback.delete = my.delete.callback)
}

##### Create the shiny UI
ui <- fluidPage(
  
  navbarPage(HTML("New Fixie"),
             theme = bs_theme(bootswatch = "united",
                              base_font = font_google("Poppins"))
             ),
  # 
  # dtedit_ui('HTS_2023')
  
  # Give the page a title
  # titlePanel("New Fixie"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      # selectInput("region", "Region:", 
      #             choices=colnames(WorldPhones)),
      hr(),
      helpText("Data from AT&T (1961) The World's Telephones."), 
      width = 3
    ),
    
    # Create a spot for the barplot
    mainPanel(
      dtedit_ui('HTS_2023') , 
      width = 9
    )
    
  )
)

##### Start the shiny app
shinyApp(ui = ui, server = server)
