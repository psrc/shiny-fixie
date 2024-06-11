# DTedit Shiny Demo
library(shiny)
library(DTedit)

##### Create the Shiny server
server <- function(input, output, session) {
  
  dtedit(input, output,
         name = 'books',
         thedata = data.frame(
           Buy = c('Tea', 'Biscuits', 'Apples'),
           Quantity = c(7, 2, 5),
           stringsAsFactors = FALSE
         ))
  
}

##### Create the shiny UI
ui <- fluidPage(
  h3('Books'),
  uiOutput('books')
)

shinyApp(ui = ui, server = server)