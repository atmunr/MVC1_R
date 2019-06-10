library(shiny)
library(shinythemes)
source("PLS.R")

ui <- fluidPage( theme = shinytheme("darkly"),

)
server <- function( input, output ) {

}

app <- shinyApp(ui = ui, server = server)
