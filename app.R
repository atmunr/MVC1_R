library(shiny)

ui <- fluidPage(
  titlePanel("Calibración Multivariada")
)

server <- function(input, output){}

app <- shinyApp(ui = ui, server = server)
