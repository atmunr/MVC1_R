library(shiny)

ui <- fluidPage(
  headerPanel("Calibración Multivariada"),
  sidebarPanel(
    titlePanel("Carga de Datos")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Output"),
      tabPanel("Gráficas")
    )
  )
)

server <- function(input, output){}

app <- shinyApp(ui = ui, server = server)
