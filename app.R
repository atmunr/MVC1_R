library(shiny)

ui <- fluidPage(
  headerPanel("Calibración Multivariada"),
  sidebarPanel(tabsetPanel(
    tabPanel( "Carga de Datos",
      fileInput("inputXcal" , "Calibración X"),
      fileInput("inputYcal" , "Calibración Y"),
      fileInput("inputXtest", "Prueba X"),
      fileInput("inputYtest", "Prueba Y")
    )
  )),
  mainPanel(
    tabsetPanel(
      tabPanel("Output"),
      tabPanel("Gráficas")
    )
  )
)

server <- function(input, output) {

  data <- reactive({
    Xcalarch  <- input$inputXcal
    Ycalarch  <- input$inputYcal
    Xtestarch <- input$inputXtest
    Ytestarch <- input$inputYtest
  })

}

app <- shinyApp(ui = ui, server = server)
