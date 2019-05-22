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
    Xcal   = as.matrix(read.table( Xcalarch  )) # matriz de calibración X
    Ycal   = as.matrix(read.table( Ycalarch  )) # matriz de calibración Y
    Xtest  = as.matrix(read.table( Xtestarch )) # matriz de prueba X
    Ytest  = as.matrix(read.table( Ytestarch )) # matriz de prueba X

  })

}

app <- shinyApp(ui = ui, server = server)
