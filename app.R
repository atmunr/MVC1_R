library(shiny)

ui <- fluidPage(
  headerPanel("Calibración Multivariada"),
  sidebarPanel(tabsetPanel(
    tabPanel( "Carga de Datos",
      fileInput("input_Xcal" , "Calibración X"),
      fileInput("input_Ycal" , "Calibración Y"),
      fileInput("input_Xtest", "Prueba X"),
      fileInput("input_Ytest", "Prueba Y"),
      checkboxInput("input_centrar", "Centrar datos")
    )
  )),
  mainPanel(
    tabsetPanel(
      tabPanel("Output",
        textOutput("output_centrar")
      ),
      tabPanel("Gráficas")
    )
  )
)

server <- function(input, output) {

  data <- reactive({
    Xcalarch  <- input$input_Xcal
    Ycalarch  <- input$input_Ycal
    Xtestarch <- input$input_Xtest
    Ytestarch <- input$input_Ytest
    Xcal   <- as.matrix(read.table( Xcalarch  )) # matriz de calibración X
    Ycal   <- as.matrix(read.table( Ycalarch  )) # matriz de calibración Y
    Xtest  <- as.matrix(read.table( Xtestarch )) # matriz de prueba X
    Ytest  <- as.matrix(read.table( Ytestarch )) # matriz de prueba X

    centrar <- input$input_centrar
  })

}

app <- shinyApp(ui = ui, server = server)
