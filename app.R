library(shiny)

ui <- fluidPage(
  headerPanel("Calibración Multivariada"),
  sidebarPanel(tabsetPanel(
    tabPanel("Datos del programa",
      fileInput("Xcal" , "Calibración X"),
      fileInput("Ycal" , "Calibración Y"),
      fileInput("Xtest", "Prueba X"),
      fileInput("Ytest", "Prueba Y")
    ),
    tabPanel("Opciones",
      checkboxInput("centrar", "Centrar datos"),
      selectInput("algoritmo", "Algoritmo:",
        c("PLS" = "PLS")),
      numericInput("nvarlat", "Variables Latentes",
        value = 1, min = 1, max = 100),
      actionButton("construirmodelo", "Construir modelo")
    )
  )),
  mainPanel(
    tabsetPanel(
      tabPanel("Output",
      selectInput("showraw", "Mostrar datos:", c(
        "Calibración X" = "Xcal",
        "Calibración Y" = "Ycal",
        "Prueba X"      = "Xtest",
        "Prueba Y"      = "Ytest"
        )),
        tableOutput("matriz")
      ),
      tabPanel("Gráficas")
    )
  )
)

server <- function(input, output, session) {

  observe({
    if( input$showraw == "Xcal"  ) { frawmatrix <- input$Xcal  }
    if( input$showraw == "Ycal"  ) { frawmatrix <- input$Ycal  }
    if( input$showraw == "Xtest" ) { frawmatrix <- input$Xtest }
    if( input$showraw == "Ytest" ) { frawmatrix <- input$Ytest }
    if ( is.null(frawmatrix) ) { rawmatrix <- NULL }
    else { rawmatrix <- read.table( frawmatrix$datapath ) }
    output$matriz <- renderTable({ rawmatrix })
  })

}

app <- shinyApp(ui = ui, server = server)
