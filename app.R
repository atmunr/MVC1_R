library(shiny)
library(shinythemes)

ui <- fluidPage( theme = shinytheme("darkly"),

headerPanel("Calibración Multivariada"),

    sidebarPanel(tabsetPanel(

        tabPanel( "Datos de entrada",
            fileInput( "calib.x" , "Calibración X" ),
            fileInput( "calib.y" , "Calibración Y" ),
            fileInput(  "test.x" ,      "Prueba X" ),
            fileInput(  "test.y" ,      "Prueba Y" )
        ),
        tabPanel( "Opciones",
            checkboxInput( "centrar", "Centrar datos" ),
            selectInput( "algoritmo", "Algoritmo:",
                c("PLS" = "pls"
            )),
            numericInput( "num.var.lat", "Variables Latentes",
                value = 1, min = 1, max = 100 ),
            actionButton( "construir.modelo", "Construir modelo" )
        )
    )),
    mainPanel(tabsetPanel(

        tabPanel( "Datos del programa",
            selectInput( "vis.crudo", "Mostrar datos:", c(
                "Calibración X" = "calib.x" ,
                "Calibración Y" = "calib.y" ,
                     "Prueba X" = "test.x"  ,
                     "Prueba Y" = "test.y"

            )),
            tableOutput( "vis.crudo.out" )
        ),
        tabPanel( "Gráficas" )
    ))
)

server <- function( input, output ) {

    observe({
        if( input$vis.crudo == "calib.x" ) { vis.crudo.val.file <- input$calib.x }
        if( input$vis.crudo == "calib.y" ) { vis.crudo.val.file <- input$calib.y }
        if( input$vis.crudo ==  "test.x" ) { vis.crudo.val.file <- input$test.x  }
        if( input$vis.crudo ==  "test.y" ) { vis.crudo.val.file <- input$test.y  }
        if ( is.null(vis.crudo.val.file) ) { vis.crudo.val <- NULL }
        else { vis.crudo.val <- read.table( vis.crudo.val.file$datapath ) }
        output$vis.crudo.out <- renderTable({ vis.crudo.val })
    })

}

app <- shinyApp(ui = ui, server = server)
