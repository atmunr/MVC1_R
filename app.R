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

            )), tableOutput( "vis.crudo.out" )
        ),
        tabPanel( "Gráficas",
			selectInput( "vis.grafica", "Mostrar datos:", c(
				"Calibración X" = "calib.x" ,
				"Calibración Y" = "calib.y" ,
					 "Prueba X" = "test.x"  ,
					 "Prueba Y" = "test.y"
			)), plotOutput( "vis.grafica.out" )
		)
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

	observe({
        if( input$vis.grafica == "calib.x" ) { vis.grafica.val.file <- input$calib.x }
        if( input$vis.grafica == "calib.y" ) { vis.grafica.val.file <- input$calib.y }
        if( input$vis.grafica ==  "test.x" ) { vis.grafica.val.file <- input$test.x  }
        if( input$vis.grafica ==  "test.y" ) { vis.grafica.val.file <- input$test.y  }
        if ( is.null(vis.grafica.val.file) ) { vis.grafica.val <- NULL }
		else { vis.grafica.val <- read.table( vis.grafica.val.file$datapath ) }
		output$vis.grafica.out <- renderPlot({

			if( input$vis.grafica == "calib.x" || input$vis.grafica == "test.x" ) {
				matplot( 1 : nrow(vis.grafica.val), vis.grafica.val,
					xlab = 'Espectro', ylab = 'Absorbancia',
					lwd = 1.5, type = 'l' )
				par( xpd = TRUE )
				legend( 'bottom', inset = 1,
					legend = sprintf("%s", seq( 1 : ncol(vis.grafica.val) )),
					horiz = TRUE, fill = c( 1 : ncol(vis.grafica.val) ) )
			}
			if( input$vis.grafica == "calib.y" || input$vis.grafica == "test.y" ) {
				plot( 1 : nrow(vis.grafica.val), vis.grafica.val[,1],
					xlab = 'N° de Muestra', ylab = 'Contenido',
			 		bg = c( 1 : nrow(vis.grafica.val) ), pch = 21 )
				par( xpd = TRUE )
				legend( 'bottom', inset = 1,
					legend = sprintf("%s", seq( 1 : nrow(vis.grafica.val) )),
					horiz = TRUE, fill = c( 1 : nrow(vis.grafica.val) ) )
			}

		})
    })

}

app <- shinyApp(ui = ui, server = server)
