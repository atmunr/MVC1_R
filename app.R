library(shiny)
library(shinythemes)

data <- reactiveValues()
data$calib.x <- NULL
data$calib.y <- NULL
data$test.x  <- NULL
data$test.y  <- NULL

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
		if( !is.null( input$calib.x ) ) {
			   data$calib.x <<- read.table( (input$calib.x)$datapath )
		} else data$calib.x <<- NULL
		if( !is.null( input$calib.y ) ) {
			   data$calib.y <<- read.table( (input$calib.y)$datapath )
		} else data$calib.y <<- NULL
		if( !is.null( input$test.x  ) ) {
			    data$test.x <<- read.table( (input$test.x)$datapath  )
		} else  data$test.x <<- NULL
		if( !is.null( input$test.y  ) ) {
			    data$test.y <<- read.table( (input$test.y)$datapath  )
		} else  data$test.y <<- NULL
	})

	observe({
		output$vis.crudo.out <- renderTable(switch(input$vis.crudo,
			'calib.x' = data$calib.x,
			'calib.y' = data$calib.y,
			 'test.x' = data$test.x,
			 'test.y' = data$test.y
		))
	})

	observe({
		vis.grafica.val <- switch(input$vis.grafica,
			'calib.x' = data$calib.x,
			'calib.y' = data$calib.y,
			 'test.x' = data$test.x,
 			 'test.y' = data$test.y)
		if( is.null(vis.grafica.val) ) {
			     output$vis.grafica.out <- NULL
		} else { output$vis.grafica.out <- renderPlot({
			par( xpd = TRUE )
			switch(input$vis.grafica,
				'calib.x' = , 'test.x' = {
					matplot( 1 : nrow(vis.grafica.val), vis.grafica.val,
   						xlab = 'Espectro', ylab = 'Absorbancia',
   						lwd = 1.5, type = 'l' )
   				 	legend( 'bottom', inset = 1,
   						legend = sprintf("%s", seq( 1 : ncol(vis.grafica.val) )),
   					 	horiz = TRUE, fill = c( 1 : ncol(vis.grafica.val) ) ) },
				'calib.y' = , 'test.y' = {
					plot( 1 : nrow(vis.grafica.val), vis.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = c( 1 : nrow(vis.grafica.val) ), pch = 21 )
   				 	legend( 'bottom', inset = 1,
   						legend = sprintf("%s", seq( 1 : nrow(vis.grafica.val) )),
   						horiz = TRUE, fill = c( 1 : nrow(vis.grafica.val) ) ) }
			)
		})}
	})
}

app <- shinyApp(ui = ui, server = server)
