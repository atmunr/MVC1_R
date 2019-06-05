library(shiny)
library(shinythemes)
source("PLS.R")

data <- reactiveValues()
data$calib.x <- NULL
data$calib.y <- NULL
data$test.x  <- NULL
data$test.y  <- NULL
data$calib.x.cent <- NULL
data$calib.x.prom <- NULL
data$calib.y.cent <- NULL
data$calib.y.prom <- NULL
data$num.var.lat <- NULL
data$coef.regr <- NULL
data$test.y.pred <- NULL

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
            checkboxInput( "centrar.datos", "Centrar datos" ),
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
                     "Prueba Y" = "test.y"  ,
				"Calibración X Centrado" = "calib.x.cent" ,
				"Calibración Y Centrado" = "calib.y.cent" ,
				"Coeficientes de regresión" = 'coef.regr',
				"Valores Y Predichos" = 'test.y.pred'
            )), tableOutput( "vis.crudo.out" )
        ),
        tabPanel( "Gráficas",
			selectInput( "vis.grafica", "Mostrar datos:", c(
				"Calibración X" = "calib.x" ,
				"Calibración Y" = "calib.y" ,
					 "Prueba X" = "test.x"  ,
					 "Prueba Y" = "test.y"  ,
				"Calibración X Centrado" = "calib.x.cent" ,
	 			"Calibración Y Centrado" = "calib.y.cent" ,
				"Coeficientes de regresión" = 'coef.regr',
				"Valores Y Predichos" = 'test.y.pred'
			)), plotOutput( "vis.grafica.out" )
		)
    ))
)

server <- function( input, output ) {

	observe({
		if( !is.null( input$calib.x ) ) {
			   data$calib.x <<- as.matrix(read.table( (input$calib.x)$datapath ))
		} else data$calib.x <<- NULL
		if( !is.null( input$calib.y ) ) {
			   data$calib.y <<- as.matrix(read.table( (input$calib.y)$datapath ))
		} else data$calib.y <<- NULL
		if( !is.null( input$test.x  ) ) {
			    data$test.x <<- as.matrix(read.table( (input$test.x)$datapath  ))
		} else  data$test.x <<- NULL
		if( !is.null( input$test.y  ) ) {
			    data$test.y <<- as.matrix(read.table( (input$test.y)$datapath  ))
		} else  data$test.y <<- NULL

		if( input$centrar.datos == TRUE ) {
			if( !is.null(data$calib.x) ) {
				datos_centrados_X <- CentrarMatriz2DPorColumnas( data$calib.x )
				data$calib.x.cent <<- datos_centrados_X[[1]]
				data$calib.x.prom <<- datos_centrados_X[[2]]
			}
			if( !is.null(data$calib.y) ) {
				datos_centrados_Y <- CentrarVector( data$calib.y )
				data$calib.y.cent <<- datos_centrados_Y[[1]]
				data$calib.y.prom <<- datos_centrados_Y[[2]]
			}
		} else {
			data$calib.x.cent <<- NULL
			data$calib.y.cent <<- NULL
		}

		data$num.var.lat <<- input$num.var.lat
	})

	observe({
		output$vis.crudo.out <- renderTable(switch(input$vis.crudo,
			'calib.x' = data$calib.x,
			'calib.y' = data$calib.y,
			 'test.x' = data$test.x,
			 'test.y' = data$test.y,
			 'calib.x.cent' = data$calib.x.cent,
			 'calib.y.cent' = data$calib.y.cent,
			 'coef.regr' = data$coef.regr,
			 'test.y.pred' = data$test.y.pred
		))

		vis.grafica.val <- switch(input$vis.grafica,
			'calib.x' = data$calib.x,
			'calib.y' = data$calib.y,
			 'test.x' = data$test.x,
 			 'test.y' = data$test.y,
			'calib.x.cent' = data$calib.x.cent,
			'calib.y.cent' = data$calib.y.cent,
			'coef.regr' = data$coef.regr,
			'test.y.pred' = data$test.y.pred)
		if( is.null(vis.grafica.val) ) {
			     output$vis.grafica.out <- NULL
		} else { output$vis.grafica.out <- renderPlot({
			par( xpd = TRUE )
			switch(input$vis.grafica,
				'calib.x' = , 'calib.x.cent' = , 'test.x' = {
					matplot( 1 : nrow(vis.grafica.val), vis.grafica.val,
   						xlab = 'Espectro', ylab = 'Absorbancia',
   						lwd = 1.5, type = 'l' )
   				 	legend( 'bottom', inset = 1,
   						legend = sprintf("%s", seq( 1 : ncol(vis.grafica.val) )),
   					 	horiz = TRUE, fill = c( 1 : ncol(vis.grafica.val) ) ) },
				'calib.y' = , 'calib.y.cent' = , 'test.y' = , 'test.y.pred' = {
					plot( 1 : nrow(vis.grafica.val), vis.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = c( 1 : nrow(vis.grafica.val) ), pch = 21 )
   				 	legend( 'bottom', inset = 1,
   						legend = sprintf("%s", seq( 1 : nrow(vis.grafica.val) )),
   						horiz = TRUE, fill = c( 1 : nrow(vis.grafica.val) ) ) },
				'coef.regr' = {
					matplot( 1 : nrow(vis.grafica.val), vis.grafica.val,
   						xlab = 'Espectro', ylab = 'Valor de coeficiente',
   						lwd = 1.5, type = 'l' )
				}
			)
		})}
	})

	observeEvent(input$construir.modelo, {
		if( input$centrar.datos == TRUE ) {
			   data$coef.regr <<- CalcularCoefRegrPLS(
				   data$calib.x.cent, data$calib.y.cent, data$num.var.lat )
		} else {
			data$coef.regr <<- CalcularCoefRegrPLS(
				data$calib.x, data$calib.y, data$num.var.lat )
		}

		data$test.y.pred <<- t( data$test.x ) %*% data$coef.regr
		if( input$centrar.datos == TRUE ) {
			data$test.y.pred <<- data$test.y.pred + data$calib.y.prom
		}
	})

}

app <- shinyApp(ui = ui, server = server)
