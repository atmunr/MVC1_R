library(shiny)
library(shinythemes)
source("PLS.R")

datosEntrada <- reactiveValues()
datosEntrada$calib.x  <- NULL
datosEntrada$calib.y  <- NULL
datosEntrada$prueba.x <- NULL
datosEntrada$prueba.y <- NULL

prePro <- reactiveValues()
prePro$calib.x         <- NULL
prePro$calib.y         <- NULL
prePro$prueba.x        <- NULL
prePro$calib.x.espectroPromedio <- NULL
prePro$calib.y.valorPromedio <- NULL

datosSalida <- reactiveValues()
datosSalida$coeficientesRegresion       <- NULL
datosSalida$press.variablesLatentes     <- NULL
datosSalida$fstat.variablesLatentes     <- NULL
datosSalida$probFstat.variablesLatentes <- NULL
datosSalida$concentracionesPredichas    <- NULL


ui = fluidPage( theme = shinytheme('darkly'),

	headerPanel( 'Calibración Multivariada' ),
	tabsetPanel(
		tabPanel( 'Datos de entrada',
			sidebarPanel(
				fileInput(  'calib.x' , 'Calibración X' ),
            	fileInput(  'calib.y' , 'Calibración Y' ),
            	fileInput( 'prueba.x' ,      'Prueba X' ),
				fileInput( 'prueba.y' ,      'Prueba Y' )
			),
			mainPanel(tabsetPanel(
				tabPanel( 'Datos crudos',
				selectInput( 'datosEntrada.mostrar.crudo', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x',
					 'Prueba Y' = 'prueba.y'
				 )), tableOutput( 'datosEntrada.mostrar.crudo.figura' )
				),
				tabPanel( 'Gráfica',
				selectInput( 'datosEntrada.mostrar.grafica', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x',
					 'Prueba Y' = 'prueba.y'
				)), plotOutput( 'datosEntrada.mostrar.grafica.figura' )
				)
			))
  		),
		tabPanel( 'Preprocesamiento',
			sidebarPanel(
				checkboxInput( 'centrarDatos', 'Centrar Datos' ),
				actionButton( 'preprocesarDatos', 'Actualizar' )
			),
			mainPanel(tabsetPanel(
				tabPanel( 'Datos crudos',
				selectInput( 'prePro.mostrar.crudo', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x'
				)), tableOutput( 'prePro.mostrar.crudo.figura' )
				),
				tabPanel( 'Gráfica',
				selectInput( 'prePro.mostrar.grafica', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x'
				)), plotOutput( 'prePro.mostrar.grafica.figura' )
				)
			))
  		),
		tabPanel( 'Datos de salida',
			sidebarPanel(
				tags$b( 'Construcción del modelo' ),
            	selectInput( 'algoritmo', 'Algoritmo:',
                	c('PLS' = 'pls'
            	)),
            	numericInput( 'numVariablesLatentes', 'Variables Latentes',
                	value = 1, min = 1 ),
            	actionButton( 'construirModelo', 'Construir modelo' ),
				tags$hr(), tags$b( 'Validación del modelo '),
				numericInput( 'numMaxVariablesLatentes', 'Número Máximo de Variables Latentes',
                	value = 1, min = 1 ),
				actionButton( 'validarModelo', 'Validar modelo' )
			),
			mainPanel(tabsetPanel(
				tabPanel( 'Datos crudos',
				selectInput( 'datosSalida.mostrar.crudo', 'Mostrar:', c(
				'Coeficientes de Regresión' =  'coeficientesRegresion',
				'PRESS por Número de Variables Latentes' =  'press.variablesLatentes',
				'Estadística F por Número de Variables Latentes' = 'fstat.variablesLatentes',
				'Probabilidad de la Estadística F por Número de Variables Latentes' = 'probFstat.variablesLatentes',
				'Concentraciones Predichas' = 'concentracionesPredichas'
				)), tableOutput( 'datosSalida.mostrar.crudo.figura' )
				),
				tabPanel( 'Gráfica',
				selectInput( 'datosSalida.mostrar.grafica', 'Mostrar:', c(
				'Coeficientes de Regresión' =  'coeficientesRegresion',
				'PRESS por Número de Variables Latentes' =  'press.variablesLatentes',
				'Estadística F por Número de Variables Latentes' = 'fstat.variablesLatentes',
				'Probabilidad de la Estadística F por Número de Variables Latentes' = 'probFstat.variablesLatentes',
				'Concentraciones Predichas' = 'concentracionesPredichas',
				'Prueba Y' = 'prueba.y'
			)), plotOutput( 'datosSalida.mostrar.grafica.figura' )
				)
			))
  		)
	)
)

server <- function( input, output ) {

	observe({

		if (!is.null( input$calib.x )) {
			   datosEntrada$calib.x  <<- as.matrix(read.table((input$calib.x)$datapath))
		} else datosEntrada$calib.x  <<- NULL

		if (!is.null( input$calib.y )) {
			   datosEntrada$calib.y  <<- as.matrix(read.table((input$calib.y)$datapath))
		} else datosEntrada$calib.y  <<- NULL

		if (!is.null( input$prueba.x )) {
			   datosEntrada$prueba.x <<- as.matrix(read.table((input$prueba.x)$datapath))
		} else datosEntrada$prueba.x <<- NULL

		if (!is.null( input$prueba.y )) {
			   datosEntrada$prueba.y <<- as.matrix(read.table((input$prueba.y)$datapath))
		} else datosEntrada$prueba.y <<- NULL

		output$datosEntrada.mostrar.crudo.figura <- renderTable(
		switch(input$datosEntrada.mostrar.crudo,
			 'calib.x' = datosEntrada$calib.x ,
			 'calib.y' = datosEntrada$calib.y ,
			'prueba.x' = datosEntrada$prueba.x,
			'prueba.y' = datosEntrada$prueba.y
		))

		mostrar.grafica.val <- switch(input$datosEntrada.mostrar.grafica,
			 'calib.x' = datosEntrada$calib.x ,
			 'calib.y' = datosEntrada$calib.y ,
			'prueba.x' = datosEntrada$prueba.x,
 			'prueba.y' = datosEntrada$prueba.y
		)
		if (is.null(mostrar.grafica.val)) {
			      output$datosEntrada.mostrar.grafica.figura <- NULL
		} else {  output$datosEntrada.mostrar.grafica.figura <- renderPlot({
			switch(input$datosEntrada.mostrar.grafica,
				'calib.x' = , 'prueba.x' = {
					matplot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val,
   						xlab = 'Espectro', ylab = 'Absorbancia',
   						lwd = 1.5, type = 'l' )},
				'calib.y' = , 'prueba.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = c( 1 : nrow(mostrar.grafica.val) ), pch = 21 )}
			)
		})}
	})

	observeEvent( input$preprocesarDatos, {

		if (!is.null(datosEntrada$calib.x)) {
			   prePro$calib.x  <<- datosEntrada$calib.x
		} else prePro$calib.x  <<- NULL

		if (!is.null(datosEntrada$calib.y)) {
			   prePro$calib.y  <<- datosEntrada$calib.y
		} else prePro$calib.y  <<- NULL

		if (!is.null(datosEntrada$prueba.x)) {
			   prePro$prueba.x <<- datosEntrada$prueba.x
		} else prePro$prueba.x <<- NULL

		if (input$centrarDatos == TRUE && !is.null(prePro$calib.x)) {

			prePro$calib.x.espectroPromedio <<- matrix( nrow = nrow( prePro$calib.x ) )
			for ( i in 1 : nrow( prePro$calib.x ) ) {
				prePro$calib.x.espectroPromedio[i] <<- mean( prePro$calib.x[i,] )
			}
			for ( i in 1 : ncol(prePro$calib.x ) ) {
				prePro$calib.x[,i] <<- prePro$calib.x[,i] - prePro$calib.x.espectroPromedio
			}

			if (!is.null(prePro$calib.y)) {
				prePro$calib.y.valorPromedio <<- mean(prePro$calib.y)
				prePro$calib.y <<- prePro$calib.y - prePro$calib.y.valorPromedio
			}

			if (!is.null(prePro$prueba.x)) {
				for ( i in 1 : ncol(prePro$prueba.x) ) {
					prePro$prueba.x[,i] <<- prePro$prueba.x[,i] - prePro$calib.x.espectroPromedio
				}
			}
		}
	})

	observe({
		output$prePro.mostrar.crudo.figura <- renderTable(
		switch(input$prePro.mostrar.crudo,
			 'calib.x' = prePro$calib.x ,
			 'calib.y' = prePro$calib.y ,
			'prueba.x' = prePro$prueba.x,
		))

		mostrar.grafica.val <- switch(input$prePro.mostrar.grafica,
			 'calib.x' = prePro$calib.x ,
			 'calib.y' = prePro$calib.y ,
			'prueba.x' = prePro$prueba.x
		)
		if (is.null(mostrar.grafica.val)) {
			      output$prePro.mostrar.grafica.figura <- NULL
		} else {  output$prePro.mostrar.grafica.figura <- renderPlot({
			switch(input$prePro.mostrar.grafica,
				'calib.x' = , 'prueba.x' = {
					matplot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val,
   						xlab = 'Espectro', ylab = 'Absorbancia',
   						lwd = 1.5, type = 'l' )},
				'calib.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = c( 1 : nrow(mostrar.grafica.val) ), pch = 21 )}
			)
		})}
	})

	observeEvent( input$construirModelo, { if
	(!is.null(prePro$calib.x) && !is.null(prePro$calib.y)) {

		datosSalida$coeficientesRegresion <<- CalcularCoefRegrPLS(
			prePro$calib.x, prePro$calib.y, input$numVariablesLatentes
		)

		if (!is.null(prePro$prueba.x)) {
			datosSalida$concentracionesPredichas <<-
				t(prePro$prueba.x) %*% datosSalida$coeficientesRegresion
			if (input$centrarDatos == TRUE) {
				datosSalida$concentracionesPredichas <<- datosSalida$concentracionesPredichas + prePro$calib.y.valorPromedio
			}
		}
	}})

	observeEvent( input$validarModelo, { if
	(!is.null(prePro$calib.x) && !is.null(prePro$calib.y)) {

		if (input$centrarDatos == TRUE) {
 			for ( i in 1 : ncol(prePro$calib.x) ) {
 				prePro$calib.x[,i] <<- prePro$calib.x[,i] + prePro$calib.x.espectroPromedio
 			}
			prePro$calib.y     <<- prePro$calib.y     + prePro$calib.y.valorPromedio
			datosSalida$press.variablesLatentes <<- as.matrix(CalcularPRESSPorNumVarLat(
				prePro$calib.x, prePro$calib.y, input$numMaxVariablesLatentes, centrar.datos = TRUE  ))
 		} else {
			datosSalida$press.variablesLatentes <<- as.matrix(CalcularPRESSPorNumVarLat(
				prePro$calib.x, prePro$calib.y, input$numMaxVariablesLatentes, centrar.datos = FALSE ))
		}

		datosSalida$fstat.variablesLatentes <<- datosSalida$press.variablesLatentes /
			datosSalida$press.variablesLatentes[ length(datosSalida$press.variablesLatentes) ]

		datosSalida$probFstat.variablesLatentes <<- as.matrix(CalcularProbF(
			datosSalida$fstat.variablesLatentes, ncol( prePro$calib.x ), ncol( prePro$calib.x ) ))

	}})

	observe({
		output$datosSalida.mostrar.crudo.figura <- renderTable(
		switch(input$datosSalida.mostrar.crudo,
			 'coeficientesRegresion' = datosSalida$coeficientesRegresion,
			 'press.variablesLatentes' = datosSalida$press.variablesLatentes,
			 'fstat.variablesLatentes' = datosSalida$fstat.variablesLatentes,
			 'probFstat.variablesLatentes' = datosSalida$probFstat.variablesLatentes,
			 'concentracionesPredichas' = datosSalida$concentracionesPredichas,
			 'prueba.y' = datosEntrada$prueba.y
		))

		mostrar.grafica.val <- switch(input$datosSalida.mostrar.grafica,
			'coeficientesRegresion' = datosSalida$coeficientesRegresion,
			'press.variablesLatentes' = datosSalida$press.variablesLatentes,
			'fstat.variablesLatentes' = datosSalida$fstat.variablesLatentes,
			'probFstat.variablesLatentes' = datosSalida$probFstat.variablesLatentes,
			'concentracionesPredichas' = datosSalida$concentracionesPredichas,
			'prueba.y' = datosEntrada$prueba.y
		)
		if (is.null(mostrar.grafica.val)) {
			      output$datosSalida.mostrar.grafica.figura <- NULL
		} else {  output$datosSalida.mostrar.grafica.figura <- renderPlot({
			switch(input$datosSalida.mostrar.grafica,
				'coeficientesRegresion' = , 'press.variablesLatentes' = ,
				'fstat.variablesLatentes' = , 'probFstat.variablesLatentes' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
	   					xlab = 'Espectro', ylab = 'Valor de coeficiente',
	   					lwd = 1.5, type = 'l'
					)},
				'concentracionesPredichas' = , 'prueba.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = c( 1 : nrow(mostrar.grafica.val) ), pch = 21
					)}
			)
		})}
	})

}

app <- shinyApp(ui = ui, server = server)
