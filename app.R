library(shiny)
library(shinythemes)
source("PLS.R")

 # datos ingresados al programa
datosEntrada <- reactiveValues()
datosEntrada$calib.x  <- NULL # espectros de calibración
datosEntrada$calib.y  <- NULL # concentraciones de calibración
datosEntrada$prueba.x <- NULL # espectros de prueba
datosEntrada$prueba.y <- NULL # concentraciones de prueba

 # datos ingresados, luego de ser preprocesados
prePro <- reactiveValues()
prePro$calib.x         <- NULL # espectros de calibración preprocesados
prePro$calib.y         <- NULL # concentraciones de calibración preprocesadas
prePro$prueba.x        <- NULL # espectros de prueba preprocesados
 # si se centran los datos, el promedio de los espectros de calibración
prePro$calib.x.espectroPromedio <- NULL
 # si se centran los datos, el promedio de las concentraciones de calibración
prePro$calib.y.valorPromedio <- NULL

 # resultados de la calibración multivariada
datosSalida <- reactiveValues()
datosSalida$coeficientesRegresion       <- NULL # coeficientes de regresión
 # error estadístico PRESS para cada número de variables latentes<
datosSalida$press.variablesLatentes     <- NULL
 # estadística F producida con los valores PRESS para cada número de variables latentes
datosSalida$fstat.variablesLatentes     <- NULL
 # probabilidad de obtener cada estadística F
datosSalida$probFstat.variablesLatentes <- NULL
 # las concentraciones que predice la calibración multivariada
datosSalida$concentracionesPredichas    <- NULL

#defición de la interfaz gráfica
ui = fluidPage( theme = shinytheme('darkly'),

	headerPanel( 'Calibración Multivariada' ),
	tabsetPanel(

		# sección de ingreso de datos
		tabPanel( 'Datos de entrada',
			sidebarPanel( # ingreso de archivos de entrada
				fileInput(  'calib.x' , 'Calibración X' ),
            	fileInput(  'calib.y' , 'Calibración Y' ),
            	fileInput( 'prueba.x' ,      'Prueba X' ),
				fileInput( 'prueba.y' ,      'Prueba Y' )
			),
			mainPanel(tabsetPanel( # mostrar datos en forma de tabla
				tabPanel( 'Datos crudos',
				selectInput( 'datosEntrada.mostrar.crudo', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x',
					 'Prueba Y' = 'prueba.y'
				 )), tableOutput( 'datosEntrada.mostrar.crudo.figura' )
				),
				tabPanel( 'Gráfica', # mostrar datos como una gráfica
				selectInput( 'datosEntrada.mostrar.grafica', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x',
					 'Prueba Y' = 'prueba.y'
				)), plotOutput( 'datosEntrada.mostrar.grafica.figura' )
				)
			))
  		),

		# sección de preprocesamiento de datos
		tabPanel( 'Preprocesamiento',
			sidebarPanel(
				# elección de métodos de preprocesado
				checkboxInput( 'centrarDatos', 'Centrar Datos' ),
				checkboxInput( 'procesarSavitzkyGolay', 'Suavizado Savitzky Golay' ),
				numericInput( 'procesarSavitzkyGolay.ordenDerivada', 'Orden de derivada', min = 1, max = 3, value = 1 ),
				numericInput( 'procesarSavitzkyGolay.gradoPolinomio', 'Grado del polinomio', min = 1, max = 5, value = 1 ),
				numericInput( 'procesarSavitzkyGolay.largoVentana', 'Largo de la ventana', min = 1, max = 9, value = 1 ),
				checkboxInput( 'procesarMSC', 'Corrección de Esparcimiento Multiplicativo' ),
				# botón para realizar el preprocesamieto
				actionButton( 'preprocesarDatos', 'Actualizar' )
			),
			mainPanel(tabsetPanel(
				 # mostrar datos procesados en forma de tabla
				tabPanel( 'Datos crudos',
				selectInput( 'prePro.mostrar.crudo', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x'
				)), tableOutput( 'prePro.mostrar.crudo.figura' )
				),
				 # mostrar datos procesados en forma de gráfica
				tabPanel( 'Gráfica',
				selectInput( 'prePro.mostrar.grafica', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x'
				)), plotOutput( 'prePro.mostrar.grafica.figura' )
				)
			))
  		),

		# sección de construcción y validación del modelo y predicción
		tabPanel( 'Datos de salida',
			sidebarPanel(
				tags$b( 'Construcción del modelo' ),
				 # elección del algoritmo
            	selectInput( 'algoritmo', 'Algoritmo:',
                	c('PLS' = 'pls'
            	)),
				# elección del número de variables latentes
            	numericInput( 'numVariablesLatentes', 'Variables Latentes',
                	value = 1, min = 1 ),
				# construcción del modelo
            	actionButton( 'construirModelo', 'Construir modelo' ),
				tags$hr(), tags$b( 'Validación del modelo '),
				# elección del número máximo de variables latentes para la
				# validación
				numericInput( 'numMaxVariablesLatentes', 'Número Máximo de Variables Latentes',
                	value = 1, min = 1 ),
				# validación del modelo
				actionButton( 'validarModelo', 'Validar modelo' )
			),
			mainPanel(tabsetPanel(
				 # mostrar resultados de predicción en forma de tabla
				tabPanel( 'Datos crudos',
				selectInput( 'datosSalida.mostrar.crudo', 'Mostrar:', c(
				'Coeficientes de Regresión' =  'coeficientesRegresion',
				'PRESS por Número de Variables Latentes' =  'press.variablesLatentes',
				'Estadística F por Número de Variables Latentes' = 'fstat.variablesLatentes',
				'Probabilidad de la Estadística F por Número de Variables Latentes' = 'probFstat.variablesLatentes',
				'Concentraciones Predichas' = 'concentracionesPredichas',
				'Prueba Y' = 'prueba.y'
				)), tableOutput( 'datosSalida.mostrar.crudo.figura' )
				),
				 # mostrar resultados de predicción en forma de gráfica
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
  		),
		# estadísticas sobre la calidad de la predicción
		tabPanel( 'Estadísticas',
			sidebarPanel(
				tags$b('Lorem ipsum dolor sit amet, consectetur adipiscing elit,
				sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.'),
				tags$hr(),
				tags$b('Ut enim ad minim veniam, quis nostrud exercitation ullamco
				laboris nisi ut aliquip ex ea commodo consequat.'),
				tags$hr(),
				tags$b('Duis aute irure dolor in reprehenderit in voluptate velit
				esse cillum dolore eu fugiat nulla pariatur.'),
				tags$hr(),
				tags$b('Excepteur sint occaecat cupidatat non proident, sunt in
				culpa qui officia deserunt mollit anim id est laborum.')
			),
			mainPanel(#tabsetPanel(
				 # mostrar estadísticas en forma de tabla
				#tabPanel( 'Datos crudos',
				#selectInput( 'estad.mostrar.crudo', 'Mostrar:', c(
				#
				#)), tableOutput( 'estad.mostrar.crudo.figura' )
				#),
				 # mostrar estadísticas en forma de gráfica
				#tabPanel( 'Gráfica',
 				selectInput( 'estad.mostrar.grafica', 'Mostrar:', c(
 				'Concentraciones esperadas y predichas' = 'prueba.y.vs.concentPred'
				)), plotOutput( 'estad.mostrar.grafica' )
 				#)
			)#)
  		)
	)
)

# defición del código de servidor
server <- function( input, output ) {

	# carga de datos de entrada y
	# creación de sus visualizaciones
	observe({
		# asignación de valores para los datos de entrada:
		# leer los archivos de entrada y convertirlos de tablas a matrices
		# para facilitar el cómputo
		# si no se ingresó ningún archivo, el valor correspondiente será NULL

		if (!is.null( input$calib.x )) { # espectros de calibración
			   datosEntrada$calib.x  <<- as.matrix(read.table((input$calib.x)$datapath))
		} else datosEntrada$calib.x  <<- NULL

		if (!is.null( input$calib.y )) { # concentraciones de calibración
			   datosEntrada$calib.y  <<- as.matrix(read.table((input$calib.y)$datapath))
		} else datosEntrada$calib.y  <<- NULL

		if (!is.null( input$prueba.x )) { # espectros de prueba
			   datosEntrada$prueba.x <<- as.matrix(read.table((input$prueba.x)$datapath))
		} else datosEntrada$prueba.x <<- NULL

		if (!is.null( input$prueba.y )) { # concentraciones de prueba
			   datosEntrada$prueba.y <<- as.matrix(read.table((input$prueba.y)$datapath))
		} else datosEntrada$prueba.y <<- NULL

		# visualizacion en tabla:
		# con la opción elegida en el widget datosEntrada.mostrar.crudo
		# se elige el valor correspondiente para pasar a renderTable()
		# y se lo asigna a datosEntrada.mostrar.crudo.figura
		output$datosEntrada.mostrar.crudo.figura <- renderTable(
		switch(input$datosEntrada.mostrar.crudo,
			 'calib.x' = datosEntrada$calib.x ,
			 'calib.y' = datosEntrada$calib.y ,
			'prueba.x' = datosEntrada$prueba.x,
			'prueba.y' = datosEntrada$prueba.y
		))

		# visualizacion en gráfica:
		# con la opción elegida en el widget datosEntrada.mostrar.grafica
		# se elige el valor correspondiente y se lo asigna a
		# datosEntrada.mostrar.grafica.val, para luego pasarlo a renderPlot()
		mostrar.grafica.val <- switch(input$datosEntrada.mostrar.grafica,
			 'calib.x' = datosEntrada$calib.x ,
			 'calib.y' = datosEntrada$calib.y ,
			'prueba.x' = datosEntrada$prueba.x,
 			'prueba.y' = datosEntrada$prueba.y
		)
		# caso especial: si no existe el valor que corresponde a la opción
		# elegida (el valor es igual a NULL), asigna directamente a
		# datosEntrada.mostrar.grafica.figura el valor NULL, para evitar llamar
		# a renderPlot(NULL) (no le gusta)
		if (is.null(mostrar.grafica.val)) {
			      output$datosEntrada.mostrar.grafica.figura <- NULL
		# llama a renderPlot(datosEntrada.mostrar.grafica.val) y se lo asigna a
		# datosEntrada.mostrar.grafica.figura
		} else {  output$datosEntrada.mostrar.grafica.figura <- renderPlot({
			switch(input$datosEntrada.mostrar.grafica,
				# llama a matplot() directamente, dado que los espectros se
				# almacenan como columnas en una matriz
				'calib.x' = , 'prueba.x' = {
					matplot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val,
   						xlab = 'Espectro', ylab = 'Absorbancia',
   						lwd = 1.5, type = 'l' ) },
				# como las concentraciones se almacenan en una matriz (a pesar
				# de ser una sola columna) se llama a la función plot sobre
				# la primer columna
				'calib.y' = , 'prueba.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = 'black', pch = 20, cex = 1.3 )
					lines(mostrar.grafica.val) }
			)
		})}
	})

	# preprocesamiento de los datos de entrada
	observeEvent( input$preprocesarDatos, {

		# existen los datos preprocesados si se cargaron los datos necesarios
		# por defecto, calib.x preprocesado es calib.x
		if (!is.null(datosEntrada$calib.x)) {
			   prePro$calib.x  <<- datosEntrada$calib.x
		} else prePro$calib.x  <<- NULL
		# por defecto, calib.y preprocesada es calib.y
		if (!is.null(datosEntrada$calib.y)) {
			   prePro$calib.y  <<- datosEntrada$calib.y
		} else prePro$calib.y  <<- NULL
		# por defecto, prueba.x preprocesado es prueba.x
		if (!is.null(datosEntrada$prueba.x)) {
			   prePro$prueba.x <<- datosEntrada$prueba.x
		} else prePro$prueba.x <<- NULL

		if (input$procesarMSC == TRUE) {
			if (!is.null(prePro$calib.x) && !is.null(prePro$prueba.x)) {
				outMSC <- ProcesarCorreccionEsparcimientoMult(
					prePro$calib.x, prePro$prueba.x)
				prePro$calib.x  <<- outMSC[[1]]
				prePro$prueba.x <<- outMSC[[2]]
			}
		}

		if (input$procesarSavitzkyGolay == TRUE) {
			if (!is.null(prePro$calib.x)) {
				prePro$calib.x <<- ProcesarSavitzkyGolay( prePro$calib.x,
					input$procesarSavitzkyGolay.ordenDerivada,
					input$procesarSavitzkyGolay.gradoPolinomio,
					input$procesarSavitzkyGolay.largoVentana)
			}
			if (!is.null(prePro$prueba.x)) {
				prePro$prueba.x <<- ProcesarSavitzkyGolay( prePro$prueba.x,
					input$procesarSavitzkyGolay.ordenDerivada,
					input$procesarSavitzkyGolay.gradoPolinomio,
					input$procesarSavitzkyGolay.largoVentana)
			}
		}

		# centrado de los datos
		# (sólo se puede hacer algún centrado si existen los espectros de calibración)
		if (input$centrarDatos == TRUE && !is.null(prePro$calib.x)) {
			# calcula el promedio de los espectros de calibración y lo guarda
			# como una matrix columna
			prePro$calib.x.espectroPromedio <<- matrix( nrow = nrow( prePro$calib.x ) )
			for ( i in 1 : nrow( prePro$calib.x ) ) {
				prePro$calib.x.espectroPromedio[i] <<- mean( prePro$calib.x[i,] )
			}

			# centrado de los espectros de calibración:
			# restarle el espectro promedio a todos los espectros de calibración
			for ( i in 1 : ncol(prePro$calib.x ) ) {
				prePro$calib.x[,i] <<- prePro$calib.x[,i] - prePro$calib.x.espectroPromedio
			}

			# centrado de las concentraciones de calibración:
			# calcular el promedio de todas las concentraciones de calibración
			# y restárselo a todas las concentraciones de calibración
			if (!is.null(prePro$calib.y)) {
				prePro$calib.y.valorPromedio <<- mean(prePro$calib.y)
				prePro$calib.y <<- prePro$calib.y - prePro$calib.y.valorPromedio
			}

			# centrado de los espectros de prueba:
			# restarle el espectro promedio a todos los espectros de prueba
			if (!is.null(prePro$prueba.x)) {
				for ( i in 1 : ncol(prePro$prueba.x) ) {
					prePro$prueba.x[,i] <<- prePro$prueba.x[,i] - prePro$calib.x.espectroPromedio
				}
			}
		}

	})

	# visualizaciones de los datos preprocesados
	observe({
		# visualizacion en tabla:
		# con la opción elegida en el widget prePro.mostrar.crudo
		# se elige el valor correspondiente para pasar a renderTable()
		# y se lo asigna a prePro.mostrar.crudo.figura
		output$prePro.mostrar.crudo.figura <- renderTable(
		switch(input$prePro.mostrar.crudo,
			 'calib.x' = prePro$calib.x ,
			 'calib.y' = prePro$calib.y ,
			'prueba.x' = prePro$prueba.x,
		))
		# visualizacion en gráfica:
		# con la opción elegida en el widget prePro.mostrar.grafica
		# se elige el valor correspondiente y se lo asigna a
		# prePro.mostrar.grafica.val, para luego pasarlo a renderPlot()
		mostrar.grafica.val <- switch(input$prePro.mostrar.grafica,
			 'calib.x' = prePro$calib.x ,
			 'calib.y' = prePro$calib.y ,
			'prueba.x' = prePro$prueba.x
		)
		# caso especial: si no existe el valor que corresponde a la opción
		# elegida (el valor es igual a NULL), asigna directamente a
		# prePro.mostrar.grafica.figura el valor NULL, para evitar llamar
		# a renderPlot(NULL) (no le gusta)
		if (is.null(mostrar.grafica.val)) {
			      output$prePro.mostrar.grafica.figura <- NULL
		} else {  output$prePro.mostrar.grafica.figura <- renderPlot({
			switch(input$prePro.mostrar.grafica,
				# llama a matplot() directamente, dado que los espectros se
				# almacenan como columnas en una matriz
				'calib.x' = , 'prueba.x' = {
					matplot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val,
   						xlab = 'Espectro', ylab = 'Absorbancia',
   						lwd = 1.5, type = 'l' ) },
				# como las concentraciones se almacenan en una matriz (a pesar
				# de ser una sola columna) se llama a la función plot sobre
				# la primer columna
				'calib.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = 'black', pch = 20, cex = 1.3 )
					lines(mostrar.grafica.val) }
			)
		})}
	})

	# construcción del modelo con los datos de preprocesados y
	# predicción de las concentraciones:
	# los coeficientes de regresión se pueden calcular sólo si están definidos
	# calib.x y calib.y, y
	# las concentraciones predichas sólo si existen calib.x, calib.y y prueba.x
	observeEvent( input$construirModelo, { if
	(!is.null(prePro$calib.x) && !is.null(prePro$calib.y)) {
		# obtiene los coeficientes como una matriz columna
		datosSalida$coeficientesRegresion <<- CalcularCoefRegrPLS(
			prePro$calib.x, prePro$calib.y, input$numVariablesLatentes
		)
		# obtiene las concentraciones de predicción
		if (!is.null(prePro$prueba.x)) {
			# multiplica cada espectro de prueba por los coeficientes para
			# obtener las concentraciones
			datosSalida$concentracionesPredichas <<-
				t(prePro$prueba.x) %*% datosSalida$coeficientesRegresion
			# si se centraron los datos, se tiene que decentralizar las
			# concentraciones de prediccón sumándoles el promedio de las
			# concentraciones de prueba
			if (input$centrarDatos == TRUE) {
				datosSalida$concentracionesPredichas <<- datosSalida$concentracionesPredichas + prePro$calib.y.valorPromedio
			}
		}
	}})

	# validación del modelo:
	# sólo si están definidos calib.x y calib.y
	observeEvent( input$validarModelo, { if
	(!is.null(prePro$calib.x) && !is.null(prePro$calib.y)) {
		# si se centraron los datos, se tienen que decentralizar antes de
		# realizar la validacíon del modelo
		calib.x <- prePro$calib.x
		calib.y <- prePro$calib.y
		if (input$centrarDatos == TRUE) {
			# decentraliza calib.x
 			for ( i in 1 : ncol(prePro$calib.x) ) {
 				calib.x[,i] <- prePro$calib.x[,i] + prePro$calib.x.espectroPromedio
 			}
			# decentraliza calib.y
			calib.y <- prePro$calib.y + prePro$calib.y.valorPromedio
 		}
		# calcula los errores PRESS por número de variables latentes
		datosSalida$press.variablesLatentes <<- as.matrix(CalcularPRESSPorNumVarLat(
			calib.x, calib.y, input$numMaxVariablesLatentes, centrar = input$centrarDatos  ))
		# calcula las estadísticas F producidas con los valores PRESS
		datosSalida$fstat.variablesLatentes <<- datosSalida$press.variablesLatentes /
			datosSalida$press.variablesLatentes[ length(datosSalida$press.variablesLatentes) ]
		# calcula las probabilidades de obtener cada estadística F
		datosSalida$probFstat.variablesLatentes <<- as.matrix(CalcularProbF(
			datosSalida$fstat.variablesLatentes, ncol( calib.x ), ncol( calib.x ) ))

	}})

	# visualizaciones de los datos obtenidos por la validación del modelo
	observe({
		# visualizacion en tabla:
		# con la opción elegida en el widget datosSalida.mostrar.crudo
		# se elige el valor correspondiente para pasar a renderTable()
		# y se lo asigna a datosSalida.mostrar.crudo.figura
		output$datosSalida.mostrar.crudo.figura <- renderTable(
		switch(input$datosSalida.mostrar.crudo,
			 'coeficientesRegresion' = datosSalida$coeficientesRegresion,
			 'press.variablesLatentes' = datosSalida$press.variablesLatentes,
			 'fstat.variablesLatentes' = datosSalida$fstat.variablesLatentes,
			 'probFstat.variablesLatentes' = datosSalida$probFstat.variablesLatentes,
			 'concentracionesPredichas' = datosSalida$concentracionesPredichas,
			 'prueba.y' = datosEntrada$prueba.y
		))
		# visualizacion en gráfica:
		# con la opción elegida en el widget datosSalida.mostrar.grafica
		# se elige el valor correspondiente y se lo asigna a
		# datosSalida.mostrar.grafica.val, para luego pasarlo a renderPlot()
		mostrar.grafica.val <- switch(input$datosSalida.mostrar.grafica,
			'coeficientesRegresion' = datosSalida$coeficientesRegresion,
			'press.variablesLatentes' = datosSalida$press.variablesLatentes,
			'fstat.variablesLatentes' = datosSalida$fstat.variablesLatentes,
			'probFstat.variablesLatentes' = datosSalida$probFstat.variablesLatentes,
			'concentracionesPredichas' = datosSalida$concentracionesPredichas,
			'prueba.y' = datosEntrada$prueba.y
		)
		# caso especial: si no existe el valor que corresponde a la opción
		# elegida (el valor es igual a NULL), asigna directamente a
		# datosSalida.mostrar.grafica.figura el valor NULL, para evitar llamar
		# a renderPlot(NULL) (no le gusta)
		if (is.null(mostrar.grafica.val)) {
			      output$datosSalida.mostrar.grafica.figura <- NULL
		} else {  output$datosSalida.mostrar.grafica.figura <- renderPlot({
			switch(input$datosSalida.mostrar.grafica,
				# como todos estos valores se almacenan en matrices (a pesar
				# de ser sólo columnas) se llama a la función plot sobre
				# la primer columna de cada uno
				'coeficientesRegresion' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
	   					xlab = 'Número', ylab = 'Valor',
	   					lwd = 1.5, type = 'l' ) },
				'press.variablesLatentes' = , 'fstat.variablesLatentes' = ,
				'probFstat.variablesLatentes' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
	   					xlab = 'Número', ylab = 'Valor',
					bg = 'black', pch = 20, cex = 1.3 )
					lines(mostrar.grafica.val) },
				'concentracionesPredichas' = , 'prueba.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Contenido',
   						bg = 'black', pch = 20, cex = 1.3 )
					lines(mostrar.grafica.val) }
			)
		})}
	})

	# visualizaciones de los datos estadísticos sobre la calidad de la prediccón
	observe({
		if (input$estad.mostrar.grafica == 'prueba.y.vs.concentPred') {
			if (
				is.null( datosSalida$concentracionesPredichas) ||
			    is.null(datosEntrada$prueba.y)
			) {    output$estad.mostrar.grafica <- NULL }
			else { output$estad.mostrar.grafica <- renderPlot({
				plot( datosSalida$concentracionesPredichas, datosEntrada$prueba.y,
 					xlab = 'Concentraciones Predichas', ylab = 'Valores Nominales',
				 	bg = 'black', pch = 20, cex = 1.3 )
				lines(1 : length(datosSalida$concentracionesPredichas))
			})}
		}
	})

}

# crea la aplicación shiny con la interfaz gráfica y el servidor
app <- shinyApp(ui = ui, server = server)
