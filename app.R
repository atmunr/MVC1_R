library(stringr)
library(shiny)
library(shinythemes)
source("calibpred.R")
source("preprocesar.R")

 # datos ingresados al programa
INPUT <- reactiveValues()
INPUT$calib.x  <- NULL # espectros de calibrado
INPUT$calib.y  <- NULL # concentraciones de calibrado
INPUT$prueba.x <- NULL # espectros de prueba
INPUT$prueba.y <- NULL # concentraciones de prueba

 # datos ingresados, luego de ser preprocesados
PREPRO <- reactiveValues()
PREPRO$calib.x         <- NULL # espectros de calibrado preprocesados
PREPRO$calib.y         <- NULL # concentraciones de calibrado preprocesadas
PREPRO$prueba.x        <- NULL # espectros de prueba preprocesados
 # si se centran los datos, el promedio de los espectros de calibrado
PREPRO$calib.x.especProm <- NULL
 # si se centran los datos, el promedio de las concentraciones de calibrado
PREPRO$calib.y.concentProm <- NULL

 # resultados de la calibrado multivariada
OUTPUT <- reactiveValues()
OUTPUT$coefRegr       <- NULL # coeficientes de regresión
 # error ESTADístico PRESS para cada número de variables latentes
OUTPUT$press.nvl     <- NULL
 # ESTADística F producida con los valores PRESS para cada número de variables latentes
OUTPUT$fstat.nvl     <- NULL
 # probabilidad de obtener cada ESTADística F
OUTPUT$probFstat.nvl <- NULL
 # las concentraciones que predice la calibrado multivariada
OUTPUT$concentPred    <- NULL

# defición de la interfaz gráfica
ui <- fluidPage( theme = shinytheme('darkly'),

	headerPanel( 'Calibración Multivariada' ),
	tabsetPanel(

		# ingreso de datos, elección de sensores y eleminación de muestras
		tabPanel( 'Datos de entrada',
			sidebarPanel(
				# ingresar de archivos de entrada
				fileInput( 'calib.x'  , 'Calibración X' ),
            	fileInput( 'calib.y'  , 'Calibración Y' ),
            	fileInput( 'prueba.x' ,      'Prueba X' ),
				fileInput( 'prueba.y' ,      'Prueba Y' ),
				# elegir de sensores
				textInput( 'INPUT.elegirSensores', 'Sensores' ),
				# quitar muestras
				textInput( 'INPUT.quitarMuestras', 'Quitar muestras' ),
				# aplicar cambios
				actionButton('INPUT.aplicar', 'Actualizar' )
			),
			mainPanel(tabsetPanel( # mostrar datos en forma de tabla
				tabPanel( 'Datos crudos',
				selectInput( 'INPUT.mostrar.crudo', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x',
					 'Prueba Y' = 'prueba.y'
				 )), tableOutput( 'INPUT.mostrar.crudo.figura' )
				),
				tabPanel( 'Gráfica', # mostrar datos como una gráfica
				selectInput( 'INPUT.mostrar.grafica', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x',
					 'Prueba Y' = 'prueba.y'
				)), plotOutput( 'INPUT.mostrar.grafica.figura' )
				)
			))
  		),

		# sección de preprocesamiento de datos
		tabPanel( 'Preprocesamiento',
			sidebarPanel(
				# elegir de algoritmos de preprocesamiento
				checkboxInput( 'PREPRO.centrar', 'Centrar Datos' ),
				checkboxInput( 'PREPRO.SavitzkyGolay', 'Suavizado/derivadas (Algoritmo Savitzky Golay)' ),
				numericInput( 'PREPRO.SavitzkyGolay.ord', 'Orden de derivada',
					min = 0, max = 2, value = 0 ),
				numericInput( 'PREPRO.SavitzkyGolay.grad', 'Grado del polinomio',
					min = 0, max = 5, value = 1 ),
				numericInput( 'PREPRO.SavitzkyGolay.vlen', 'Largo de la ventana',
					min = 0, max = 9, value = 1 ),
				checkboxInput( 'PREPRO.MSC', 'Corrección de Esparcimiento Multiplicativo (MSC)' ),
				# aplicar cambios
				actionButton( 'PREPRO.aplicar', 'Actualizar' )
			),
			mainPanel(tabsetPanel(
				 # mostrar datos procesados en forma de tabla
				tabPanel( 'Datos crudos',
				selectInput( 'PREPRO.mostrar.crudo', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x'
				)), tableOutput( 'PREPRO.mostrar.crudo.figura' )
				),
				 # mostrar datos procesados en forma de gráfica
				tabPanel( 'Gráfica',
				selectInput( 'PREPRO.mostrar.grafica', 'Mostrar:', c(
				'Calibración X' =  'calib.x',
				'Calibración Y' =  'calib.y',
					 'Prueba X' = 'prueba.x'
				)), plotOutput( 'PREPRO.mostrar.grafica.figura' )
				)
			))
  		),

		# sección de construcción y validación del modelo y predicción
		tabPanel( 'Datos de salida',
			sidebarPanel(
				tags$b( 'Construcción del modelo' ),
				 # elección del algoritmo
            	selectInput( 'OUTPUT.pred.alg', 'Algoritmo:',
                	c('PLS-1' = 'PLS1'
            	)),
				# elección del número de variables latentes
            	numericInput( 'OUTPUT.nvl', 'Variables Latentes',
                	value = 1, min = 1 ),
				# construcción del modelo
            	actionButton( 'OUTPUT.construirModelo', 'Construir modelo' ),
				# validación del modelo
				tags$hr(), tags$b( 'Validación Cruzada'),
				# elección del método de validación
				selectInput( 'OUTPUT.valid.alg', 'Técnica de Validacón:',
					c('Leave one out' = 'LOO'
				)),
				# elección del número máximo de variables latentes para la
				# validación
				numericInput( 'OUTPUT.nvl.max', 'Número Máximo de Variables Latentes',
                	value = 1, min = 1 ),
				# validación del modelo
				actionButton( 'OUTPUT.validarModelo', 'Validar modelo' )
			),
			mainPanel(tabsetPanel(
				 # mostrar resultados de predicción en forma de tabla
				tabPanel( 'Datos crudos',
				selectInput( 'OUTPUT.mostrar.crudo', 'Mostrar:', c(
				'Coeficientes de Regresión' = 'coefRegr',
				'PRESS por Número de Variables Latentes' =  'press.nvl',
				'Estadística F por Número de Variables Latentes' = 'fstat.nvl',
				'Probabilidad de la Estadística F por Número de Variables Latentes' = 'probFstat.nvl',
				'Concentraciones Predichas' = 'concentPred',
				'Prueba Y' = 'prueba.y'
				)), tableOutput( 'OUTPUT.mostrar.crudo.figura' )
				),
				 # mostrar resultados de predicción en forma de gráfica
				tabPanel( 'Gráfica',
				selectInput( 'OUTPUT.mostrar.grafica', 'Mostrar:', c(
				'Coeficientes de Regresión' =  'coefRegr',
				'PRESS por Número de Variables Latentes' =  'press.nvl',
				'Estadística F por Número de Variables Latentes' = 'fstat.nvl',
				'Probabilidad de la Estadística F por Número de Variables Latentes' = 'probFstat.nvl',
				'Concentraciones Predichas' = 'concentPred',
				'Prueba Y' = 'prueba.y'
				)), plotOutput( 'OUTPUT.mostrar.grafica.figura' )
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
			mainPanel(
 				selectInput( 'ESTAD.mostrar.grafica', 'Mostrar:', c(
 				'Concentraciones esperadas y predichas' = 'prueba.y.vs.concentPred'
				)), plotOutput( 'ESTAD.mostrar.grafica' )
			)
  		)
	)
)

# defición del código de servidor
server <- function( input, output ) {

	# definición de datos ingresados a la herramienta:
	observeEvent( input$INPUT.aplicar, {

		# cargar archivos
		if (!is.null( input$calib.x )) {
			   INPUT$calib.x  <<- as.matrix(read.table((input$calib.x)$datapath))
		} else INPUT$calib.x  <<- NULL

		if (!is.null( input$calib.y )) {
			   INPUT$calib.y  <<- as.matrix(read.table((input$calib.y)$datapath))
		} else INPUT$calib.y  <<- NULL

		if (!is.null( input$prueba.x )) {
			   INPUT$prueba.x <<- as.matrix(read.table((input$prueba.x)$datapath))
		} else INPUT$prueba.x <<- NULL

		if (!is.null( input$prueba.y )) {
			   INPUT$prueba.y <<- as.matrix(read.table((input$prueba.y)$datapath))
		} else INPUT$prueba.y <<- NULL

		# toma un string con intervalos de números y, si son válidos, los
		# convierte a un vector de pares de números. Si no, devuelve NULL.
		# verifica que los intervalos representan un subconjunto de valores
		# del rango [1,N]
		procesarIntervalos <- function( intervalos, nmax ) {
			# verificar que solo hayan números o espacios
			if (grepl("^[0-9 ]+$", intervalos) == FALSE) return(NULL)
			# quitar espacios al principio y fin y unir espacios consecutivos en uno
			intervalos <- gsub("\\s+", " ", str_trim(intervalos))
			# obtener un vector de strings
			intervalos <- strsplit(intervalos, split = " ")
			intervalos <- intervalos[[1]]
			# verificar que haya un número par de valores
			if (length(intervalos) %% 2 != 0) return(NULL)
			# obtener un vector de números
			intervalos <- strtoi(intervalos)
			# verificar que sean subintervalos del rango [1,N]
			if (min(intervalos) < 1 || max(intervalos) > nmax) return(NULL)
			# obtener un vector de pares de valores
			intervalos <- split(intervalos, ceiling(seq_along(intervalos)/2))
			return(intervalos)
		}

		# quitar sensores y muestras en espectros de calibrado
		if (!is.null(INPUT$calib.x)) {
			if (input$INPUT.elegirSensores != "") {
				sensores <- procesarIntervalos(input$INPUT.elegirSensores, nrow(INPUT$calib.x))
				if (is.null(sensores)) INPUT$calib.x <<- NULL
				else INPUT$calib.x  <<- PrePro.FiltrarSensores(INPUT$calib.x, sensores)
			}
			if (input$INPUT.quitarMuestras != "") {
				muestras <- procesarIntervalos(input$INPUT.quitarMuestras, ncol(INPUT$calib.x))
				if (is.null(muestras)) INPUT$calib.x <<- NULL
				else INPUT$calib.x  <<- PrePro.QuitarMuestras(INPUT$calib.x, muestras)
			}
		}

		# quitar sensores y muestras en espectros de prueba
		if (!is.null(INPUT$prueba.x)) {
			if (input$INPUT.elegirSensores != "") {
				sensores <- procesarIntervalos(input$INPUT.elegirSensores, nrow(INPUT$prueba.x))
				if (is.null(sensores)) INPUT$prueba.x <<- NULL
				else INPUT$prueba.x  <<- PrePro.FiltrarSensores(INPUT$prueba.x, sensores)
			}
			if (input$INPUT.quitarMuestras != "") {
				muestras <- procesarIntervalos(input$INPUT.quitarMuestras, ncol(INPUT$prueba.x))
				if (is.null(muestras)) INPUT$prueba.x <<- NULL
				else INPUT$prueba.x  <<- PrePro.QuitarMuestras(INPUT$prueba.x, muestras)
			}
		}

	})

	# visualizaciones de los datos ingresados
	observe({
		# visualizacion en tabla:
		# con la opción elegida en el widget INPUT.mostrar.crudo
		# se elige el valor correspondiente para pasar a renderTable()
		# y se lo asigna a INPUT.mostrar.crudo.figura
		output$INPUT.mostrar.crudo.figura <- renderTable(
		switch(input$INPUT.mostrar.crudo,
			 'calib.x' = INPUT$calib.x ,
			 'calib.y' = INPUT$calib.y ,
			'prueba.x' = INPUT$prueba.x,
			'prueba.y' = INPUT$prueba.y
		))

		# visualizacion en gráfica:
		# con la opción elegida en el widget INPUT.mostrar.grafica
		# se elige el valor correspondiente y se lo asigna a
		# INPUT.mostrar.grafica.val, para luego pasarlo a renderPlot()
		mostrar.grafica.val <- switch(input$INPUT.mostrar.grafica,
			 'calib.x' = INPUT$calib.x ,
			 'calib.y' = INPUT$calib.y ,
			'prueba.x' = INPUT$prueba.x,
 			'prueba.y' = INPUT$prueba.y
		)
		# caso especial: si no existe el valor que corresponde a la opción
		# elegida (el valor es igual a NULL), asigna directamente a
		# INPUT.mostrar.grafica.figura el valor NULL, para evitar llamar
		# a renderPlot(NULL) (no le gusta)
		if (is.null(mostrar.grafica.val)) {
			      output$INPUT.mostrar.grafica.figura <- NULL
		# llama a renderPlot(INPUT.mostrar.grafica.val) y se lo asigna a
		# INPUT.mostrar.grafica.figura
		} else {  output$INPUT.mostrar.grafica.figura <- renderPlot({
			switch(input$INPUT.mostrar.grafica,
				# llama a matplot() directamente, dado que los espectros se
				# almacenan como columnas en una matriz
				'calib.x' = , 'prueba.x' = {
					matplot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val,
   						xlab = 'N° de Sensor', ylab = 'Absorbancia',
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
	observeEvent( input$PREPRO.aplicar, {

		# existen los datos preprocesados si se cargaron los datos necesarios
		# por defecto, calib.x preprocesado es calib.x
		if (!is.null(INPUT$calib.x)) {
			   PREPRO$calib.x  <<- INPUT$calib.x
		} else PREPRO$calib.x  <<- NULL
		# por defecto, calib.y preprocesada es calib.y
		if (!is.null(INPUT$calib.y)) {
			   PREPRO$calib.y  <<- INPUT$calib.y
		} else PREPRO$calib.y  <<- NULL
		# por defecto, prueba.x preprocesado es prueba.x
		if (!is.null(INPUT$prueba.x)) {
			   PREPRO$prueba.x <<- INPUT$prueba.x
		} else PREPRO$prueba.x <<- NULL

		if (input$PREPRO.MSC == TRUE) {
			if (!is.null(PREPRO$calib.x) && !is.null(PREPRO$prueba.x)) {
				outMSC <- PrePro.MSC(PREPRO$calib.x, PREPRO$prueba.x)
				PREPRO$calib.x  <<- outMSC[[1]]
				PREPRO$prueba.x <<- outMSC[[2]]
			}
		}

		if (input$PREPRO.SavitzkyGolay == TRUE) {
			if (!is.null(PREPRO$calib.x)) {
				PREPRO$calib.x <<- PrePro.SavitzkyGolay( PREPRO$calib.x,
					input$PREPRO.SavitzkyGolay.ord,
					input$PREPRO.SavitzkyGolay.grad,
					input$PREPRO.SavitzkyGolay.vlen)
			}
			if (!is.null(PREPRO$prueba.x)) {
				PREPRO$prueba.x <<- PrePro.SavitzkyGolay( PREPRO$prueba.x,
					input$PREPRO.SavitzkyGolay.ord,
					input$PREPRO.SavitzkyGolay.grad,
					input$PREPRO.SavitzkyGolay.vlen)
			}
		}

		# centrado de los datos
		# (sólo se puede hacer algún centrado si existen los espectros de calibrado)
		if (input$PREPRO.centrar == TRUE && !is.null(PREPRO$calib.x)) {
			# calcula el promedio de los espectros de calibrado y lo guarda
			# como una matrix columna
			PREPRO$calib.x.especProm <<- matrix(nrow = nrow(PREPRO$calib.x))
			for (i in 1 : nrow(PREPRO$calib.x)) {
				PREPRO$calib.x.especProm[i] <<- mean(PREPRO$calib.x[i,])
			}

			# centrado de los espectros de calibrado:
			# restarle el espectro promedio a todos los espectros de calibrado
			for (i in 1 : ncol(PREPRO$calib.x)) {
				PREPRO$calib.x[,i] <<- PREPRO$calib.x[,i] - PREPRO$calib.x.especProm
			}

			# centrado de las concentraciones de calibrado:
			# calcular el promedio de todas las concentraciones de calibrado
			# y restárselo a todas las concentraciones de calibrado
			if (!is.null(PREPRO$calib.y)) {
				PREPRO$calib.y.concentProm <<- mean(PREPRO$calib.y)
				PREPRO$calib.y <<- PREPRO$calib.y - PREPRO$calib.y.concentProm
			}

			# centrado de los espectros de prueba:
			# restarle el espectro promedio a todos los espectros de prueba
			if (!is.null(PREPRO$prueba.x)) {
				for (i in 1 : ncol(PREPRO$prueba.x)) {
					PREPRO$prueba.x[,i] <<- PREPRO$prueba.x[,i] - PREPRO$calib.x.especProm
				}
			}
		}

	})

	# visualizaciones de los datos preprocesados
	observe({
		# visualizacion en tabla:
		# con la opción elegida en el widget PREPRO.mostrar.crudo
		# se elige el valor correspondiente para pasar a renderTable()
		# y se lo asigna a PREPRO.mostrar.crudo.figura
		output$PREPRO.mostrar.crudo.figura <- renderTable(
		switch(input$PREPRO.mostrar.crudo,
			 'calib.x' = PREPRO$calib.x ,
			 'calib.y' = PREPRO$calib.y ,
			'prueba.x' = PREPRO$prueba.x,
		))
		# visualizacion en gráfica:
		# con la opción elegida en el widget PREPRO.mostrar.grafica
		# se elige el valor correspondiente y se lo asigna a
		# PREPRO.mostrar.grafica.val, para luego pasarlo a renderPlot()
		mostrar.grafica.val <- switch(input$PREPRO.mostrar.grafica,
			 'calib.x' = PREPRO$calib.x ,
			 'calib.y' = PREPRO$calib.y ,
			'prueba.x' = PREPRO$prueba.x
		)
		# caso especial: si no existe el valor que corresponde a la opción
		# elegida (el valor es igual a NULL), asigna directamente a
		# PREPRO.mostrar.grafica.figura el valor NULL, para evitar llamar
		# a renderPlot(NULL) (no le gusta)
		if (is.null(mostrar.grafica.val)) {
			      output$PREPRO.mostrar.grafica.figura <- NULL
		} else {  output$PREPRO.mostrar.grafica.figura <- renderPlot({
			switch(input$PREPRO.mostrar.grafica,
				# llama a matplot() directamente, dado que los espectros se
				# almacenan como columnas en una matriz
				'calib.x' = , 'prueba.x' = {
					matplot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val,
   						xlab = 'N° de Sensor', ylab = 'Absorbancia',
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
	# predicción de las concentraciones
	observeEvent( input$OUTPUT.construirModelo, {
		if (!is.null(PREPRO$calib.x) && !is.null(PREPRO$calib.y)) {
		# obtiene los coeficientes como una matriz columna
		OUTPUT$coefRegr <<- CalcularCoefRegr.PLS1(
			PREPRO$calib.x, PREPRO$calib.y, input$OUTPUT.nvl)
		# obtiene las concentraciones de predicción
		if (!is.null(PREPRO$prueba.x)) {
			# multiplica cada espectro de prueba por los coeficientes para
			# obtener las concentraciones
			OUTPUT$concentPred <<- t(PREPRO$prueba.x) %*% OUTPUT$coefRegr
			# si se centraron los datos, se tiene que decentralizar las
			# concentraciones de prediccón sumándoles el promedio de las
			# concentraciones de prueba
			if (input$PREPRO.centrar == TRUE) {
				OUTPUT$concentPred <<- OUTPUT$concentPred + PREPRO$calib.y.concentProm
			}
		}
	}})

	# validación del modelo:
	# sólo si están definidos calib.x y calib.y
	observeEvent( input$OUTPUT.validarModelo, { if
	(!is.null(PREPRO$calib.x) && !is.null(PREPRO$calib.y)) {
		# si se centraron los datos, se tienen que decentralizar antes de
		# realizar la validacíon del modelo
		calib.x <- PREPRO$calib.x
		calib.y <- PREPRO$calib.y
		if (input$PREPRO.centrar == TRUE) {
			# decentraliza calib.x
 			for ( i in 1 : ncol(PREPRO$calib.x) ) {
 				calib.x[,i] <- PREPRO$calib.x[,i] + PREPRO$calib.x.especProm
 			}
			# decentraliza calib.y
			calib.y <- PREPRO$calib.y + PREPRO$calib.y.concentProm
 		}
		# calcula los errores PRESS por número de variables latentes
		OUTPUT$press.nvl <<- as.matrix(ValidarModelo.LOO.PLS1(
			calib.x, calib.y, input$OUTPUT.nvl.max,
			centrar = input$PREPRO.centrar))
		# calcula las ESTADísticas F producidas con los valores PRESS
		OUTPUT$fstat.nvl <<- OUTPUT$press.nvl / OUTPUT$press.nvl[ length(OUTPUT$press.nvl) ]
		# calcula las probabilidades de obtener cada ESTADística F
		OUTPUT$probFstat.nvl <<- as.matrix(CalcularProbF(
			OUTPUT$fstat.nvl, ncol(calib.x), ncol(calib.x)))

	}})

	# visualizaciones de los datos obtenidos por la validación del modelo
	observe({
		# visualizacion en tabla:
		# con la opción elegida en el widget OUTPUT.mostrar.crudo
		# se elige el valor correspondiente para pasar a renderTable()
		# y se lo asigna a OUTPUT.mostrar.crudo.figura
		output$OUTPUT.mostrar.crudo.figura <- renderTable(
		switch(input$OUTPUT.mostrar.crudo,
			 'coefRegr' = OUTPUT$coefRegr,
			 'press.nvl' = OUTPUT$press.nvl,
			 'fstat.nvl' = OUTPUT$fstat.nvl,
			 'probFstat.nvl' = OUTPUT$probFstat.nvl,
			 'concentPred' = OUTPUT$concentPred,
			 'prueba.y' = INPUT$prueba.y
		))
		# visualizacion en gráfica:
		# con la opción elegida en el widget OUTPUT.mostrar.grafica
		# se elige el valor correspondiente y se lo asigna a
		# OUTPUT.mostrar.grafica.val, para luego pasarlo a renderPlot()
		mostrar.grafica.val <- switch(input$OUTPUT.mostrar.grafica,
			'coefRegr' = OUTPUT$coefRegr,
			'press.nvl' = OUTPUT$press.nvl,
			'fstat.nvl' = OUTPUT$fstat.nvl,
			'probFstat.nvl' = OUTPUT$probFstat.nvl,
			'concentPred' = OUTPUT$concentPred,
			'prueba.y' = INPUT$prueba.y
		)
		# caso especial: si no existe el valor que corresponde a la opción
		# elegida (el valor es igual a NULL), asigna directamente a
		# OUTPUT.mostrar.grafica.figura el valor NULL, para evitar llamar
		# a renderPlot(NULL) (no le gusta)
		if (is.null(mostrar.grafica.val)) {
			      output$OUTPUT.mostrar.grafica.figura <- NULL
		} else {  output$OUTPUT.mostrar.grafica.figura <- renderPlot({
			switch(input$OUTPUT.mostrar.grafica,
				# como todos estos valores se almacenan en matrices (a pesar
				# de ser sólo columnas) se llama a la función plot sobre
				# la primer columna de cada uno
				'coefRegr' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
	   					xlab = 'N° de Sensor', ylab = 'Valor de Coeficiente',
	   					lwd = 1.5, type = 'l' ) },
				'press.nvl' = , 'fstat.nvl' = ,
				'probFstat.nvl' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
	   					xlab = 'N° de Variables Latenes', ylab = 'Valor',
					bg = 'black', pch = 20, cex = 1.3 )
					lines(mostrar.grafica.val) },
				'concentPred' = , 'prueba.y' = {
					plot( 1 : nrow(mostrar.grafica.val), mostrar.grafica.val[,1],
   						xlab = 'N° de Muestra', ylab = 'Concentración',
   						bg = 'black', pch = 20, cex = 1.3 )
					lines(mostrar.grafica.val) }
			)
		})}
	})

	# visualizaciones de los datos estadísticos sobre la calidad de la prediccón
	observe({
		if (input$ESTAD.mostrar.grafica == 'prueba.y.vs.concentPred') {
			if ( is.null( OUTPUT$concentPred) || is.null(INPUT$prueba.y)) {
				   output$ESTAD.mostrar.grafica <- NULL }
			else { output$ESTAD.mostrar.grafica <- renderPlot({
				plot( OUTPUT$concentPred, INPUT$prueba.y,
 					xlab = 'Valores Nominales', ylab = 'Valores Predichos',
				 	bg = 'black', pch = 20, cex = 1.3 )
				lines(1 : length(OUTPUT$concentPred))
			})}
		}
	})

}

# crea la aplicación shiny con la interfaz gráfica y el servidor
app <- shinyApp(ui = ui, server = server)
