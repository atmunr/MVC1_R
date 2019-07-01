# centra un vector
CentrarVector <- function( vector ) {
	vector.prom = mean(vector)
	vector.cent = vector - vector.prom
	return(list(vector.cent, vector.prom))
}

# centra una matriz 2D por columnas
CentrarMatriz2DPorColumnas <- function( matriz ) {
	# columna promedio de M: el i-ésimo valor de column_prom es el promedio de
	# los valores de la i-ésima fila de M
	colprom <- matrix(nrow = nrow(matriz))
	for (i in 1 : nrow(matriz)) {
		colprom[i] <- mean(matriz[i,])
	}
	# matriz centrada: a cada columna se le resta la columna promedio
	matriz.cent <- matrix(nrow = nrow(matriz), ncol = ncol(matriz))
	for (i in 1 : ncol(matriz.cent)) {
		matriz.cent[,i] <- matriz[,i] - colprom
	}
	return(list(matriz.cent, colprom))
}

# produce coeficientes de regresión
# para unas muestras de calibrado y un número variables latentes
# usando el modelo PLS
# calib.x : espectros de calibración
# calib.y : concentraciones de calibración
# nvl     : número de variables latentes
CalcularCoefRegrPLS <- function( calib.x, calib.y, nvl ) {

	w = matrix( ncol = nrow(calib.x) )
	v = matrix( ncol = nvl           )
	W = matrix( ncol = nvl, nrow = nrow(calib.x) )
	T = matrix( ncol = nvl, nrow = ncol(calib.x) )
	P = matrix( ncol = nvl, nrow = nrow(calib.x) )

	for (i in 1 : nvl) {
		w       <- calib.x %*% calib.y / as.numeric( t( calib.y ) %*% calib.y )
		W[,i]   <- w / as.numeric(sqrt( t(w) %*% w ))
		T[,i]   <- t( calib.x ) %*% W[,i]
		v[i]    <- t(T[,i]) %*% calib.y / as.numeric(t(T[,i]) %*% T[,i])
		P[,i]   <- calib.x %*% T[,i] / as.numeric(t(T[,i]) %*% T[,i])
		calib.x <- calib.x - P[,i] %*% t(T[,i])
		calib.y <- calib.y - v[i] * T[,i]
	}

	coef.regr <- W %*% solve(t(P) %*% W) %*% t(v)
	return( coef.regr ) # coeficientes de regresión

}

# realiza una validación cruzada con el modelo PLS
# devuelve los errores PRESS para cada número de variables latentes
# calib.x : espectros de calibración
# calib.y : concentraciones de calibración
# nvl.max : número máximo de variables latentes
# centrar : booleano para decidir si centrar los datos o no en la validación
CalcularPRESSPorNumVarLat <- function( calib.x, calib.y, nvl.max, centrar ) {

	# salida, lista de errores PRESS, de uno a var_lat_max
	press.vals <- numeric( nvl.max )

	for     (nvl in 1 :      nvl.max ) { # para todos los números de variables latentes
		for (  i in 1 : ncol(calib.x)) { # para todas las muestras

			# se aisla la i-ésima muestra
		 	espect.ais  <- calib.x[, i] # espectro aislado
			calib.x.p   <- calib.x[,-i] # espectros sin el aislado
			concent.ais <- calib.y[  i] # concentración aislada
			calib.y.p   <- calib.y[ -i] # concentraciones sin la asilada

			if (centrar == TRUE) { # se centran los datos
				datos_centrados_X <- CentrarMatriz2DPorColumnas(calib.x.p)
				calib.x.p         <- datos_centrados_X[[1]] # espectros centrados
				calib.x.p.prom    <- datos_centrados_X[[2]] # espectro promedio
				datos_centrados_Y <- CentrarVector(calib.y.p)
				calib.y.p         <- datos_centrados_Y[[1]] # concentraciones centradas
				calib.y.p.prom    <- datos_centrados_Y[[2]] # concentración promedio
				espect.ais  <- espect.ais  - calib.x.p.prom
				concent.ais <- concent.ais - calib.y.p.prom
			}

			# se crea un modelo PLS con los datos, se intenta predecir el valor de la
			# muestra aislada y se suma el error cuadrado al PRESS para este número de
			# variables latentes
			coef.regr <- as.vector(CalcularCoefRegrPLS(calib.x.p, calib.y.p, nvl))
			concent.ais.pred <- espect.ais %*% coef.regr
			press.vals[nvl]  <- press.vals[nvl] + ((concent.ais.pred - concent.ais)^2)

		}
	}

	return( press.vals )

}

# devuelve la probablidad asociada a la estadística F
# en función de F y los grados de libertad k1 y k2
CalcularProbF <- function( f, k1, k2 ) {
	d1 <- 0.0498673470
	d2 <- 0.0211410061
	d3 <- 0.0032776263
	d4 <- 0.0000380036
	d5 <- 0.0000488906
	d6 <- 0.0000053830
	f3 <- f ^ (1 / 3)
	k8 <- 2 / (9 * k1)
	k9 <- 2 / (9 * k2)
	z  <- ((1 - k9) * f3 - 1 + k8) / sqrt((k9 * f3 * f3) + k8)
	if ( k2 <= 3 ) z <- z * (1 + 0.08 * z * (z / k2) ^ 3)
	z9   <- abs(z)
	z9d6 <- z9 * d6
	p <- 1 + z9 * (d1 + z9 * (d2 + z9 * (d3 + z9 * (d4 + z9 * (d5 + z9d6)))))
	p <- 0.5 / (p ^ 16)
	p <- (sign(z) * (p - 0.5)) + 0.5
	p <- round(p * 1000 + 0.5) / 1000
	return(1 - p)
}

# estima el número óptimo de variables latentes a usar
# por validación cruzada y PLS
# calib.x : espectros de calibración
# calib.y : concentraciones de calibración
# nvl.max : número máximo de variables latentes
# centrar : booleano que decide si centrar o no los datos
CalcularNumOptVarLat <- function( calib.x, calib.y, nvl.max, centrar ) {

	# errores PRESS
	press.vals <- CalcularPRESSPorNumVarLat(calib.x, calib.y, nvl.max, centrar)

	# PRESS / min(PRESS) (análogo al parámetro estadístico F)
	f.vals <- press.vals / press.vals[length(press.vals)]

	# probabilidades asociadas
	p.vals <- CalcularProbF(f.vals, ncol(calib.x), ncol(calib.x))

	# hay una propuesta basada en resultados empíricos de que el número óptimo
	# de variables latentes es el primero para el cual la probabilidad asociada
	# es menor a 0.75 (Thomas y Haaland 1988)
	for (i in 1 : length(p.vals)) {
		if (p.vals[i] < 0.75) return(i)
	} return(-1)

}

# procesa unos espectros con el algoritmo SavitzkyGolay
# espec : espectros
# ord   : orden de la derivda
# grad  : grado del polinomio
# vlen  : largo de la ventana
ProcesarSavitzkyGolay <- function( espec, ord, grad, vlen ) {

	# calcula los coeficientes del polinomio
	CalcularCoeficientesPolinomio <- function( ord, grad, vlen ) {

		vent <- c(1 : vlen) # ventana deslizante
		med <- (vlen + 1) / 2 # mediana

		mat <- matrix(nrow = 6, ncol = vlen) # ???
		for( i in 1 : nrow(mat) ) {
			mat[i,] <- ventana ^ (i-1)
		}

		p <- matrix(ncol = 6) # ????????
		if (ord == 0) { # eww
			p <- c( med^0, med^1,   med^2,   med^3,    med^4,    med^5 )
		} else if( ord == 1 ) {
			p <- c(    0,      1, 2*med^1, 3*med^2,  4*med^3,  5*med^4 )
		} else if( ord == 2 ) {
			p <- c(    0,      0,       2, 6*med^1, 12*med^2, 20*med^3 )
		}
		p <- t(p)
		p <- t(p) # wtf
		p <- p[ 1 : (grad+1) ]

		aux <- t(mat[ 1 : (grad+1) , ]) # emm
		aux <- t(solve(t(aux) %*% aux) %*% t(aux)) # inversa de penrose

		coeficientes <- matrix(nrow = vlen)
		for (i in 1 : nrow(coeficientes)) {
			coeficientes[i,] <- aux[i,] %*% p
		}

		return (coeficientes)
	} coeficientes <- CalcularCoeficientesPolinomio(ord, grad, vlen)

	espec.p <- matrix( # valor de retorno: espectros procesados
		nrow = nrow(espec) - vlen - 1,
		ncol = ncol(espec)
	)

	for (i in 1 : ncol(espec.p)) { # para todos los espectros

		z <- numeric( nrow(espec.p) )
		# se desliza la ventana por el espectro
		# j es el índice de la primera señal contenido en la ventana
		for (j in 1 : nrow(espec.p) ) {
			data <- espec[j : (j + vlen - 1), i] # señales en la ventana
			# se predice la señal del medio de la ventana con el polinomio
			# usado (se hace una transposición porque 'coeficientes' es una
			# matriz columna)
			z[j] <- t(coeficientes) %*% data
		}
		espec.p[,i] <- t(z)

	}

	return( espec.p )
}
