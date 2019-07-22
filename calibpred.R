# produce coeficientes de regresión
# usando espectros y concentraciones de calibrado y un número variables latentes
# usando el modelo PLS-1
# calib.x : espectros de calibrado
# calib.y : concentraciones de calibrado
# nvl     : número de variables latentes
CalcularCoefRegr.PLS1 <- function( calib.x, calib.y, nvl ) {

	w <- matrix(ncol = nrow(calib.x)) # vector de pesos
	v <- matrix(ncol = nvl          ) # vector de coeficientes de regresión
	W <- matrix(ncol = nvl, nrow = nrow(calib.x)) # cargamentos de peso
	P <- matrix(ncol = nvl, nrow = nrow(calib.x)) # cargamentos normales
	T <- matrix(ncol = nvl, nrow = ncol(calib.x)) # matriz de puntajes

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
	return (coef.regr) # coeficientes de regresión

}

# realiza una validación cruzada leave-one-out con el modelo PLS-1
# devuelve los errores PRESS para cada número de variables latentes
# calib.x : espectros de calibrado
# calib.y : concentraciones de calibrado
# nvl.max : número máximo de variables latentes
# centrar : booleano para decidir si centrar los datos o no en la validación
ValidarModelo.LOO.PLS1 <- function( calib.x, calib.y, nvl.max, centrar ) {

	# salida, lista de errores PRESS, de uno a nvl.max
	press.vals <- numeric( nvl.max )

	for     (nvl in 1 :      nvl.max ) {
		for (  i in 1 : ncol(calib.x)) {

			# se aisla la i-ésima muestra
		 	espect.ais  <- calib.x[, i] # espectro aislado
			calib.x.p   <- calib.x[,-i] # espectros sin el aislado
			concent.ais <- calib.y[  i] # concentración aislada
			calib.y.p   <- calib.y[ -i] # concentraciones sin la asilada

			if (centrar == TRUE) { # centrado
				CentrarMatriz2DPorColumnas <- function( matriz ) {
					colprom <- matrix(nrow = nrow(matriz)) # columna promedio
					for (i in 1 : nrow(matriz)) {
						colprom[i] <- mean(matriz[i,])
					}
					# restar columa promedio a todas las columnas
					matriz.cent <- matrix(nrow = nrow(matriz), ncol = ncol(matriz))
					for (i in 1 : ncol(matriz.cent)) {
						matriz.cent[,i] <- matriz[,i] - colprom
					}
					return (list(matriz.cent, colprom))
				} # espectros centrados y espectro promedio
				datos_centrados_X <- CentrarMatriz2DPorColumnas(calib.x.p)
				calib.x.p         <- datos_centrados_X[[1]]
				calib.x.p.prom    <- datos_centrados_X[[2]]

				CentrarVector <- function( vector ) {
					vector.prom <- mean(vector)
					vector.cent <- vector - vector.prom
					return (list(vector.cent, vector.prom))
				} # concentraciones centradas y concentración promedio
				datos_centrados_Y <- CentrarVector(calib.y.p)
				calib.y.p         <- datos_centrados_Y[[1]]
				calib.y.p.prom    <- datos_centrados_Y[[2]]
				espect.ais  <- espect.ais  - calib.x.p.prom
				concent.ais <- concent.ais - calib.y.p.prom
			}

			# se crea un modelo PLS-1 con los datos, se intenta predecir el
			# valor de la muestra aislada y se suma el error cuadrado al PRESS
			# para este número de variables latentes
			coef.regr <- as.vector(CalcularCoefRegr.PLS1(calib.x.p, calib.y.p, nvl))
			concent.ais.pred <- coef.regr %*% espect.ais
			press.vals[nvl]  <- press.vals[nvl] + ((concent.ais.pred - concent.ais)^2)

		}
	}

	return (press.vals)
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
	return (1 - p)
}

# estima el número óptimo de variables latentes a usar
# por validación cruzada leave-one-out y PLS-1
# calib.x : espectros de calibrado
# calib.y : concentraciones de calibrado
# nvl.max : número máximo de variables latentes
# centrar : booleano que decide si centrar o no los datos
CalcularNumOptVarLat.LOO.PLS1 <- function( calib.x, calib.y, nvl.max, centrar ) {

	# errores PRESS
	press.vals <- ValidarModelo.LOO.PLS1(calib.x, calib.y, nvl.max, centrar)

	# PRESS / min(PRESS) (análogo al parámetro estadístico F)
	f.vals <- press.vals / press.vals[length(press.vals)]

	# probabilidades asociadas
	p.vals <- CalcularProbF(f.vals, ncol(calib.x), ncol(calib.x))

	# hay una propuesta basada en resultados empíricos de que el número óptimo
	# de variables latentes es el primero para el cual la probabilidad asociada
	# es menor a 0.75 (Thomas y Haaland 1988)
	for (i in 1 : length(p.vals)) {
		if (p.vals[i] < 0.75) return (i)
	}
	return (-1)

}

# toma unos espectros y concentraciones de calibrado, y con un número de
# variables latentes toma unos espectros de prueba y devuelve unas
# concentraciones predichas por regresión sobre componentes principales (PCR)
PredecirConcent.PCR <- function( calib.x, calib.y, test.x, nvl ) {

	calib.x.pca  <- prcomp(calib.x) # análisis de componentes principales (PCA)
	cargas   <- calib.x.pca$rotation
	puntajes <- calib.x.pca$x

	# coeficientes de regresión
	coef.regr <- cargas[,1:nvl] * t(solve(t(T[,1:nvl]) * T[,1:nvl])) * calib.y

	# valor de retorno: concentraciones predichas
	test.y <- matrix(nrow = ncol(test.x))

	for (i in 1 : ncol(test.x)) {
		test.y[i,] <- test.x[,i] * t(coef.regr)
	}

	return(test.y)

}
