library('corpcor') # pseudoinverse()

# realiza suavizado y derivadas con el algoritmo Savitzky-Golay
# espec : espectros
# ord   : orden de la derivda
# grad  : grado del polinomio
# vlen  : largo de la ventana
PrePro.SavitzkyGolay <- function( espec, ord, grad, vlen ) {

	# calcula los coeficientes del polinomio
	CalcularCoeficientesPolinomio <- function( ord, grad, vlen ) {

		vent <- c(1 : vlen) # ventana deslizante
		med <- (vlen + 1) / 2 # mediana

		mat <- matrix(nrow = 6, ncol = vlen) # ???
		for( i in 1 : nrow(mat) ) {
			mat[i,] <- vent ^ (i-1)
		}

		p <- matrix(ncol = 6) # ????????
		if (ord == 0) { # eww
			p <- c( med^0, med^1,   med^2,   med^3,    med^4,    med^5 )
		} else if (ord == 1) {
			p <- c(    0,      1, 2*med^1, 3*med^2,  4*med^3,  5*med^4 )
		} else if (ord == 2) {
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
	}
	coeficientes <- CalcularCoeficientesPolinomio(ord, grad, vlen)

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

	return (espec.p)
}

# realiza una corrección de esparcimiento mutiplicativo
# calib.x  : espectros de calibrado
# prueba.y : espectros de prueba
PrePro.MSC <- function( calib.x, prueba.x ) {

	# procesa un espectro de prueba individualmente
	ProcesarEspectro <- function( calib.x, espec ) {
		espxcal <- matrix(rowMeans(calib.x), nrow = 1) - mean(calib.x)
		espycal <- calib.x - mean(calib.x)
		beta <- t(espycal) %*% pseudoinverse(espxcal, tol = sqrt(.Machine$double.eps))
		alpha <- matrix(colMeans(calib.x), nrow = 1) - t(beta) * sum(matrix(rowMeans(calib.x))) / nrow(calib.x)
		espynew <- espec - mean(calib.x)
		betanew <- t(espynew) %*% pseudoinverse(espxcal)
		alphanew <- mean(espec) - betanew * sum(matrix(rowMeans(calib.x))) / nrow(calib.x)
		unos <- matrix(numeric(nrow(calib.x))+1,ncol=1)
		calib.x.p <- (calib.x - unos %*% alpha) / (unos %*% t(beta))
		espec.p <- (espec - unos %*% alphanew) / (unos %*% betanew)
		return(list(calib.x.p, espec.p))
	}

	# procesa todos los espectros de prueba
	for (i in 1 : ncol(prueba.x)) {
		res <- ProcesarEspectro(calib.x, prueba.x[,i])
		calib.x <- res[[1]]
		prueba.x[,i] <- res[[2]]
	}

	return (list(calib.x, prueba.x))
}

# toma un grupo espectros y deja solamente los valores de un subconjunto de
# los sensores, expresados como una lista de intervalos cerrados ( [a,b] )
# intervalos: vector de vectores
PrePro.FiltrarSensores <- function( espectros, intervalos ) {

	nsensores <- 0 # número de sensores los espectros filtrados
	for (intervalo in intervalos) {
		nsensores <- nsensores + (intervalo[2] - intervalo[1] + 1)
	}

	# valor de retorno: espectros con los sensores filtrados
	espec.filt  <- matrix(nrow = nsensores, ncol = ncol(espectros))

	for (i in 1 : ncol(datosEntrada$calib.x)) { # filtrar cada espectro
		k <- 1 # k-ésimo sensor en espec.filt
		for (intervalo in intervalos) {
			for (j in intervalo[1] : intervalo[2]) {
				espec.filt[k,i] <- espectros[j,i]
				k <- k + 1
			}
		}
	}
	return (espec.filt)

}
