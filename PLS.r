# centra un vector
centrarVector = function( V ) {
  meanval = mean(V)
  V_cent = V - meanval
  return(list( V_cent, meanval ))
}

# centra una matriz 2D por columnas
centrarMatriz_2_2 = function( M ) {
  # columna promedio de M: el i-ésimo valor de column_prom es el promedio de
  # los valores de la i-ésima fila de M
  col_mean = matrix( nrow = nrow(M) )
  for ( i in 1 : nrow(M) ) {
    col_mean[i] = mean( M[i,] )
  }
  # matriz centrada: a cada columna se le resta la columna promedio
  M_cent = matrix( nrow = nrow(M), ncol = ncol(M) )
  for ( i in 1 : ncol(M_cent) ) {
    M_cent[,i] = M[,i] - col_mean
  }
  return(list( M_cent, col_mean ))
}

# produce coeficientes de regresión
# para unas muestras de calibrado y un número variables latentes
# usando el modelo PLS
PLS = function( Xcal, Ycaln, var_lat ) {
  I = ncol(Xcal) # n° de muestras en Xcal
  J = nrow(Xcal) # orden de una muestra
  w = matrix(         , ncol = J       )
  v = matrix(         , ncol = var_lat )
  W = matrix( nrow = J, ncol = var_lat )
  T = matrix( nrow = I, ncol = var_lat )
  P = matrix( nrow = J, ncol = var_lat )

  for ( i in 1 : var_lat ) {
    w     = Xcal %*% Ycaln / as.numeric(t(Ycaln) %*% Ycaln)
    W[,i] = w / as.numeric(sqrt( t(w) %*% w ))
    T[,i] = t(Xcal) %*% W[,i]
    v[i]  = t(T[,i]) %*% Ycaln / as.numeric(t(T[,i]) %*% T[,i])
    P[,i] = Xcal %*% T[,i] / as.numeric(t(T[,i]) %*% T[,i])
    Xcal  = Xcal - P[,i] %*% t(T[,i])
    Ycaln = Ycaln - v[i] * T[,i]
  }
  coef_regr  = W %*% solve(t(P) %*% W) %*% t(v)
  return( coef_regr ) # coeficientes de regresión
}

# prueba todos los números de variables latentes desde 1 a var_lat_max
# y devuelve su error estadístico PRESS por validación cruzada
errorPorNumVarLat = function( Xcal, Ycaln, var_lat_max ) {

  # salida, lista de errores PRESS, de uno a var_lat_max
  press_vals = numeric( var_lat_max )

  for ( var_lat in 1 : var_lat_max ) { # para todos los números de variables latentes
    for ( i in 1 : ncol(Xcal) ) { # para todas las muestras
      # se aisla la i-ésima muestra
      xout   = Xcal[,i]
      Xcalp  = Xcal[,-i]
      yout   = Ycaln[i]
      Ycalnp = Ycaln[-i]

      # se centran los datos
      datos_centrados_X = centrarMatriz_2_2( Xcalp )
      Xcalp             = datos_centrados_X[[1]]
      Xcalpmean         = datos_centrados_X[[2]]
      datos_centrados_Y = centrarVector( Ycalnp )
      Ycalnp            = datos_centrados_Y[[1]]
      Ycalnpmean        = datos_centrados_Y[[2]]
      xout = xout - Xcalpmean
      yout = yout - Ycalnpmean

      # se crea un modelo PLS con los datos, se intenta predecir el valor de la
      # muestra aislada y se suma el error cuadrado al PRESS de este número de
      # variables latentes
      coef_regr = as.vector(PLS( Xcalp, Ycalnp, var_lat ))
      yout_pred = coef_regr %*% xout
      press_vals[var_lat] = press_vals[var_lat] + ((yout_pred - yout)^2)
    }
  }
  return( press_vals )
}

# devuelve la probablidad de obtener la estadística F
# en función de F y los grados de libertad k1 y k2
probFstat = function( f, k1, k2 ) {
  d1 = 0.0498673470
  d2 = 0.0211410061
  d3 = 0.0032776263
  d4 = 0.0000380036
  d5 = 0.0000488906
  d6 = 0.0000053830
  f3 = f ^ (1 / 3)
  k8 = 2 / (9 * k1)
  k9 = 2 / (9 * k2)
  z  = ((1 - k9) * f3 - 1 + k8) / sqrt((k9 * f3 * f3) + k8)
  if ( k2 <= 3 ) {
    z = z * (1 + 0.08 * z * (z / k2) ^ 3)
  }
  z9   = abs(z)
  z9d6 = z9 * d6
  p = 1 + z9 * (d1 + z9 * (d2 + z9 * (d3 + z9 * (d4 + z9 * (d5 + z9d6)))))
  p = 0.5 / (p ^ 16)
  p = (sign(z) * (p - 0.5)) + 0.5
  p = round(p * 1000 + 0.5) / 1000
  return(1 - p)
}

# calcula el número óptimo de variables latentes a utilizar entre 1 y var_lat_max
numOptimoVarLat = function( Xcal, Ycaln, var_lat_max ) {
  # errores PRESS de distintos número de variables latentes
  press_vals = errorPorNumVarLat( Xcal, Ycaln, var_lat_max )
  # estadísticas F para los errores PRESS
  f_vals = press_vals / press_vals[ length(press_vals) ]
  # probabilidad P de obtener cada estadística F
  p_vals = probFstat( f_vals, ncol(Xcal), ncol(Xcal) )
  # teorema: el número óptimo de variables latentes es el primero para el cual
  # la probabilidad  P es menor a 0.75
  for ( i in 1 : length(p_vals) ) {
    if( p_vals[i] < 0.75 ) return(i)
  }
  return(-1)
}

Xcal  = as.matrix(read.table("Xcal.txt" )) # espectros de calibrado
Ycal1 = as.matrix(read.table("Ycal1.txt")) # concentraciones de calibrado del primer analito
Xtest = as.matrix(read.table("Xtest.txt")) # espectros de predicción

A = numOptimoVarLat( Xcal, Ycal1, 10 ) # variables latentes

bn = PLS( Xcal, Ycal1, A ) # coeficientes de regresión

Ytest1 = t(Xtest) %*% bn # concentraciones predichas del primer analito

write.table( Ytest1, file = "Ytest1.txt", row.names = FALSE, col.names = FALSE )
