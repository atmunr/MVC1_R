# centra un vector
CentrarVector = function( vector ) {
  vector.prom = mean( vector )
  vector.cent = vector - vector.prom
  return(list( vector.cent, vector.prom ))
}

# centra una matriz 2D por columnas
CentrarMatriz2DPorColumnas = function( matriz ) {
  # columna promedio de M: el i-ésimo valor de column_prom es el promedio de
  # los valores de la i-ésima fila de M
  colprom = matrix( nrow = nrow( matriz ) )
  for ( i in 1 : nrow( matriz ) ) {
    colprom[i] = mean( matriz[i,] )
  }
  # matriz centrada: a cada columna se le resta la columna promedio
  matriz.cent = matrix( nrow = nrow( matriz ), ncol = ncol( matriz ) )
  for ( i in 1 : ncol(matriz.cent) ) {
    matriz.cent[,i] = matriz[,i] - colprom
  }
  return(list( matriz.cent, colprom ))
}

# produce coeficientes de regresión
# para unas muestras de calibrado y un número variables latentes
# usando el modelo PLS
CalcularCoefRegrPLS = function( calib.x, calib.y, num.var.lat ) {
  I = ncol( calib.x ) # n° de muestras en Xcal
  J = nrow( calib.x ) # orden de una muestra
  w = matrix(         , ncol = J       )
  v = matrix(         , ncol = num.var.lat )
  W = matrix( nrow = J, ncol = num.var.lat )
  T = matrix( nrow = I, ncol = num.var.lat )
  P = matrix( nrow = J, ncol = num.var.lat )

  for ( i in 1 : num.var.lat ) {
    w     = calib.x %*% calib.y / as.numeric( t( calib.y ) %*% calib.y )
    W[,i] = w / as.numeric(sqrt( t(w) %*% w ))
    T[,i] = t( calib.x ) %*% W[,i]
    v[i]  = t(T[,i]) %*% calib.y / as.numeric(t(T[,i]) %*% T[,i])
    P[,i] = calib.x %*% T[,i] / as.numeric(t(T[,i]) %*% T[,i])
    calib.x  = calib.x - P[,i] %*% t(T[,i])
    calib.y = calib.y - v[i] * T[,i]
  }
  coef.regr  = W %*% solve(t(P) %*% W) %*% t(v)
  return( coef.regr ) # coeficientes de regresión
}

# prueba todos los números de variables latentes desde 1 a var_lat_max
# y devuelve su error estadístico PRESS por validación cruzada
CalcularPRESSPorNumVarLat = function( calib.x, calib.y, num.max.var.lat, centrar.datos ) {

  # salida, lista de errores PRESS, de uno a var_lat_max
  press.vals = numeric( num.max.var.lat )

  for ( num.var.lat in 1 : num.max.var.lat ) { # para todos los números de variables latentes
    for ( i in 1 : ncol( calib.x ) ) { # para todas las muestras
      # se aisla la i-ésima muestra
      muestra.asilada.x   = calib.x[,i]
      calib.x.p  = calib.x[,-i]
      muestra.asilada.y   = calib.y[i]
      calib.y.p = calib.y[-i]

      if ( centrar.datos == TRUE ) { # se centran los datos
        datos_centrados_X = CentrarMatriz2DPorColumnas( calib.x.p )
        calib.x.p             = datos_centrados_X[[1]]
        calib.x.p.prom         = datos_centrados_X[[2]]
        datos_centrados_Y = CentrarVector( calib.y.p )
        calib.y.p            = datos_centrados_Y[[1]]
        calib.y.p.prom        = datos_centrados_Y[[2]]
        muestra.asilada.x = muestra.asilada.x - calib.x.p.prom
        muestra.asilada.y = muestra.asilada.y - calib.y.p.prom
      }

      # se crea un modelo PLS con los datos, se intenta predecir el valor de la
      # muestra aislada y se suma el error cuadrado al PRESS de este número de
      # variables latentes
      coef.regr = as.vector(CalcularCoefRegrPLS( calib.x.p, calib.y.p, num.var.lat ))
      muestra.asilada.y.pred = coef.regr %*% muestra.asilada.x
      press.vals[num.var.lat] = press.vals[num.var.lat] + ((muestra.asilada.y.pred - muestra.asilada.y)^2)
    }
  }
  return( press.vals )
}

# devuelve la probablidad de obtener la estadística F
# en función de F y los grados de libertad k1 y k2
CalcularProbF = function( f, k1, k2 ) {
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
CalcularNumOptVarLat = function( calib.x, calib.y, num.max.var.lat, centrar.datos ) {
  # errores PRESS de distintos número de variables latentes
  press.vals = CalcularPRESSPorNumVarLat( calib.x, calib.y, num.max.var.lat, centrar.datos )
  # estadísticas F para los errores PRESS
  f.vals = press.vals / press.vals[ length( press.vals ) ]
  # probabilidad P de obtener cada estadística F
  p.vals = CalcularProbF( f.vals, ncol( calib.x ), ncol( calib.x ) )
  # teorema: el número óptimo de variables latentes es el primero para el cual
  # la probabilidad  P es menor a 0.75
  for ( i in 1 : length( p.vals ) ) {
    if( p.vals[i] < 0.75 ) return(i)
  }
  return(-1)
}

calib.x = as.matrix(read.table("tests/Xcalib.txt" )) # espectros de calibrado
calib.y = as.matrix(read.table("tests/Ycalib.txt" )) # concentraciones de calibrado del primer analito
test.x  = as.matrix(read.table("tests/Xtest.txt"  )) # espectros de predicción

num.var.lat = CalcularNumOptVarLat( calib.x, calib.y, 10, centrar.datos = TRUE ) # variables latentes

coef.regr = CalcularCoefRegrPLS( calib.x, calib.y, num.var.lat ) # coeficientes de regresión

test.y = t( test.x ) %*% coef.regr # concentraciones predichas del primer analito

write.table( test.y, file = "tests/Ytest.txt", row.names = FALSE, col.names = FALSE )
