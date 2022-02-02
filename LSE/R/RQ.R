# Factorización RQ

RQ = function(y){

  stopifnot(is.numeric(y))

  Permutar_fila = function(x){
    A = matrix(0,nrow(x),ncol(x))
    for (i in 1:nrow(x)) {
      A[nrow(x)+1-i,] = x[i,]
    }
    A
  }

  Permutar_columna = function(x){
    A = matrix(0,nrow(x),ncol(x))
    for (i in 1:ncol(x)) {
      A[,ncol(x)+1-i] = x[,i]
    }
    A
  }

  # Paso 1: Revertir las filas de la matriz
  yy = Permutar_fila(y)

  # Paso 2: Descomponer la matriz
  Q1 = householder(t(yy))$Q
  R1 = householder(t(yy))$R

  # Paso 3: Permutar Q
  J = Permutar_fila(t(Q1))

  # Paso 4: Permutar filas y luego columnas de R para ser una matriz triangular superior
  K_1 = Permutar_fila(t(R1))
  K = Permutar_columna(K_1)

  # Resultado
  list("Q"=J,"R"=K)

}
