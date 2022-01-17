GQR = function(x,y){

  if(nrow(x)!=nrow(y)){
    stop("'x' and 'y' must have same rows number.")
  }

  stopifnot(is.numeric(x),is.numeric(y))

  Q = householder(y)$Q
  S = householder(y)$R

  W = RQ( t(Q) %*% x )$Q
  R = RQ( t(Q) %*% x )$R

  list("Sean Q,W matrices ortogonales y R,S matrices triangulares superiores, se cumple que A^T=QR y B^T=QSW.",
       "Q"=Q,
       "R"=R,
       "W"=W,
       "S"=S)
}
