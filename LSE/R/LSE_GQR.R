LSE_GQR = function(A,C,b,d){

  stopifnot(is.numeric(A) & is.numeric(C) & is.numeric(b) & is.numeric(d))

  if(ncol(A)!=ncol(C)){
    stop("'A' and 'C' must have the same columns number.")
  }

  if(nrow(A)!=nrow(b)){
    stop("'A' and 'b' must have the same rows number.")
  }

  if(nrow(C)!=nrow(d)){
    stop("'A' and 'C' must have the same columns number.")
  }

  n = nrow(t(A)) # o equivalentemente n = nrow(t(C))
  m = ncol(t(A))
  p = ncol(t(C))

  RT = t(GQR(t(A),t(C))$R)

  R11T = RT[ (m-n+1):(m-n+p) , 1:p]
  R12T = RT[ (m-n+p+1):m, 1:p]
  R22T = RT[ (m-n+p+1):m, (p+1):n ]

  ST = t(GQR(t(A),t(C))$S)
  S11T = ST[1:p,1:p]

  P = GQR(t(A),t(C))$W%*%b
  P1 = P[m-n,1]
  P2 = P[m-n+1,1]
  P3 = P[m-n+p+1,1]

  y1 = ginv(S11T)%*%d
  y2 = ginv(R22T)%*%(P3-R12T%*%y1)

  y = rbind(y1,y2)
  x = GQR(t(A),t(C))$Q%*%y
  x
}
