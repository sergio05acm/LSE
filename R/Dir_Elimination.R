Dir_Elimination = function(A,C,b,d){

  stopifnot(is.numeric(A) & is.numeric(C) & is.numeric(b) & is.numeric(d))

  p = Rank(C) # pracma library

  C_1 = C[1:p,1:p]
  C_2 = C[1:p,(p+1):ncol(C)]

  A_1 = A[,1:p]
  A_2 = A[,(p+1):ncol(A)]

  b1 = b[1:p,]
  b2 = b[(p+1):nrow(b),]

  E = A_2 - A_1 %*% ginv(C_1) %*% C_2 # MASS library
  J = b - A_1 %*% ginv(C_1) %*% d # MASS library

  x2 = ginv(E)%*%J # MASS library
  x1 = ginv(C_1) %*% (d-C_2%*%x2) # MASS library

  rbind(x1,x2)

}
