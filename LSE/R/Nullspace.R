Nullspace = function(A,C,b,d){

  stopifnot(is.numeric(A) & is.numeric(C) & is.numeric(b) & is.numeric(d))

  RC = householder(t(C))$R # pracma library
  QC = householder(t(C))$Q # pracma library

  p = Rank(C) # pracma library

  Q1C = QC[,1:p]
  Q2C = QC[,(p+1):ncol(QC)]

  x1 = ginv(C) %*% d

  y2 = ginv(A %*% Q2C) %*% (b-A %*% x1)

  x = x1+Q2C %*% y2

  x

}
