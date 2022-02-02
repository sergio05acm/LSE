Lagrange = function(A,C,b,d){

  stopifnot(is.numeric(A) & is.numeric(C) & is.numeric(b) & is.numeric(d))

  x_norm = solve(t(A)%*%A) %*% t(A)%*%b
  x_norm + solve(t(A)%*%A) %*% t(C) %*%
  solve( C %*% solve(t(A)%*%A) %*% t(C) ) %*% (d-C %*% x_norm)
}
