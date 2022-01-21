\name{GQR}
\alias{GQR}

\title{
Generalized QR Factorization
}
\description{
This code provides a simultaneous orthogonal factorization for two matrices A and B.
}
\usage{
GQR(x,y)
}

\arguments{
  \item{x}{
  Numeric matrix
  }
  \item{y}{
  Numeric matrix
  }
}
\details{
Given two matrices, with the same number of rows, this algorithm provides a single factorization, such that A=QR and ((Q^T)B)=WS.
}
\value{
  \item{Q}{Orthogonal matrix for A}
  \item{R}{Trapezoidal matrix for A}
  \item{W}{Orthogonal matrix for (Q^T)B}
  \item{S}{Trapezoidal matrix for (Q^T)B}
}
\references{
Anderson, E., Bai, Z., & Dongarra, J. (1992). Generalized QR factorization and its applications. Linear Algebra and its Applications, 162, 243-271.
}
\author{
Sergio Andrés Cabrera Miranda
Statician
sergio05acm@gmail.com
}

\examples{
A = matrix(c(1,1,1,1,3,1,1,-1,1,1,1,1),4,3,byrow=TRUE)
C = matrix(c(1,1,1,1,1,-1),2,3,byrow=TRUE)
GQR(t(A),t(C))
}
