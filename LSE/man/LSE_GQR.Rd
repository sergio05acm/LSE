\name{LSE_GQR}
\alias{LSE_GQR}

\title{
LSE and GQR Factorization
}
\description{
This code provides the solution of equality constrained least squares problem through Generalized QR Factorization.
Require MASS package.
}
\usage{
LSE_GQR(A,C,b,d)
}

\arguments{
  \item{A}{
  Design matrix, m rows and n columns.
  }
  \item{C}{
  Constraint matrix, p rows and n columns.
  }
  \item{b}{
  Response vector for A, Ax=b, m rows and 1 column.
  }
  \item{d}{
  Response vector for C, Cx=d, p rows and 1 column.
  }
}
\details{
This algorithm provides the solution of the equality constrained least squares problem through Generalized QR factorization. This algorithm requires the same number of columns for matrices A and C.
}
\value{
Numerical vector for a LSE problem.
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
A = matrix(c(1,2,3,4,5,6),3,2,byrow = TRUE)
C = matrix(c(1,1),1,2,byrow=TRUE)
b = matrix(c(7,1,3),3,1,byrow=TRUE)
d = matrix(c(1),1,1,byrow=TRUE)

LSE_GQR(A,C,b,d) #You can verify that x+y=1 satisfies the constraint.
}
