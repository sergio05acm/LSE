\name{RQ}
\alias{RQ}

\title{
RQ Factorization of a matrix
}
\description{
RQ factorization allows to develop an orthogonal transformation in a matrix through Householder reflections.
Requires pracma package.
}
\usage{
RQ(y)
}

\arguments{
  \item{y}{
Numeric matrix or vector.
}
}
\details{
RQ factorization make a orthogonal transformation at the rows of the matrix, beginning in the last one, and finishing with the first one row.
}
\value{
 \item{Q}{Orthogonal matrix for x}
 \item{R}{Triangular matrix for x}
}
\author{
Sergio Andrés Cabrera Miranda
Statician
sergio05acm@gmail.com
}

\examples{
A = matrix(runif(12,0,5),4,3,byrow=TRUE)
RQ(A)
}
