\name{Lagrange}
\alias{Lagrange}

\title{
Lagrange multipliers for LSE problem.
}
\description{
Lagrange multipliers allows to give a analytic solution for equality constrained least squares problem (LSE).
}
\usage{
Lagrange(A,C,b,d)
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
The Lagrange multipliers method gives a numerical vector as the solution of a least squares problem (Ax=b) through unification the model and their restrictions in one function, the restrictions impose in the model (additional information, extramuestral information or a priori information) lead to another linear equality system (Cx=d). See significance constraint (x=0) or inclusion restriction (x+y=1), etc.
}
\value{
Numerical vector for a LSE problem.
}
\references{
Rao, C. R., Toutenburg, H., Shalabh, H. C., & Schomaker, M. (2008). Linear models and generalizations. Least Squares and Alternatives (3rd edition) Springer, Berlin Heidelberg New York.

Theil, H. (1971). Principles of econometrics (No. 04; HB139, T44.).
}
\author{
Sergio Andrés Cabrera Miranda
Statician
sergio05acm@gmail.com
}
\examples{
A = matrix(runif(50,-1,1),10,5)
C = matrix(runif(20,-1,1),4,5)
b = matrix(runif(10,-1,1),10,1)
d = matrix(runif(4,-1,1),4,1)

Lagrange(A,C,b,d)

}
