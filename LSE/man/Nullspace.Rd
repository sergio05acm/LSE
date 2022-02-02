\name{Nullspace}
\alias{Nullspace}

\title{
Nullspace method for LSE problem.
}
\description{
Null Space method allows to give an analytic solution for equality constrained least squares problem (LSE).
Requires pracma library.
}
\usage{
Nullspace(A,C,b,d)
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
Null Space method gives a numerical vector as the solution of a least squares problem (Ax=b), using an unconstrained problem equivalent to the LSE proposed, this method an be applied when impose some restrictions (additional information, extramuestral information or a priori information) that lead to another linear equality system (Cx=d). See significance constraint (x=0) or inclusion restriction (x+y=1), etc.
}
\value{
Numerical vector for a LSE problem.
}
\references{
Lawson, C. L., & Hanson, R. J. (1974). Linear least squares with linear inequality constraints. Solving least squares problems, 158-173.

Van Benthem, M. H., Keenan, M. R., & Haaland, D. M. (2002). Application of equality constraints on variables during alternating least squares procedures. Journal of Chemometrics: A Journal of the Chemometrics Society, 16(12), 613-622.
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

Nullspace(A,C,b,d)
}
