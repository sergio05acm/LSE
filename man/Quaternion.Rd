\name{Quaternion}
\alias{Quaternion}

\title{
Quaternion transformation
}
\description{
A quaternion a+bi+cj+dk can be transformed into a matrix M\in \Re^{4}.
}
\usage{
Quaternion(a,b,c,d)
}

\arguments{
  \item{a}{
  Real value coefficient.
  }
  \item{b}{
  Imaginary i-axis
  }
  \item{c}{
  Imaginary j-axis
  }
  \item{d}{
  Imaginary k-axis
  }
}
\details{
A quaternion matrix can be simplify into a quaternion number.
}
\value{
Real value matrix to represent a quaternion.
}
\references{
Al-Zhour, Z. (2019). Some new linear representations of matrix quaternions with some applications. Journal of King Saud University-Science, 31(1), 42-47.
}
\author{
Sergio Andrés Cabrera Miranda
Statician
sergio05acm@gmail.com
}
\examples{
Quaternion(1,0,1,0)

rbind(cbind(Quaternion(-1,0,1,0),Quaternion(0,0,0,-1)),
      cbind(Quaternion(0,1,0,0),Quaternion(1,0,1,0))
      )
}
