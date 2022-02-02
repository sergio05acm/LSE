\name{Quaternion}
\alias{Quaternion}

\title{
Quaternion transformation
}
\description{
A quaternion q=a+bi+cj+dk can be transformed into a real value matrix M(4x4).
}
\usage{
Quaternion(a,b,c,d)
}

\arguments{
  \item{a}{
  Real value coefficient.
  }
  \item{b}{
  Coefficient of the imaginary i-axis.
  }
  \item{c}{
  Coefficient of the imaginary j-axis.
  }
  \item{d}{
  Coefficient of the imaginary k-axis.
  }
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
