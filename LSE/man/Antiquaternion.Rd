\name{Antiquaternion}
\alias{Antiquaternion}

\title{
Antiquaternion
}
\description{
A quaternion matrix obtained by the Quaternion function, can be transformed into a matrix, which contains as many quaternions as rows obtained, each column represent an imaginary axis (1,i,j,k).
}
\usage{
Antiquaternion(x)
}

\arguments{
  \item{x}{
  Quaternion matrix object.
  }
}
\details{
This function shows in each row a quaternion, and in each column an axis in the order a+bi+cj+dk.
}
\value{
Numeric matrix.
}
\author{
Sergio Andrés Cabrera Miranda
Statician
sergio05acm@gmail.com
}
\seealso{
See Also as \code{\link{Quaternion}}
}
\examples{
Antiquaternion(Quaternion(1,0,1,0))

% There is an Ordinary Least Squares problem solve by quaternions.
% A as design matrix, b as response vector. solved through normal equations
%
% A = rbind(cbind(Quaternion(-1,0,1,0),Quaternion(0,0,0,-1)),
%          cbind(Quaternion(0,1,0,0),Quaternion(1,0,1,0)))
%
% b = rbind(Quaternion(-1,0,0,0),Quaternion(0,1,0,0))
%
% Antiquaternion(ginv(t(A)%*%A)%*%t(A)%*%b)   # ginv function from MASS package.

}
