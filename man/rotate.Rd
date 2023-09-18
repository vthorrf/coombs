\name{rotate}
\alias{rotate}
\title{Rotate a matrix by 90 degrees clockwise}
\description{
Rotates a matrix by 90 degrees clockwise.
}
\usage{
rotate(x)
}
\arguments{
   \item{x}{A matrix.}
}
\value{
The original x matrix rotated 90 degrees clockwise.
}
\examples{
##### Rotate any arbiratry matrix
x <- rWishart(1, 5, diag(5))[,,1]
rotate(x); x
}
