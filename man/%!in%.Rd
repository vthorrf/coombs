\name{'%!in%'}
\alias{'%!in%'}
\title{NOT Value Matching}
\description{
`%!in%` is abinary operator, which returns a logical vector indicating if there is a match (FALSE) or not (TRUE) for its left operand.
}
\usage{
x %!in% table
}
\arguments{
   \item{x}{vector or NULL: the values to be matched. Long vectors are supported.}
   \item{table}{vector or NULL: the values to be matched against. Long vectors are not supported.}
}
\details{
%!in% is currently defined as
"%!in%" <- function(x,y) !{x %in% y}
}
\value{
A vector of the same length as x.
}
\examples{
### Compare the results of:
1:10 %in% c(1,3,5,9)
### and its complement:
1:10 %!in% c(1,3,5,9)
}
