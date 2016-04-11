\name{myFrac}
\alias{myFrac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Fraction for markdown
}
\description{
Converts fractions for inline markdown using super script for the numerator,
a slash, then subscript for the denominator.
}
\usage{
myFrac(num, denom)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
Either a vector of values for the numerators,
or, if \code{denom} is \code{NULL},
fractions using "/" as the separator
(allows passing fractions directly, as from \code{\link[MASS]{fractions}})
}
  \item{denom}{
Vector of values for the denominators, or \code{NULL} (the default)
if \code{num} is already formatted fractions.
}
}

\value{
Character vector of the fractions
}
\note{
If passing fractions, the function assumes that there is
exactly one "/" in each fraction,
and will return "NA" for the denominator if none is present
or truncate the fraction if more than one "/" is included.
}
\author{
Mark Peterson
}
\seealso{
\code{\link[MASS]{fractions}}
}
\examples{
myFrac(3,4)

myFrac(1:3,4:6)

myFrac(letters[1:5], LETTERS[1:5])

myFrac( c("1/2","3/4", "9856/5646") )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fraction }
