\name{formatP}
\alias{formatP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format p-value
}
\description{
A wrapper to sensibly control the printing of p-values
because I was frustrated with playing with \code{round}.
Calls \code{format} instead now.
}
\usage{
formatP(p, digits = 3, scientific = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
Numeric vector of values to be displayed
}
  \item{digits}{
Numeric of length one giving the number of digits to display.
Note that if \code{p} is longer than 1, all returned values
will be to the same level of precision.
}
  \item{scientific}{
Logical, should scientific notation be used?
}
  \item{\dots}{
Further arguments passed to \code{format}
  }
}
\value{
Character vector of the p-value(s) formatted
}
\author{
Mark Peterson
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[base]{format}}
}
\examples{
formatP(0.049865465646)

formatP(0.00000013212354)

formatP(c(0.01564643131,.0003456463131, .45643131564), 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ p-value }
