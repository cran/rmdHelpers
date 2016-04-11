\name{myKable}
\alias{myKable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper for kable
}
\description{
A small wrapper for the knitr kable function
to allow automated bolding of row and/or column names.
Additional functionality may be added/
}
\usage{
myKable(x, row.names = NA, boldRowNames = TRUE, boldColNames = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Table or matrix to be passed to \code{kable}
}
  \item{row.names}{
Logical: should row names be included?
Defaults to \code{NULL} which includes row names if they are not
just numeric in order.
}
  \item{boldRowNames}{
Logical: should row names be bolded?
}
  \item{boldColNames}{
Logical: should column names be bolded?
}
  \item{\dots}{
Additional arguments to be passed to \code{kable}
}
}
\details{
Currently bolds in markdown format,
so needs to be passed through interpreter after running.
}
\value{
A formatted table from \code{kable}
}
\author{
Mark Peterson
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[knitr]{kable}
}
\examples{
tempTable <- matrix(LETTERS[6:20], nrow = 5)
colnames(tempTable) <- LETTERS[24:26]
row.names(tempTable) <- LETTERS[1:5]
myKable(tempTable)

myKable(tempTable, boldColNames =  FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ kable }
