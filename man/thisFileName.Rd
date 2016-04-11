\name{thisFileName}
\alias{thisFileName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify current file
}
\description{
Identify the file currently being processed
}
\usage{
thisFileName()
}
%- maybe also 'usage' for other objects documented here.
\details{
Currently only works for files being processed with \code{knit};
may try to add more
}
\value{
The current file name
}
\references{
Based on https://github.com/krlmlr/kimisc/blob/master/R/thisfile.R
}

\seealso{
\code{\link{thisfile_knit}}
}
\examples{
thisFileName()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
