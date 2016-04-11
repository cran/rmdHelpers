\name{thisfile_knit}
\alias{thisfile_knit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify the file currently being knitted
}
\description{
Identifies the file that knitr is processing when the function is called.
}
\usage{
thisfile_knit()
}
\value{
Returns the current file name
}
\references{
Based on https://github.com/krlmlr/kimisc/blob/master/R/thisfile.R
}
\author{
Mark Peterson
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{thisFileName}}
}
\examples{
thisfile_knit()
}
\keyword{ file }
