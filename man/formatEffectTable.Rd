\name{formatEffectTable}
\alias{formatEffectTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format an effect table
}
\description{
Performs general formatting of effect tables from `lm` suitable for basic printing.
This includes merging estimates and confidence intervals,
rounding, and
optionally improving the display of factor and logical columns
(using a colon and space, instead of just concatenating them).
}
\usage{
formatEffectTable(object
                  , level = 0.95
                  , estDigits = 2
                  , pDigits = 4
                  , cleanFactors = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A fitted model object from `lm`
}
  \item{level}{
The confidence level to be returned
}
  \item{estDigits}{
The number of digits to be displayed for the estimate and confidence thresholds
}
  \item{pDigits}{
The number of digits to be displayed for the p-values
}
  \item{cleanFactors}{
Logical. Should the parameter names for factors be cleaned
by separating the parameter from the value with a colon and a space,
or not.
(This option may lead to issues with merging if set to `TRUE`.)
  }
}

\value{
A data.frame formatted ready to be displayed (e.g. by `kable`)
}
\note{
Note that "NA" values are silently dropped
(they are not returned by `summary.lm` in the coefficients table).
}
\author{
Mark Peterson
}
\examples{

irisMod <- lm(Sepal.Length ~ ., data = iris)
formatEffectTable(irisMod)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ format }
