\name{printList}
\alias{printList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print nice lists
}
\description{
Generate a list formatted for printing from a vector.
}
\usage{
printList(toPrint = letters[1:3], finalSepWord = "and", midSep = ",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{toPrint}{
  Vector that you want to turn into a text list.
}
  \item{finalSepWord}{
  The last word to include, defaults to "and" but could be "or" or similar
}
  \item{midSep}{
  Separator between items, defaults to ","
}
}
\details{
Note that this function includes an Oxford comma.
}
\value{
Character vector of length 1 with the values of \code{toPrint} concatenated
and separated as specified in the text.
}
\author{
Mark Peterson
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
printList()

printList(LETTERS[1:5])

printList(letters[1:5], "or", ";")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ paste }
