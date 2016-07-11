\name{refNote}
\alias{refNote}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a popup reference note
}
\description{
Generates the html needed to include a popup reference note.
Note requires inclusion of javascript libraries for this to function.
}
\usage{
refNote(text = "This is a test note", number = "*")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
Note to include in popup. Line breaks occasionally cause problems.
As this is generally for short notes; I have not come up with 
a general solution yet.
If your note text is complicated, ensure that you check the result.
}
  \item{number}{
The label to identify the note in the text.
}
}
\value{
The html for the popup note
}
\references{
This was based on the popups on the https://what-if.xkcd.com/ site
}
\author{
Mark Peterson
}
\note{
The html page also needs to include the javascript and css to process the notes for these to work.
Specifically, need to include the following (generally in the header):

\preformatted{
<!-- The javascript to run the popup refernce notes -->
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"></script>
    <script>
      jQuery.noConflict();
      jQuery(function() {
        jQuery(".refbody").hide();
        jQuery(".refnum").click(function(event) {
          jQuery(this.nextSibling).toggle();
          event.stopPropagation();
        });
        jQuery("body").click(function(event) {
          jQuery(".refbody").hide();
        });
      });
    </script>
}

And the following to wherever you include you css definitions:

\preformatted{
/* the reference tooltips style starts here */
/* This was stolen from the What If? css sytle sheet
 *  at https://what-if.xkcd.com/css/style.css
 *  in an effort to emulate that style  */

.ref {
    position: relative;
    vertical-align: baseline;
   }

.refnum {
    position: relative;
    left: 2px;
    bottom: 1ex;
    font-family: Verdana, sans-serif;
    color: #005994;
    font-size: .7em;
    font-weight: bold;
    text-decoration: underline;
    cursor: pointer;
}

.refbody {
    font-family: Verdana, sans-serif;
    font-size: .7em;
    line-height: 1.1;
    display: block;
    min-width: 20em;
    position: absolute;
    left: 25px;
    bottom: 5px ;
    border: 1px solid;
    padding: 5px;
    background-color: #fff;
    word-wrap: break-word;
    z-index: 9999;
    overflow: auto;
}

}

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
cat(refNote())

cat(refNote("Any text can go in here", 42))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ popup }
