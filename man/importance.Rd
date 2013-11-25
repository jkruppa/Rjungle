\name{importance}
\alias{importance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
importance
}
\description{
 Reads importance information from the files produced by the 
 \code{\link{rjungle}} function.
}
\usage{
importance(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  An object of class \code{rjungle}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
 A \code{data.frame}. If \code{\link{rjungle}} was called with \code{imp=2}, 
 rows represent the input variables ordered by index and columns contain 
 Gini index as well as Raw-, Breiman-, Liaw- and Meng-Score, SDs for the 
 Breiman- and Meng-Score, variable names and the number of trees in which the 
 according variable was used for splitting.
}
\references{
\url{www.randomjungle.org}
}
\author{
Daniel Schwarz with modifications by Andreas Bender
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{rjungle}}, \code{\link{confusion}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{importance}