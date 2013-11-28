\name{importance.rjungle}
\alias{importance.rjungle}
\title{importance}
\usage{
  importance.rjungle(x, ...)
}
\arguments{
  \item{x}{An object of class \code{rjungle}.}

  \item{...}{}
}
\value{
  A \code{data.frame}. If \code{\link{rjungle}} was called
  with \code{imp=2}, rows represent the input variables
  ordered by index and columns contain Gini index as well
  as Raw-, Breiman-, Liaw- and Meng-Score, SDs for the
  Breiman- and Meng-Score, variable names and the number of
  trees in which the according variable was used for
  splitting.
}
\description{
  Reads importance information from the files produced by
  the \code{\link{rjungle}} function.
}
\details{
  to add
}
\author{
  Daniel Schwarz with modifications by Andreas Bender and
  Jochen Kruppa
}
\references{
  \url{www.randomjungle.de}
}
\seealso{
  \code{\link{rjungle}}, \code{\link{confusion}}
}
\keyword{importance}

