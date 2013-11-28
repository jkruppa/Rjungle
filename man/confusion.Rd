\name{confusion}
\alias{confusion}
\title{rjungle}
\usage{
  confusion(rj, printOnly = FALSE, confusionTable = FALSE,
    ...)
}
\arguments{
  \item{rj}{An object of class \code{rjungle}.}

  \item{printOnly}{If \code{TRUE}, confusion information is
  printed to the screen when rjungle was used for
  regression tasks. If \code{FALSE} (default), A
  \code{vector} with 5 elemnts is returned.}

  \item{confusionTable}{The confusion table in case of
  classification tasks.}

  \item{...}{Further arguments; not used at the moment.}
}
\value{
  For regression tasks either \code{NULL} or a
  \code{vector} with elements SS_Residual, SS_Total,
  SS_Predictor = SS_Total - SS_Residual, Accuracy Estimate
  = SS_Predictor/SS_Total and Error Estimate = 1 - Accuracy
  Estimate
}
\description{
  In case of regression tasks confusion returns the total
  sum of squares (SS), the SS of residuals and predictor,
  the prediction accuracy estimate and the and the error
  estimate.
}
\details{
  add here more detail information
}
\author{
  Daniel F. Schwarz with modifications by Andreas Bender
  and Jochen Kruppa
}
\references{
  \url{www.randomjungle.de}
}
\seealso{
  \code{\link{rjungle}}, \code{\link{importance}}
}
\keyword{confusion}

