\name{confusion}
\alias{confusion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rjungle
}
\description{
In case of regression tasks confusion returns the total sum of squares (SS), 
the SS of residuals and predictor, the prediction accuracy estimate and the 
and the error estimate.
}
\usage{
confusion(rj, printOnly = FALSE, confusionTable = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rj}{
    An object of class \code{rjungle}.
}
  \item{printOnly}{
    If \code{TRUE}, confusion information is printed to the screen 
    when rjungle was used for regression tasks. If \code{FALSE} (default), 
    A \code{vector} with 5 elemnts is returned.
  
}
  \item{confusionTable}{
    The confusion table in case of classification tasks.
    
}
  \item{\dots}{
    Further arguments; not used at the moment.
}
}

\value{
    For regression tasks either \code{NULL} or a \code{vector} with 
    elements SS_Residual, SS_Total,  
    SS_Predictor = SS_Total - SS_Residual,
    Accuracy Estimate = SS_Predictor/SS_Total 
    and Error Estimate = 1 - Accuracy Estimate
%% ...
}
\references{
\url{www.randomjungle.org}
}
\author{
Daniel F. Schwarz with modifications by Andreas Bender
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rjungle}}, \code{\link{importance}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{confusion}% __ONLY ONE__ keyword per line
