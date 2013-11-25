\name{impute}
\alias{impute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
impute
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
impute(iterations = 1, options = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{iterations}{
%%     ~~Describe \code{iterations} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(iterations = 1, options = "", ...
  ) {
  
  options = paste(options, "-I", iterations)
  rj = rjungle(options = options, ...)
  prefix = rj@tmpFile;
  fileName2 = paste(prefix, ".imputed.dat", sep = "");
  fileName = paste(fileName2, ".gz", sep = "");
  
  if (file.exists(fileName)) {
    if (file.exists(fileName2)) {
      system(paste("rm", fileName2));
    }
    system(paste("gunzip", fileName));
  } else {
    if (!file.exists(fileName2)) stop(RJ__MSG4);
  }
  dataImputed = read.table(file = fileName2, header = T);
  return(dataImputed)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ impute }% __ONLY ONE__ keyword per line
