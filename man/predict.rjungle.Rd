\name{predict.rjungle}
\alias{predict.rjungle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
predict
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.rjungle(object, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function(object, data, ...) {
  rj = object
  
  if (!inherits(rj, "rjungle"))
    stop("data argument should be rjungle-class")
  if (!rj@keepJungle) stop(RJ__MSG3);
  
  mydata = data
  for (i in 1:ncol(mydata)) {
    if (is.factor(mydata[,i])) {
      mydata[,i] = as.integer(mydata[,i]) 
    }
  }
  fileNameIn = tempfile("rjungledata")
  fileNameOut = tempfile("rjungledata")
  
  write.table(mydata, file = fileNameIn, row.names = FALSE, quote = FALSE)
  if(file.exists(paste(rj@tmpFile, ".jungle.xml.gz", sep = ""))) {
    system(paste("rm -f ", rj@tmpFile, ".jungle.xml", sep = ""))
    system(paste("gunzip ", rj@tmpFile, ".jungle.xml.gz", sep = ""))
  }
  
  system(paste(
    RJ__EXECNAME,
    "-f", fileNameIn,
    "-D", rj@depVarName,
    "-y", rj@treeType,
    "-o", fileNameOut,
    "-P", paste(rj@tmpFile, ".jungle.xml", sep = ""),
    "-v"
  ))
  
  rjPred = new(
    "rjungle",
    tmpDir = tempdir(),
    tmpFile = fileNameOut,
    depVarName = rj@depVarName,
    treeType = rj@treeType,
    ntree = rj@ntree,
    mtry = rj@mtry,
    seed = rj@seed,
    importance = rj@importance,
    proximity = rj@proximity,
    replace = rj@replace,
    keepJungle = rj@keepJungle,
    balanceData = rj@balanceData,
    verbose = rj@verbose
   )
  file.show(paste(rjPred@tmpFile, ".confusion", sep = ""), pager = "cat")
  return(scan(paste(rjPred@tmpFile, ".prediction", sep = "")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ predict }% __ONLY ONE__ keyword per line
