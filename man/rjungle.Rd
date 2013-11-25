\name{rjungle}
\alias{rjungle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rjungle
}
\description{
Calls the rjungle C++ routines and grows a Random Jungle.
}
\usage{
rjungle(
		depVarName = "",
		data = NULL,
		dataFileName = NULL,
		ntree = 500,
		mtry = NULL,
		treeType = 1,
		importance = 1, 
		replace = FALSE,
		proximity = FALSE,
		keepJungle = TRUE,
		nthread = 0, 
		seed = 123,
    fileNameOut=character(),
		fileNameIn=character(),
		balanceData = FALSE, 
		verbose = FALSE, 
		convertdata=FALSE, 
		inDir=character(),
		outDir=character(),
		options = "",
		...
		)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{depVarName}{
      A \code{\link{character}} string providing the name of the dependend 
      variable, if any. If no variable is specified \code{rjungle} is run 
      unsupervised.
}
  \item{data}{
      A \code{\link{data.frame}} containing the data to be analysed.
}
  \item{dataFileName}{
      A \code{\link{character}} string defining the name of (or the path to) 
      the dataset to be used for analysis. Not implemented or needed at the 
      moment. See \code{fileNameIn}.
}
  \item{ntree}{
      The \code{\link{numeric}} number of trees to be grown in jungle. Defaults 
      to \code{500}.
}
  \item{mtry}{
      A \code{\link{numeric}}, gives the number of randomly chosen variable sets.
}
  \item{treeType}{
  A \code{\link{numeric}} taking values \code{1}--\code{5}. Defines what type of 
  regression or classification should be performed.
  \code{1}: y(response): nominal, x(input): numeric
  \code{2}: y nominal, x nominal
  \code{3}: y numeric, x numeric
  \code{4}: y numeric, x nominal
  \code{5}: as \code{1}, but recommended for more different values in the input variables
  (i.e many floating point numbers). Defaults to \code{1}.
}
  \item{importance}{
    A \code{\link{numeric}} taking values \code{1}--\code{5}. If \code{1}, only 
    the GINI-index importance measure is evaluated. If \code{2}--\code{5} four 
    other measures will be computed and sorted by the measure coerced with the 
    specified value as follows:
    \code{1}: GINI-Index
    \code{2}: Breiman-Score
    \code{3}: Liaw-Score
    \code{4}: raw values, no normalization
    \code{5}: Meng-Score
}
 \item{replace}{
      A \code{\link{logical}}. If \code{TRUE}, the set of variables to choose
      the split from is drawn with replacement, otherwise without replacement.
      Defaults to \code{FALSE}.
}
  \item{proximity}{
    A \code{\link{logical}}, indicates whether proximites should be computed. 
    Defaults to \code{FALSE}.
}

  \item{keepJungle}{
  A \code{\link{logical}} indicating whether to keep the jungle (e.g. for future 
  use in the call of \code{\link{predict}}). Defaults to \code{TRUE}.
}
  \item{nthread}{
  A \code{numeric} taking values from \code{1} to total number of CPUs.
  Defaults to \code{0} which is equivalent to \code{1}.
  
}
  \item{seed}{
  A \code{\link{numeric}} specifing the seed to be used for computation.  
  Defaults to \code{123}.
}
  \item{fileNameOut}{
  A \code{character} specifing the prefix to the files produced by rjungle.
  If \code{\link{missing}} the files will be written to \code{tempfile('rjungledata')}.
  If specified the files will be written to \code{\link{getwd}} with 
  fileNameOut as prefix to the files. If you want to write to the files to 
  another directory specify the \code{outDir} parameter.
}
  \item{fileNameIn}{
  A \code{\link{character}}, gives the name of the dataset to use if \code{data}
  is not specified. Must be a \code{.dat} file an stored in \code{\link{getwd}}
  if \code{inDir} is not given.
} 
  \item{balanceData}{
%%     ~~Describe \code{balanceData} here~~
}

  \item{verbose}{
  A \code{\link{logical}} indicating whether a progress file shall be created. If 
  \code{FALSE} no file is created. Defaults to \code{FALSE}.
}

  \item{convertdata}{
%%     ~~Describe \code{convertdata} here~~
}
  \item{inDir}{
  A \code{\link{character}} giving the directory of the input data. Only 
  if \code{fileNameIn}. Defaults to \code{\link{getwd}}.
}
  \item{outDir}{
  A \code{\link{character}} giving the directory where rjungle output data 
  should be stored. Only applies if \code{fileNameOut} was specified.
  Defaults to \code{\link{getwd}}.
}
  \item{options}{
  Further arguments to be passed to the \code{rjungle} call.
}
  \item{\dots}{
  Further arguments to be passed to the \code{\link{system}} call.
}
}
\details{
}
\value{
Returns an object of class \code{rjungle}. Contains the setting used
in the call of \code{rjungle} function and the path to the temporary files 
produced by \code{rjungle}.
}
\references{
\url{www.randomjungle.org}
}
\author{ Daniel F. Schwarz with modifications by Andreas Bender
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{importance}}, \code{\link{confusion}}
}

\keyword{ ~Random Jungle }
\keyword{ ~rjungle}% __ONLY ONE__ keyword per line
