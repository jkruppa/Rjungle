# Copyright (C) 2008-2010  Daniel F. Schwarz
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

##' Calls the rjungle C++ routines and grows a Random Jungle.
##'
##' to add
##' @title rjungle
##' @param depVarName  A \code{\link{character}} string providing the name of
##' the dependend variable, if any. If no variable is specified \code{rjungle}
##' is run unsupervised.
##' @param data A \code{\link{data.frame}} containing the data to be analysed.
##' @param dataFileName A \code{\link{character}} string defining the name of
##' (or the path to) the dataset to be used for analysis. Not implemented or
##' needed at the moment. See \code{fileNameIn}.
##' @param ntree  The \code{\link{numeric}} number of trees to be grown in
##' jungle. Defaults to \code{500}.
##' @param mtry A \code{\link{numeric}}, gives the number of randomly chosen
##' variable sets.
##' @param treeType A \code{\link{numeric}} taking values \code{1}--\code{5}.
##' Defines what type of regression or classification should be performed.
##'   \code{1}: y(response): nominal, x(input): numeric
##'   \code{2}: y nominal, x nominal
##'   \code{3}: y numeric, x numeric
##'   \code{4}: y numeric, x nominal
##'   \code{5}: as \code{1}, but recommended for more different values in the
##' input variables (i.e many floating point numbers). Defaults to \code{1}.
##' @param importance A \code{\link{numeric}} taking values \code{1}--\code{5}.
##' If \code{1}, only the GINI-index importance measure is evaluated.
##' If \code{2}--\code{5} four other measures will be computed and sorted by
##' the measure coerced with the specified value as follows:
##'   \code{1}: GINI-Index
##'   \code{2}: Breiman-Score
##'   \code{3}: Liaw-Score
##'   \code{4}: raw values, no normalization
##'   \code{5}: Meng-Score
##' @param replace  A \code{\link{logical}}. If \code{TRUE}, the set of
##' variables to choose the split from is drawn with replacement, otherwise
##' without replacement. Defaults to \code{FALSE}.
##' @param proximity  A \code{\link{logical}}, indicates whether proximites
##' should be computed. Defaults to \code{FALSE}.
##' @param keepJungle  A \code{\link{logical}} indicating whether to keep
##' the jungle (e.g. for future use in the call of \code{\link{predict}}).
##' Defaults to \code{TRUE}.
##' @param nthread  A \code{numeric} taking values from \code{1} to total
##' number of CPUs. Defaults to \code{0} which is equivalent to \code{1}.
##' @param seed  A \code{\link{numeric}} specifing the seed to be used for
##' computation.Defaults to \code{123}.
##' @param fileNameOut A \code{character} specifing the prefix to the files
##' produced by rjungle. If \code{\link{missing}} the files will be written
##' to \code{tempfile('rjungledata')}. If specified the files will be written
##' to \code{\link{getwd}} with fileNameOut as prefix to the files. If you
##' want to write to the files to another directory specify the \code{outDir}
##' parameter.
##' @param fileNameIn A \code{\link{character}}, gives the name of the dataset
##' to use if \code{data} is not specified. Must be a \code{.dat} file an
##' stored in \code{\link{getwd}} if \code{inDir} is not given.
##' @param balanceData to add
##' @param verbose  A \code{\link{logical}} indicating whether a progress
##' file shall be created. If \code{FALSE} no file is created. Defaults to
##' \code{FALSE}.
##' @param convertdata to add
##' @param inDir  A \code{\link{character}} giving the directory of the
##' input data. Only if \code{fileNameIn}. Defaults to \code{\link{getwd}}.
##' @param outDir  A \code{\link{character}} giving the directory where
##' rjungle output data should be stored. Only applies if \code{fileNameOut}
##' was specified. Defaults to \code{\link{getwd}}.
##' @param options  Further arguments to be passed to the \code{rjungle} call.
##' @param ... Further arguments to be passed to the \code{\link{system}} call.
##' @return Returns an object of class \code{rjungle}. Contains the setting
##' used in the call of \code{rjungle} function and the path to the temporary
##' files produced by \code{rjungle}.
##' @references \url{www.randomjungle.de}
##' @author Daniel F. Schwarz with modifications by Andreas Bender
##' and Jochen Kruppa
##' @seealso \code{\link{importance}}, \code{\link{confusion}}
##' @keywords Random Jungle rjungle
##' @export
rjungle <- function(
    depVarName = "",
    data = NULL,
    dataFileName = NULL,
    ntree = 500,
    mtry = NULL,
    treeType = 1,
    importance = 1, # default: GINI-Index
    replace = FALSE,
    proximity = FALSE,
    keepJungle = TRUE,
    nthread = 0, # maximal number
    seed = 123,
    fileNameOut=character(),
    fileNameIn=character(),
    balanceData = FALSE, # experimental
    verbose = FALSE, 
    convertdata=FALSE, #convert data when factors available
    inDir=character(),
    outDir=character(),
    options = "",
    ...
    ) {
    
                                        # importance
    if (is.null(data) & is.null(dataFileName)) 
        stop("Please, specify data or dataFileName.")
    
                                        # get mtry
    if (is.null(mtry)) {
        mtry = 0
		if (!is.null(data[,depVarName]) && !is.factor(data[,depVarName])) {
			max(floor(ncol(data)/3), 1)
		} else {
			floor(sqrt(ncol(data)))
		}
	}

	# convert data  
	if (convertdata) {
		isFacResponse = FALSE;
		isFacPredictors = FALSE;
		isFloating = FALSE;
		if (depVarName == "") treeType = 1 else treeType = 2;

		mydata = data
		predAreFac = c()
		for (i in 1:ncol(mydata)) {
			if (is.factor(mydata[,i])) {
				if (depVarName != "") {
					if (colnames(mydata)[i] == depVarName) isFacResponse = TRUE;
					if (colnames(mydata)[i] != depVarName)
						predAreFac = c(predAreFac, TRUE) else predAreFac = c(predAreFac, FALSE)
				}
				mydata[,i] = as.integer(mydata[,i]) # convert factor to integer
			} else {
				isNA = is.na(mydata[,i]);
				if (sum(as.integer(mydata[!isNA,i]) == mydata[!isNA,i]) == 0) isFloating = TRUE;
			}
		}

		if (all(predAreFac)) isFacPredictors = TRUE;

		# get tree type
		if (isFacResponse && !isFacPredictors && !isFloating) treeType = 1;
		if (isFacResponse && !isFacPredictors && isFloating) treeType = 6;
		#if (!isFacResponse && !isFacPredictors) treeType = 2;
		if (isFacResponse && isFacPredictors) treeType = 3;
		if (!isFacResponse && isFacPredictors) warning(RJ__MSG1);
	} else {
		mydata = data;
	}

  #tmpDir
  Dir=tempdir()
  
	#input file name + directory
	if(missing(fileNameIn)) fileNameIn = tempfile("rjungledata")
	else{
	 if(missing(inDir)) filNameIn=paste(getwd(), '/', fileNameIn, sep='')
	 else fileNameIn=paste(inDir, '/', fileNameIn, sep='')
  }
	
  # output files name + directory
  if(missing(fileNameOut)) fileNameOut = tempfile("rjungledata")
	else{
	 if(missing(outDir)) fileNameOut=paste(getwd(), '/', fileNameOut, sep='')
   else{ 
    fileNameOut=paste(outDir, '/', fileNameOut, sep='')
    Dir=outDir
   }
  }
  
  # write data
	write.table(mydata, file = fileNameIn, row.names = FALSE, quote = FALSE)

	# do the jungle
	argList<-eval(substitute(list(...)))
	argList$command<-paste(
					RJ__EXECNAME,
					"-f", fileNameIn,
					if (depVarName != "") "-D", depVarName,
					"-y", treeType,
					"-o", fileNameOut,
					"-t", ntree,
					"-m", mtry,
					"-z", seed,
					"-i", importance,
					"-U", nthread,
					if (!replace) "-u",
					if (proximity) "-s",
					if (keepJungle) "-w2",
					if (balanceData) "-W",
					if (verbose) "-v",
					options
					)
	do.call('system', args=argList)


	# create object
	rj = new(
			"rjungle",
			tmpDir = Dir,
			tmpFile = fileNameOut,
			depVarName = depVarName,
			treeType = treeType,
			ntree = ntree,
			mtry = mtry,
			seed = seed,
			importance = importance,
			proximity = proximity,
			replace = replace,
			keepJungle = keepJungle,
			balanceData = balanceData,
			verbose = verbose
			)

	return(rj)
}

