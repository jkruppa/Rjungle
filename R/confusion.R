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

library(methods)

##' In case of regression tasks confusion returns the total sum of squares (SS),
##' the SS of residuals and predictor, the prediction accuracy estimate and the
##' and the error estimate.
##'
##' add here more detail information
##' @title rjungle
##' @param rj An object of class \code{rjungle}.
##' @param printOnly If \code{TRUE}, confusion information is printed to the
##' screen when rjungle was used for regression tasks. If \code{FALSE}
##' (default), A \code{vector} with 5 elemnts is returned.  
##' @param confusionTable The confusion table in case of classification tasks.
##' @param ... Further arguments; not used at the moment.
##' @return  For regression tasks either \code{NULL} or a \code{vector} with
##' elements SS_Residual, SS_Total, SS_Predictor = SS_Total - SS_Residual,
##' Accuracy Estimate = SS_Predictor/SS_Total and Error Estimate = 1 - Accuracy
##' Estimate
##' @references \url{www.randomjungle.de}
##' @author Daniel F. Schwarz with modifications by Andreas Bender
##' and Jochen Kruppa
##' @seealso \code{\link{rjungle}}, \code{\link{importance}}
##' @keywords confusion
##' @export


confusion <- function(rj, printOnly=FALSE,
		confusionTable = FALSE, #if TRUE then show file *.confusion2
		...) {
	if (!inherits(rj, "rjungle"))
		stop("data argument should be rjungle-class")

	# get the file name 
	fileName = paste(rj@tmpFile, ".confusion", sep = "")
	whatHeader = TRUE

	if (rj@treeType != 3 & rj@treeType != 4)  {
		if (confusionTable) {
			whatHeader = FALSE
			fileName = paste(rj@tmpFile, ".confusion2", sep = "")
		}
	}

	if (!file.exists(fileName))
		stop(paste("no importance file exists.", fileName))


	ret = NULL
	if (whatHeader){
		con <- file(fileName, "r", blocking = FALSE)
		ret2 = readLines(con) # empty
		close(con)
		if(!printOnly){
      confVec<-numeric(5)
		  splitVec<-c(2,2,2,4,3)
		  for(i in 4:8){
		    str1<-unlist(strsplit(ret2[i], split='='))[splitVec[i-3]]
		    str2<-unlist(strsplit(str1, split=' '))[2]
		    confVec[i-3]<-as.numeric(str2)
      }
      names(confVec)<-c('SS_Residual', 'SS_Total', 'SS_Predictor', 
                        'Accuracy Estimate', 'Error Estimate')
      
      return(confVec)
    }
    else cat(paste(ret2, "\n"))

	} else
		ret = read.table(fileName, header = TRUE)

	return(ret)
}

