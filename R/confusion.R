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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param rj 
##' @param printOnly 
##' @param confusionTable 
##' @param ... 
##' @return 
##' @author Jochen Kruppa
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

