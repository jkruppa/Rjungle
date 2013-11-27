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
library(stats)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param object 
##' @param data 
##' @param ... 
##' @return 
##' @author Jochen Kruppa
predict.rjungle <- function(object, data, ...) {
  rj = object
  
  if (!inherits(rj, "rjungle"))
    stop("data argument should be rjungle-class")

  if (!rj@keepJungle) stop(RJ__MSG3);
  
  # convert factor to integer
  mydata = data
  for (i in 1:ncol(mydata)) {
    if (is.factor(mydata[,i])) {
      mydata[,i] = as.integer(mydata[,i]) 
    }
  }

  # save file
  fileNameIn = tempfile("rjungledata")
  fileNameOut = tempfile("rjungledata")
  
  write.table(mydata, file = fileNameIn, row.names = FALSE, quote = FALSE)

  # unpack
  if(file.exists(paste(rj@tmpFile, ".jungle.xml.gz", sep = ""))) {
    system(paste("rm -f ", rj@tmpFile, ".jungle.xml", sep = ""))
    system(paste("gunzip ", rj@tmpFile, ".jungle.xml.gz", sep = ""))
  }
  
  # do the rjungle
  system(paste(
    RJ__EXECNAME,
    "-f", fileNameIn,
    "-D", rj@depVarName,
    "-y", rj@treeType,
    "-o", fileNameOut,
    "-P", paste(rj@tmpFile, ".jungle.xml", sep = ""),
    "-v"
  ))
  
  # save results
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

  # show prediction matrix
  return(scan(paste(rjPred@tmpFile, ".prediction", sep = "")))
}
