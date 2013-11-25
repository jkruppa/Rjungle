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

if (!exists("importance"))
  importance <- function(x, ...)  UseMethod("importance")

if (!exists("importance.default"))
  importance.default <- function(x, ...)
    stop("No method implemented for this class of object")

importance.rjungle <- function(x, ...) {
  rj = x
  
  if (!inherits(rj, "rjungle"))
    stop("data argument should be rjungle-class")
  
	if (rj@importance == 1){  
		fileName = paste(rj@tmpFile, ".importance", sep = "")
	  if (!file.exists(fileName)) stop(paste("no importance file exists.", fileName))
		imp<-read.table(fileName, header=TRUE)
    imp<-imp[order(imp$id),] 
  }
	else{
    fileName = paste(rj@tmpFile, '.importance', sep='')
    if(!file.exists(fileName)) stop(paste('no importance file exists.',fileName))
		fileName2 = paste(rj@tmpFile, ".importance2", sep = "")
    if (!file.exists(fileName2)) stop(paste("no importance file exists.", fileName2))
    imp1<-read.table(fileName, header = TRUE)
    imp2<-read.table(fileName2, header = TRUE)
    imp<-cbind(imp1[order(imp1$id),], imp2[order(imp2$id),5:11])
  } 
  return(imp)
}
