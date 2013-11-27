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


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param iterations 
##' @param options 
##' @param ... 
##' @return 
##' @author Jochen Kruppa
impute <- function(iterations = 1, options = "", ...
  ) {
  
  options = paste(options, "-I", iterations)
  rj = rjungle(options = options, ...)

  prefix = rj@tmpFile;

  # gunzip file
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

