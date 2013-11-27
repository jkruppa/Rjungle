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


library(MASS)
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param rj 
##' @return 
##' @author Jochen Kruppa
plotMDS <- function(rj) {
  if (!rj@proximity) stop(RJ__MSG2);
  
  prefix = rj@tmpFile;

  # gunzip file
  fileName2 = paste(prefix, ".samproximity", sep = "");
  fileName = paste(fileName2, ".gz", sep = "");
  
  if (file.exists(fileName)) {
    if (file.exists(fileName2)) {
      system(paste("rm", fileName2));
    }
    system(paste("gunzip", fileName));
  } else {
    if (!file.exists(fileName2)) stop(RJ__MSG2);
  }
  


  # read data
  fileName = paste(prefix, ".samproximity", sep = "");
  data = matrix(scan(fileName, quiet = T), length(scan(fileName, nlines = 1, quiet = T)))
  data = data / max(data);
  diag(data) = 1
  
  # MDS
  fit <- cmdscale(1-data,eig=TRUE, k=2) # k is the number of dim
  #print(fit) # view results
  
  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  
  # function: make density plot
  kde2dplot <- function(d,                # a 2d density computed by kde2D
                        ncol=50,          # the number of colors to use 
                        zlim=c(0,max(z)), # limits in z coordinates 
                        nlevels=20,       # see option nlevels in contour 
  		      theta=30,         # see option theta in persp
  		      phi=30)           # see option phi in persp
  		      {
    z   <- d$z
    nrz <- nrow(z) 
    ncz <- ncol(z) 
    
    couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol) 
    fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1] 
    dim(fcol) <- c(nrz,ncz) 
    fcol      <- fcol[-nrz,-ncz]
    
    par(mfrow=c(1,2),mar=c(0.5,0.5,0.5,0.5)) 
    persp(d,col=fcol,zlim=zlim,theta=theta,phi=phi,zlab="density") 
    
    par(mar=c(2,2,2,2)) 
    image(d,col=couleurs) 
    contour(d,add=T,nlevels=nlevels) 
    box() 
  }

  # plot  
  d <- kde2d(x,y,n=20) 
  kde2dplot(d)
}


# make2DPlot = function(prefix) {
# 
#   # gunzip file
#   fileName = paste(prefix, ".samproximity.csv.gz", sep = "");
#   
#   if (file.exists(fileName)) {
#     fileName2 = paste(prefix, ".samproximity.csv", sep = "");
#     if (file.exists(fileName2)) {
#       system(paste("rm", fileName2));
#     }
#     system(paste("gunzip", fileName));
#   }
# 
#   # read data
#   fileName = paste(prefix, ".samproximity.csv", sep = "");
# 
#   proxi = matrix(scan(fileName), length(scan(fileName, nlines = 1)))
#   proxi = proxi / max(proxi);
#   diag(proxi) = 1
#   
# 
#   #library(heatmap.plus)
#   #heatmap.plus(proxi)
#   image(proxi)
# }
