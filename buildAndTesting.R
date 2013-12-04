library(devtools)
library(roxygen2)
install_github("Rjungle", username = "jkruppa")

## ------------------------------------------------------------
## by J.Kruppa on Donnerstag, November 28, 2013 (10:06)

rjungleDir <- file.path("rjungle")
roxygenize(rjungleDir)

##load_all(rjungleDir)
document(rjungleDir)
build(rjungleDir)

## ------------------------------------------------------------
## by J.Kruppa on Mittwoch, Dezember  4, 2013 (15:22)
library(randomForest)
library(Rjungle)
data(iris)

test <- rjungle("Species", iris, importance = 2, verbose = TRUE)
importance(test)
