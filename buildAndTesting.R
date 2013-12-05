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
library(Rjungle)
data(iris)
iris$Species = as.integer(iris$Species)

rjungleTest <- rjungle("Species", iris, importance = 3, verbose = TRUE)
importance(rjungleTest)

predict(rjungleTest, iris)


library(randomForest)
iris.rf <- randomForest(as.factor(Species) ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
## Look at variable importance:
importance(iris.rf)

## Regression:
data(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:

