library(shiny)
library(tmap)


data(Europe)
data <- read.table("./www/data.csv", sep = ",", check.names=FALSE,dec = ".", header= TRUE) 
nuts0<- read_shape('./shapes2/nuts0.shp')
nuts2<- read_shape('./shapes2/nuts2.shp')









