
library(rpart)
library(rpart.plot)
library(docstring)
data("ptitanic")

#' Vi har fire forklaringsvariabler = lengden og bredden på begerbladet, og legnden og breddden på kronbladet. 
#' Utifra dette skal vi avgjøre hvilken art vi har, for en fremtidig observasjon. 
#' Det er ikke en underliggende fordeling her, da dette er et klassifikasjonsproblem. 
#' 

#class brukes for klassifikasjonsproblemer
tree <- rpart(Species ~ ., data = iris, method = "class")


View(iris)

