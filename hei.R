

library(GGally)
library(Matrix)
?Matrix

data <- "https://www.uio.no/studier/emner/matnat/math/STK1110/data/fodsler.txt"

fodsler <- read.table(data, header = TRUE)

model <- lm(Fvekt~., data = fodlser)

summary(model)

y <- fodlser$Fvekt
n  = length(y)

x <- cbind(fodlser$Kjonn,fodlser$SvDager,fodsler$MorsAld, fodlser$Antfod)

b <- solve(t(x)%*% x) %*% t(x) %*% y

y_pred <- x %*% b

error <- sum((y - y_pred)**2)

print(sqrt(error/(n-ncol(b)-1)))

a = solve((t(x)%*% x))
a