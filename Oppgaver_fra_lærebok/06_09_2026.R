
fodlser = read.table("https://www.uio.no/studier/emner/matnat/math/STK1110/data/fodsler.txt", header = TRUE)

x = fodlser$Kjonn
y = fodlser$Fvekt
n = length(x)
S_xx = sum(x**2)-n*mean(x)**2
S_yy = sum(y**2)-n*mean(y)**2
S_xy = sum(x*y)-n*mean(x)*mean(y)
print(n)

print(c(Sxx = S_xx, Syy = S_yy, Sxy = S_xy))
colnames(fodlser)
model = lm(fodlser$Fvekt~ fodlser$SvDager + fodlser$Kjonn + fodlser$MorsAld + fodlser$Antfod)


summary(model)

qqnorm(y)

e = rep(1,n)
a = cbind(e, fodlser$SvDager, fodlser$Kjonn , fodlser$MorsAld, fodlser$Antfod)

b = solve((t(a)%*% a))%*% t(a) %*% y

est = t(a%*%b-y) %*% (a%*%b-y)
print((est/(n-4-1))**0.5)

