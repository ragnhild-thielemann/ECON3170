
x = matrix(c(1,-1,-1,
             1,-1,1,
             1,1,-1,
             1,1,1),ncol = 3,byrow = TRUE)
y = matrix(c(1,1,0,4))
y_1 = c(1,1,0,4)

x_1 = c(-1,-1,1,1)
x_2 = c(-1,1,-1,1)


model = lm(y_1~x_1 + x_2)


print(x)
x_t = t(x)


b = solve(x_t %*% x) %*% x_t %*% y
print(b)
h = x %*% solve(x_t %*% x)  %*% x_t
E = x %*%b 
print(E)
error = (h-diag(4)) %*% y_1

a = (error**2)
print(sum(a))

S_yy = sum(y_1 **2 ) - mean(y_1)**2*4

print(S_yy)
x_xT = t(x) %*% x
print(x_xT)
summary(model)