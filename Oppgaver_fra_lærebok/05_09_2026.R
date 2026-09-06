#Oppgave d)
x <- c(2.0, 1.3, 6.0, 1.9, 5.1, 0.4, 1.0, 5.3, 15.7, 0.7, 4.8, 0.9, 12.2, 5.3, 0.6)
ml = (1/mean(x))
sum_x = sum(x) ; df = length(x) ; n = df

a = 0.1

l = qchisq(a/2,df*2) ; u = qchisq(1-a/2,df*2)
intervall = 1/(sum_x*2)*c(l,u)
print(intervall)

h_0 = 0.35
t = 2*h_0*sum(x)


p_verdi = pchisq(t,2*df)
print((1-p_verdi)*2)

o = 2*n*log(ml)-2*n*log(h_0)-2*(ml-h_0)*sum_x


x <- c(1, 6, 8, 2, 12, 1, 13, 8, 1, 2, 2, 3, 6, 3, 12, 2, 2, 3, 15, 7)
n = length(x)

sum_x = sum(x)
ml = n/sum(x)
print(ml)
p_0 = 0.2

s = -2*(sum(x)-n)*log((1-p_0)/(1-ml)) -2*n*log(p_0/ml)

p_verdi = pchisq(s,1)
print(s)
print(1-p_verdi)