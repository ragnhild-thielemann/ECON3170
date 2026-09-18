
library(docstring)

#--------------------------------------------------------------------------------------------
#Oppgave a)
#--------------------------------------------------------------------------------------------
# lager en vektor med de målte verdiene for blåbærene
blueberry <- c(525, 587, 547, 558, 591, 531, 571, 551, 566, 622, 561, 502, 556, 565, 562)
 
n <- length(blueberry)
mu_emp <- mean(blueberry) #finner empirisk gjennomsnitt
S_2 <- var(blueberry) #finner empirisk varians
a <- 0.05
q <- qt(1-a/2,n-1) #finner kvantilen fra t-fordelingen 
#når vi lager kofedensintervallet, bruker trekker vi fra en t-fordeling med (n-1) = 14 frihetsgrader

intervall <- mu_emp + c(-sqrt(S_2/n)*q, sqrt(S/n)*2 )
print(c(mu_emp <- mu_emp, S <- sqrt(S_2)))
print(intervall)
#' Vi får estimatene mu = 559.67 og S = 28.55
#'[1] "Vi far at et 95% koefedensintervall er [543.85, 574.4149]"


#--------------------------------------------------------------------------------------------
#Oppgave b)
#--------------------------------------------------------------------------------------------


terms <- 6700
n <- 15
mu <- 558
sigma <- 30

antall_ganger <- 0
for (t in 1:terms){
  datasett <- rnorm(n,mu,sigma)
  mu_boot <- mean(datasett)
  a <- 0.05
  u <- mu_boot + qnorm(1-a/2)*sigma/sqrt(n) 
  l <- mu_boot - qnorm(1-a/2)*sigma/sqrt(n)
  if (between(mu,l,u)){
    antall_ganger <- antall_ganger + 1
  }
}
andel <- (antall_ganger/terms)
print(andel)
#'Vi får at sann verdi av  mu (558) er innenfor koefedensintervallene i 


#--------------------------------------------------------------------------------------------
#Oppgave c)
#--------------------------------------------------------------------------------------------


terms <- 6700 #antall ganger vi kjører simueringen
n <- 15
mu <- 558
sigma <- 30
storre <- 0 #antall ganger feilen for normalfordelignen er større enn for t-fordelingen
antall_ganger_n <- 0 #antall ganger sann verdi for mu er i intervallet for normalfordelingen

antall_ganger_s <- 0 #antall ganger sann verdi for mu er i intervalllet for tfordelingen
for (t in 1:terms){
  datasett <- rnorm(n,mu,sigma)
  mu_boot <- mean(datasett)
  S <- sd(datasett) #finner empirisk standardavvik
  a <- 0.05 #signifikansnivå
  
  error_n <-  qnorm(1-a/2)*sigma/sqrt(n) #feilen for normalfordelingen
  error_s <-  qnorm(1-a/2)*S/sqrt(n) #feilen for t-fordelingen
  
  #beregner intervallene
  u_n <- mu_boot + error_n ;l_n <- mu_boot - error_n
  u_s <- mu_boot + error_s ; l_s <- mu_boot - error_s
  
  #skjekker om sann verdi for mu er i intervallene
  if (between(mu,l_s,u_s)){
    antall_ganger_s <- antall_ganger_s + 1}
  
  if (between(mu,l_n,u_n)){
    antall_ganger_n <- antall_ganger_n + 1
  }
  #teller antall ganger bredden på koefedensintervallet for normalfordelingen
  #er større enn bredden på koefedensintervallet for t-fordelingen
  if (error_n > error_s) {
    storre <- storre + 1
  }
  }
  
  
  

n_storre <- storre/terms  #finner andelen av ganger der bredden på koefidensintervallet for normalfordelingen
# er større enn for t-fordelingen
print(n_storre)
andel_t <- (antall_ganger_s/terms)  #andelen av intervallene for t-fordelingen som inneholder sann verdi av mu
print(andel_t)
andel_n <- (antall_ganger_n/terms)  #andelen av intervallene for normal-fordelingen som inneholder sann verdi av mu
print(andel_n)
