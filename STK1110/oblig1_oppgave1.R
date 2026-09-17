
library(docstring)
library(docstring)

data <- "https://www.uio.no/studier/emner/matnat/math/STK1110/data/forsikringskrav.txt"

# Leser datafilen og henter ut kolonnevektoren med forsikringsdata
forsikring <- read.table(data, header = FALSE)$V1
#--------------------------------------------------------------------------------------------
#Oppgave a)
#--------------------------------------------------------------------------------------------
#finner antall elementer i listen
n <- length(forskiring)

#finner førstemoment og andremoment
forste_moment <- sum(forskiring)/n
andre_moment <- sum(forskiring**2)/n

#beregner momentestimatene for forsikringsdataen
a_moment <- ((forste_moment**2)/(andre_moment-forste_moment**2))
b_moment <- ((forste_moment)/(andre_moment-forste_moment**2))

sprintf("a_m = %g og b_m = %g", a_moment,b_moment)

#' Dette gir oss følgende output når vi kjører koden: 
#' [1] "a_m = 0.696649 og b_m = 0.0288605"

#--------------------------------------------------------------------------------------------
#Oppgave b)
#-------------------------------------------------------------------------------------------


L <- function(x,alpha,gamma){ #x er en vektor av verdiene fra datasettet og a og b er paramterene vi har estimert
  logL = n*alpha*log(gamma) - n*lgamma(alpha) + (alpha-1)*sum(log(x)) - gamma*sum(x) #regner ut likhooden
  return(logL)
  }

#Regner log-likhooden til momentestimatorene, basert på forsikringspremiene
likhood_moment <- L(forskiring,a_moment,b_moment)


sprintf("Likhooden til momentestimatorene er %g", likhood_moment)

#' Dette gir oss følgende output når vi kjører koden
#' [1] "Likhooden til momentestimatorene er -27264.5"
#' 
#--------------------------------------------------------------------------------------------
#Oppgave d)
#-------------------------------------------------------------------------------------------


negloglikgamma <- function(logalpha,x = forskiring){#har forsirkingsdataen som datapunktene vi intragerer ovder
  n <- length(x) #finner antall observasjoner
  alpha <- exp(logalpha) #kommer tilbake til den opprinnlige verdien av alpha
  
  gamma <- alpha/mean(x) #gamma er en funskjon av alpha, som vist tidligere
  logL <- n*alpha*log(gamma) - n*lgamma(alpha) + (alpha-1)*sum(log(x)) - gamma*sum(x) #regner ut likhooden
  -logL 
  #'Da optim-funksjonen regner minumumsverdien, tar vi negativt fortegn foran likhooden. 
}

fit.ml <- optim(log(a_moment), negloglikgamma, x = forskiring, method = "BFGS")
#'BFGS er en modifisert versjon av Newtons metode, som bruker funksjonsverdier og gradienter til å minimere funksjonen
#'Vi bruker momentestimatoren for alpha, som vi regnet ut tidligere i oppgaven, som initialverdi

alpha_ml <- exp(fit.ml$par) 
#'Under optimeringen av ML-estimatoren bruker vi logatitemen til aplha, så vi må ta eksponenten av den optimerte verdien,
#'for å finne tilbake til sann verdi.  
#'Grunnen til at vi tar logaritmen, er for å hindre funksjonen i å optimere over negative verdier for alpha og beta, 
#'da dette er parametere som er strengt større enn null.
#'Ved å ta logatitmen av startbetingelsene, sikrer vi at den bare optimerer over positive verdier. 

gamma_ml <- alpha_ml/mean(forskiring)
#' Bruker ML-estimatoren ved utregningen av verdien for gamma. 


likhood_ml <-L(forskiring,alpha_ml,gamma_ml) #finner likhoodsverdien for parameterparret (alpha_ml, gamma_ml)

sprintf("ML-estimatorene er a_ml = %g og b_ml %g", alpha_ml, gamma_ml)

differanse <- likhood_ml-likhood_moment


sprintf("Loglikhooden for ML-estimaotorene er %g , mens den er %g for momentestimatene. Det gir en differeanse pa %g , der loglighooden for ml-estimatene er strt", likhood_ml,likhood_moment,differanse)

#'Dette gir følgene output når vi kjører koden
#'ML-estimatorene er a_ml = 1.38835 og b_ml 0.0575159
#'
#'Loglikhooden for ML-estimaotorene er -26488.3 , mens den er -27264.5 
#'for momentestimatene. Det gir en differeanse pa 776.233 , 
#'der loglighooden for ml-estimatene er størst"
#'