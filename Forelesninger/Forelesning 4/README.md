---
title: "README"
output: html_document
---

#Visualisering 

## Å visualisere

Grafer er *informative* , og *bryter_opp_teksten*. Grafer hjelper deg å forstå datasettet du jobber med, samt å formidle funnene dine vidre. 
- Save on inc (gjør at motaker bare leser den informasjonen som er strengt nødvendig)

## Motaker og medie

Når man lager grafer, må man både tenke på motakergruppen, samt mediet grafen skal visualiseres i. 
- Være oppmerksom på motakers evne til å lese grafer kritisk.
- Ved presentasjoner bør man ha enkle grafer, som er enkle å lese. Ved skriflige kilder kan man ha mer komplekse grafer, da et bilde sier mer enn 1000 ord. 


## Feller

- Å bruke **3D** gjør at det er enkelt å la seg lure. Tilfører ikke ny informasjon, og kan være misvisende. 
  - Vi ser på 2-dimensjonale flater når vi observerer dataen
- Misvisende akser
  - Gjør at proposjonaliteten mistes. 
    - Gjør observasjonene blir mer eller mindre ekstreme
    - Noen mener man bør starte på 0, men dette blir også misvisende
    
- **Colors**
  - Estetisk fint
  - Induitiv bruken av farger. Oppmerskom på fargeblindhet. 
  
## Best practice (kroneksemplarene på hvordan det skal gjøres)

- Nærhet
  - Det vi ønsker å sammenligne bør være nærme hverandre.
    - Ha søylen for kvinner og menn i samme aldersgruppe ved siden av hverandre
    
- Aksetitler
  - Ha like akser på alle plott, slik at det ikke blir misvisende når man skal sammenligne.

- Punktene må være enkle å skille fra hverandre (ulike farger, eller tydlig forskjeller i formen på punktene)

## Noen sentrale grafer

### Søylediagram

### Scatter plot

- Plotter rådata for å finne bivariate avhenignheter
- Kan lage en regresjonslinje for å gjøre sammenhengen klarere
- Kan gi rare utfall for diskrete variabler, så bør brukes på *kontinuelige* variabler

### Linjediagram

- Time-series-plot. Viser utviklingen over tid. 
- Det samme som et scatter-plot, men viser utvikling over tid. 
  - Bør ikke brukes dersom dataen ikke er kontunuelig. 
    - Kan legge til infomasjon om ting som har skjedd på ulike tidspunkter, for å forklare grafen. 
      - Tekstbokser som forklarer situasjoner i verden over et tidsserieplott over renten, for å forklare svigningenen. 
- **Spagettiplott**
  - Hver linje er et år - viser hvordan ting gjentar seg periodisk gjennom et år. Kan brukes til å vise at borligprisene varierer periodisk. 
  - **Florence_Nigthingale_plot** 

### Kakediagram

- Viser andel av en helhet
  - Bare få kategorien
  - Viser ikke sammenligninger
  - Bør heller bruke søylediagram
  
## Univariate plot

### Histogram
- y-aksen er antall observasjoner

### Tetthetsplot
- Smooth

### Box-plot og fiolinplot

- Box-plot viser medianen, og hvordan den er spredt ut/ konsentrert. 
- Fiolinplot 
  - Massen viser hvor størstedelen av observasjonene ligger

### Maps

- Bryter opp lange tekster


# ggplot2

## Grunnpilarer i ggplot

1. plotet er linket til variablene ("the data")
2. vi representerer dataen gjennom ulike **geom**
3. bygger plottene i lag
  - Holde konsante aksetitler. Derfor bør disse defineres i ggplot(data = data, mapping = aes(x = x, y = y))

