### Boksplott over testformer

Det første vi ønsker å undersøke, er hvilken del av testen som er mest utfordrende for de ulike kontinentene. Dette gjør vi ved å lage et boksplott, der den sorte streken viser medianen for hver testform, mens boksene viser den midterste 50 % av observasjonene, altså spennet mellom 25%- og 75%-kvantilen. For å få et bedre innblikk i hvordan de ulike kontinentene presterer på de forskjellige testformene, har vi lagt datapunktene oppå boksplottene og gitt hvert kontinent en egen farge.

![Bilde](https://github.com/ragnhild-thielemann/ECON3170/blob/main/hva_er_vanskelig_1.png)

Amerika og Europa har gjennomgående observasjoner over 75%-kvantilen for alle testformene. Asia og Afrika har derimot større spredning, med datapunkter både over 75%-kvantilen og under 25%-kvantilen. 

## BNP i landet

Jeg har en hypotese om at dersom landet har lavere BNP per innbygger, så vil elevene score dårligere på engelsktestene. Jeg antar også at forskjellen er mer signifikant når de blir testet på lesing og skriving, enn når de blir testet på snakking og høring, da lesing og skriving er ferdigheter man i stor grad trenger skolegang for å kunne mestre. 

I tibbelen *bnp_i_landet* lager jeg to grupper for testen - en for lesing og skriving, og en for snakking og høring. 
