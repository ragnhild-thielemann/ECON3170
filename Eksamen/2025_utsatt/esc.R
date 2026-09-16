
library(tidyverse)
esc <- read_csv("esc.csv")

headers <- c(colnames(esc),"rest")
#antall deltagere er antall rader
deltagere <- nrow(esc) 
awarding <- ncol(esc) - 4

sprintf("Det er %g deltagere og %g land som gir poeng", deltagere,awarding)

esc_long <- esc |>
  pivot_longer(cols = -contestant, #skal utføre på alle kolonner som ikke 
               names_to = "Awarding", #Navnene på kolonnene går til kolonen awarding
               values_to = "Score") |> #verdiene går til score
  mutate(Score = replace_na(Score,0)) #bytter ut de manglende verdiene med 0



tolvere <- esc_long |>
  mutate(tolv = if_else(Score == 12, 1,0))|>
  summarise(tolv = as.numeric(sum(tolv)), jury = (Score[Awarding == "jury_score"]),.by = contestant) |>#lager da en tibble med to kolonner
  mutate(tolv = (tolv))
ggplot(tolvere,aes(x = tolv, y = jury)) + 
  geom_point() +
  geom_label(aes(label = contestant))+ 
  labs(x = "Antall tolvere", y = "Juryens score" , title = "Korrelasjon mellom antall 12-ere og juryevns score") +
  theme_bw()
  

uten_total <- esc_long |>
  filter(!Awarding %in% c("total_score","jury_score","televote_score"))|>
  filter(Awarding %in% contestant)

nrow(uten_total)

reverse <- uten_total |>
  rename(contestant = Awarding, 
         Awarding = contestant,
         reverse_score = Score) |>
  left_join(uten_total)|>
  arrange(contestant) |>
  ggplot(aes(Score, reverse_score)) + 
  geom_smooth() + 
  labs(x = "Score", y = "Reverse_score" , title = "Korrelasjon mellom gitt og motatt score") 
  
reverse


library(readxl)

#leser inn excelfilen
esc_exel <- read_xlsx("esc1.xlsx", sheet = "Televote")
esc_exel <-esc_exel[4:nrow(esc_exel),2:ncol(esc_exel)] 
names(esc_exel) <- headers
View(esc_exel)
esc_exel <- esc_exel |>
  mutate(across(-contestant, as.numeric))


#Da de er sortert på samme måte, kan jeg bruke overskriftene fra det pene datasettet i det nye

View(esc_exel)

View(esc_exel)
colnames(esc_exel)
long_exel <- esc_exel |>
  pivot_longer(cols = -contestant, #skal utføre på alle kolonner som ikke 
               names_to = "Awarding", #Navnene på kolonnene går til kolonen awarding
               values_to = "Score_televote") |>
  mutate(Score_televote = replace_na(Score_televote,0))|>
  left_join(esc_long)|>
  mutate(diff = abs(Score_televote - Score))|>
  filter(!Awarding %in% c("total_score","jury_score","televote_score") ) |>
  summarise(m = mean(diff,na.rm = TRUE), .by = contestant)



View(long_exel)
