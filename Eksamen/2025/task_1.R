library(readxl)
library(tidyverse)

esc <- read_csv("esc.csv") #understrek gjør at vi leser det som en tibble

sprintf("Det er %s land som deltar, og %s land som gir poeng", nrow(esc),ncol(esc)-4)

esc_long <- esc |>
  pivot_longer(cols = -c("contestant"), #kjører gjennom alle kolonnene i det opprinnlige datasettet, med unntak av deltagerlandene
                names_to = "Awarding",
               values_to = "Score") |>
  
  dplyr::mutate(Score = replace_na(Score,0)) #setter inn 0 for na i score-vekroren

topp_score <- esc_long |>
  group_by(contestant)|>
  summarise(topp = (sum(Score == 12)), Jury = Score[Awarding == "jury_score"])
ggplot(topp_score) +
  geom_point(aes(x = topp, y = Jury)) +
  geom_label(aes(x = topp, y = Jury, label = contestant)) + labs(x = "Toppplaseringer", y = "Jurys score") +
  theme_bw()

reverse_tibble <- esc_long |>
  rename(contestant = Awarding,
         Awarding = contestant,
         reverse = Score)


esc_cor <- esc_long |>
  left_join(reverse_tibble) |>
  mutate(reverse = replace_na(reverse, 0)) |>
  filter(!Awarding %in% c("total_score","televote_score","jury_score")) |>
  filter(contestant== "Ukraine")

View(esc_cor)

ggplot(esc_cor) + geom_smooth(aes(x = Score, y = reverse)) + labs(x = "Score", y = "Reverse", title = "korrelasjon mellom score og reverse")

#impoerterer både datasettet og sheets
esc_exel <- read_excel("econ3170_4170_300126_esc.xlsx.xlsx", sheet = "Televote",skip = 2,col_names = T) #fjerner de to første kolonnene
  

  
  

esc_exel <- esc_exel |>
  dplyr::select(-ends_with("vote" )) |>
  dplyr::select(-ends_with("World")) |>
  rename(contestant = "...2",
         total_score = "...3",
         jury_score = "...4",
         televote_score = "...5") |>
    filter(!is.na(contestant))|> #fjerner aller ganger landene som blir stemt på er na
    
  pivot_longer(cols = -c("contestant"),
               names_to = "Awarding",
               values_to = "Score_tv") |>
  mutate(Score_tv = replace_na(Score_tv,0)) 

esc_total <- esc_long |>
  left_join(esc_exel) |>
  filter(!Awarding %in% c("total_score","televote_score","jury_score")) |>
  mutate(diff = Score-Score_tv) |>
  group_by(contestant) |>
  summarise(m = (mean(diff))) |>
  mutate(m = (m)) |>
  arrange(desc(m))

View(esc_total[1:5,])

View(esc_total)
