
View(performance_by_first_language)
library(countrycode)


hva_er_vanskelig <- performance_by_nationality|>
  mutate(kontinent = countrycode(sourcevar = nationality,
                         origin = "country.name",
                         destination = "continent"))|>
  
  ggplot(aes(x = part, y = score,color = kontinent)) +
  geom_point() + 
  
  geom_boxplot(aes(group = part)) + 

  labs(x = "Del av  testen", y = "Score")
hva_er_vanskelig

ggsave("hva_er_vanskelig.png", plot = hva_er_vanskelig)