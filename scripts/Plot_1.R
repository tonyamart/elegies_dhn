library(tidyverse)

# Load the dataset
elegies <- read_tsv("data/elegies_corpus.tsv")

#### Plot 1: Number of texts in the corpus ####

elegies %>% 
  group_by(year) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) + geom_col(fill = "#6699CC") +
  labs(x = "Year", 
       y = "Number of texts") + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black")) 
