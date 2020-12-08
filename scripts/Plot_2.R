library(tidyverse)
library(topicmodels)
library(pals)

# Load beta and gamma values from the model
beta20 <- read_tsv("data/TM_beta.tsv")
gamma20 <- read_tsv("data/TM_gamma.tsv")

# a table from beta values to extract top words in each topic
top_words <- beta20 %>%
  group_by(topic) %>%
  top_n(5, beta) %>% # n top words
  arrange(-beta) %>% # sort by probability
  summarise(labels = paste(term, collapse=" "))

# make a label out of words
top_words <- top_words %>%
  unite_("fullname", c("topic", "labels"), sep = " ") 

# aggregate mean gamma values for each year
gamma_aggr <- gamma20 %>% 
  separate(document, c("n", "year"), sep = "_", convert = TRUE) %>% 
  group_by(year, topic) %>% 
  summarise(aggr = mean(gamma))

# a plot with a column for each 3 years period
gamma_aggr %>% 
  mutate(year_3 = floor(year/3)*3) %>%
  ggplot(aes(x = as.factor(year_3), y = aggr)) +
  geom_col(aes(fill = factor(topic))) +
  labs(х = " ",
        y = "Aggregated probability of the topics",
        fill = "    5 most probable words in topic") +
  scale_fill_manual(values = as.vector(cols25(20)), 
                    labels = top_words$fullname) +
  theme(panel.grid = element_blank()) + 
  theme(axis.title.x = element_blank()) + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12)) + 
  guides(fill = guide_legend(ncol = 2)) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black")) 
