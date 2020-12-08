library(tidyverse)
library(tidytext)
library(topicmodels)
library(pals)

# Load the dataset
elegies <- read_tsv("data/elegies_corpus.tsv")

#### Creating a model ####

# Define stopwords
stoplist <- c("и","в","во","не","что","он","на","я","с","со","как","а","то","все","она","так","его","но","ты","к","у","же","вы","за","бы","по","ее","мне","было","вот","от","меня","о","из","ему","теперь","даже","ну","ли","если","или","ни","быть","был","него","до","вас","нибудь","вам","сказал","себя","ей","может","они","есть","ней","для","мы","тебя","их","чем","была","сам","чтоб","без","будто","чего","себе","под","будет","ж","кто","этот","того","потому","этого","какой","ним","этом","мой","тем","чтобы","нее","были","куда","зачем","всех","можно","при","об","хоть","над","больше","тот","через","эти","нас","про","всего","них","какая","много","разве","сказала","три","эту","моя","свою","этой","перед","лучше","чуть","том","такой","им","более","всю","между","твой","весь")

# Prepare document-term matrix, stopwords excluded
dtm <- elegies %>% 
  mutate(doc_id = paste(id, year, sep = "_")) %>% 
  unnest_tokens(input = text_lem, output = word, to_lower = TRUE) %>%
  filter(!word %in% stoplist) %>% 
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)

# Create a model with 20 topics
mod_20 <- LDA(dtm, k = 20, method = "Gibbs",
              control = list(alpha = 0.5, delta = 0.1, 
                             iter = 2000, seed = 1234, thin = 1))

# Extract coefficients
beta20 <- tidy(mod_20, matrix = "beta") 
gamma20 <- tidy(mod_20, matrix = "gamma")

#### Plot: words probabilities in topics ####
beta20 %>% 
  group_by(topic) %>% 
  top_n(20, beta) %>% 
  ungroup() %>%
  arrange(topic, desc(beta)) %>% 
  ggplot(aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~topic, scales = "free") + 
  coord_flip() + 
  scale_fill_manual(values = as.vector(cols25(20))) +
  theme_minimal(base_size = 14) + 
  scale_x_reordered() + 
  labs(title = "Probability of top 20 words in topics",
       subtitle = "LDA model build with 'topicmodels' r package, 20 topics, alpha = 0,5, 2000 iterations") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
