library(tidyverse)

library(e1071)
library(caret)

library(tidymodels)
library(textrecipes)
library(kernlab)

library(rcartocolor)

#### Data load ####
z_test <- read_tsv("data/MFW50-300_elegies.tsv") # z-scaled frequencies from the corpus of elegies and Russian National corpus (1815-1835)

# The other files with frequencies are: 
# MFW50-300_iambic.tsv -- subset of only iambic poems from the elegies and Russian National corpora
# MFW50-30_iamb4.tsv -- subset of poems wriiten only in iamb-4 from elegies and Russian National corpora
# MFW50-300_russongs.tsv -- frequencies from the corpus of russian songs (Artjom Šela's dataset, see: https://github.com/perechen/russian.songs.fin.thesis ) and subset of Russian national corpus (1820-1848)
# the present code is based on the more detailed tutorial with same genre classification issue discussed: https://github.com/perechen/tartu_dm_2020 

## classes (stored as text_genre column):
# EL -- an elegy from the elegies corpus
# NE -- not an elegy (context material taken from the Russian National corpus)
# RS -- 'Russian song'
# NS -- not a 'Russian song' (context material taken from the Russian National corpus)

# check classes:
unique(z_test$text_genre)

#### Classification with tidymodels ####
z_test$text_genre %>% table()

tidy_corpus <- z_test %>% 
  group_by(text_genre) %>% 
  sample_n(386) %>%   ## this parameter should be set according to the size of the class with smallest number of observations
  ungroup() %>% 
  select(-c(text_id))

## splitting the data into training and test sets
corpus_split <- initial_split(tidy_corpus %>% select(-uniq_id), strata="text_genre", prop = 7.5/10)

training_set <- training(corpus_split)
test_set <- testing(corpus_split)

# preprocessed data used in the recipy (all columns)
zscores_recipe <- recipe(text_genre ~ ., data = training_set)

# 10-fold cross validation setup
folds <- vfold_cv(training_set, strata = "text_genre", v = 10)

# model specifications
svm_specs <- svm_poly(cost=1,degree = 1) %>% # linear kernel
  set_mode("classification") %>%
  set_engine("kernlab")

# add recipe and model specs to the workflow 
svm_wf <- workflow() %>%
  add_recipe(zscores_recipe) %>%
  add_model(svm_specs)

# fitting SVM to the training set
svm_res <- fit_resamples(
  svm_wf,
  folds,
  metrics = metric_set(accuracy),
  control = control_resamples(save_pred = TRUE)
)

# folds mean accuracy
metrics = collect_metrics(svm_res)
metrics 

# test set
test_res <- svm_wf %>%
  last_fit(corpus_split, metrics = metric_set(accuracy))

last_fit_metrics = collect_metrics(test_res)
last_fit_pred = collect_predictions(test_res)

last_fit_metrics

last_fit_pred %>%
  conf_mat(text_genre, .pred_class) %>% 
  autoplot(type = "heatmap")

#### e1071 settings used to compare with tidymodels results ####

# NB test & train set should be initialized before

svm_model <-svm(as.factor(text_genre)~.,  
                data = training_set, 
                method = "C-classification", 
                kernel = "linear", 
                cost = 1, 
                scale = T)

# model parameters
summary(svm_model) 
prediction <- predict(svm_model, test_set)
confusionMatrix(prediction, as.factor(test_set$text_genre)) # NB check if the same positive class used below in the plot

words_coefs <- t(svm_model$coefs) %*% svm_model$SV

#### Plot 4 ####

tibble(weight=words_coefs[1,], word=colnames(words_coefs)) %>% 
  mutate(genre = case_when(weight > 0 ~ "Elegies", 
                           weight < 0 ~ "Non elegies")) %>%
  group_by(genre) %>% 
  mutate(abs=abs(weight)) %>%
  top_n(20,abs) %>% 
  ggplot(aes(reorder(word,abs), abs, fill=genre)) + geom_col() +
  coord_flip() + 
  facet_wrap(~genre,scales="free") +
  theme_minimal(base_size = 16) + 
  labs(x = "", 
       y = "",
       fill = "") + 
  scale_fill_carto_d(palette = "Safe") + 
  theme(legend.position = "none") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14)) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black"))
