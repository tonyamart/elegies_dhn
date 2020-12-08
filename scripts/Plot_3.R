library(tidyverse)

# Load the dataset
elegies <- read_tsv("data/elegies_corpus.tsv")

# Calculate means and medians for each year
lin_mean <- elegies %>% 
  group_by(year) %>% 
  summarise(mean = mean(n_lines), 
            med = median(n_lines)) 

# Testing the normality
qqnorm(lin_mean$med)
qqline(lin_mean$med)

qqnorm(lin_mean$mean)
qqline(lin_mean$mean)

# Correlation
cor.test(lin_mean$mean, lin_mean$year, method = "pearson")
cor.test(lin_mean$med, lin_mean$year, method = "pearson")

# Plots

# Mean
lin_mean %>% 
  ggplot(aes(x = year, y = mean)) + 
  geom_smooth(method = 'lm', color = '#6699CC', alpha = 0.2) + 
  geom_point(color = "#6699CC", size = 2) +
  labs(x = 'Year', 
       y = 'Number of lines', 
       title = '  Mean number of lines') + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16)) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black")) 

# Median
lin_mean %>% 
  ggplot(aes(x = year, y = med)) + 
  geom_smooth(method = 'lm', color = '#882255', alpha = 0.2) + 
  geom_point(color = "#882255", size = 2) +
  labs(x = 'Year', 
       y = "", 
       title = '  Median number of lines') + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16)) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black")) 
