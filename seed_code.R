#In Fall 2024, the Mycology class at NAU did experiments supervised by Dr. Kitty Gehring and PhD candidate and TA, Beatrice Bock.
#This code contains analysis for the results of a seed sterilization experiment of Sorghum bicolor seeds.

#Libraries
library(ggbeeswarm)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(janitor)
library(readxl)
library(EnvStats)
library(glmmTMB)

theme_set(theme_bw())

#Analysis

seeds <- read_excel("Seeds_Sterilization_Protocol.xlsx", sheet = 1)%>%
  clean_names()%>%
  mutate(Germinated = number_seeds_germinated/number_seeds_planted,
         Contaminated = number_seeds_contaminated/number_seeds_planted)

#Plotting
seeds %>%
  pivot_longer(cols = c("Germinated", "Contaminated"), names_to = "type", values_to = "number") %>%
  mutate(type = recode(type,
                       "Germinated" = "Germination",
                       "Contaminated" = "Contamination")) %>%
  ggplot(aes(x = type, y = number, fill = type)) +
  geom_boxplot(color = "gray30", fill = NA, width = 0.6)+
  geom_quasirandom(aes(color = type), shape = 4, alpha = 0.5,
                   size = 3.2, stroke = 1.1, groupOnX = TRUE)+
  theme(legend.position = "none", axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("Germination" = "#E69F00", "Contamination" = "#56B4E9")) +
  scale_color_manual(values = c("Germination" = "#E69F00", "Contamination" = "#56B4E9")) +
  labs(y = "Proportion of Seeds", x = "Seed Outcome") +
  stat_n_text()

ggsave("seeds_results.png", width =5, height = 4, dpi = 300)

#Stats assumptions

ggplot(seeds, aes(x = Germinated)) + 
  geom_histogram(binwidth = 0.2) + 
  ggtitle("Distribution of Germination Proportions")
#Surprisingly perfectly normal.

ggplot(seeds, aes(x = Contaminated)) + 
  geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of Contamination Proportions")
#Values are either 0 or 0.2 (1). Difficult to do stats on.

counts <- seeds %>%
  select(name, number_seeds_planted, number_seeds_germinated, number_seeds_contaminated) %>%
  summarise(
    n_groups = n(),
    
    # Germination
    mean_germ = mean(number_seeds_germinated),
    median_germ = median(number_seeds_germinated),
    sd_germ = sd(number_seeds_germinated),
    min_germ = min(number_seeds_germinated),
    max_germ = max(number_seeds_germinated),
    
    # Contamination
    mean_contam = mean(number_seeds_contaminated),
    median_contam = median(number_seeds_contaminated),
    sd_contam = sd(number_seeds_contaminated),
    min_contam = min(number_seeds_contaminated),
    max_contam = max(number_seeds_contaminated)
  )%>%
  t()

percents <- seeds %>%
  select(name, Germinated, Contaminated) %>%
  summarise(
    n_groups = n(),
    
    # Germination
    mean_germ = mean(Germinated),
    median_germ = median(Germinated),
    sd_germ = sd(Germinated),
    min_germ = min(Germinated),
    max_germ = max(Germinated),
    
    # Contamination
    mean_contam = mean(Contaminated),
    median_contam = median(Contaminated),
    sd_contam = sd(Contaminated),
    min_contam = min(Contaminated),
    max_contam = max(Contaminated)
  )%>%
  t()

results <- cbind(counts, percents)
  
colnames(results) <- c("counts", "percents")

write.csv(results, "results.csv")
