### genome plot
### created 2022-04-01
### created by Roland Lacap

### load libraries
library(tidyverse)
library(here)
library(ggpubr)
library(scales)
library(ggplot2)
library(ggridges)
library(ggstream)

### load data

align <- read_csv(here("alignments.csv"))

### wrangle
align2 <- align %>% 
  pivot_longer(cols = 2:8,
               names_to = "Species",
               values_to = "E") %>% 
  mutate(percent = percent(E)) %>% 
  mutate(Species = recode(Species, 
                          'Chiloscyllium_griseum' = "Chiloscyllium griseum",
                          'Chiloscyllium_plagiosum' = "Chiloscyllium plagiosum",
                          'Chiloscyllium_punctatum' = "Chiloscyllium punctatum",
                          'Leucoraja_erinacea' = "Leucoraja erinacea",
                          'Mus_musculus' = "Mus musculus",
                          'Scyliorhinus_canicula' = "Scyliorhinus canicula")) %>% 
  filter(Species != "Gallus_gallus") %>% 
  mutate(Species = factor(Species,
                          levels = c("Mus musculus",
                                     "Chiloscyllium plagiosum",
                                     "Chiloscyllium punctatum",
                                     "Scyliorhinus canicula",
                                     "Leucoraja erinacea",
                                     "Chiloscyllium griseum")))

### plot
p <- align2 %>% 
ggplot(aes(x = GENE, y = E, fill = Species)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Species)+
  theme_pubr()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face = "bold"))+
  labs(x = " ", y = "Percent Identity")+
  coord_flip()+
  ggsave(here("alignments.png"))

p

p2 <- align2 %>% 
  ggplot(aes(y = GENE, x = E, group = GENE, height = E))+
  geom_density_ridges2(stat = "identity", scale = 1) +
  facet_wrap(~Species)

p2  
