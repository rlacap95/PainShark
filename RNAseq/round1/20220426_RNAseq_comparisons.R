### FPKM comparison Mus, Uh, and Gg
### Created by Roland Lacap
### Created on 04-26-2022

library(tidyverse)
library(here)
library(ggpubr)
library(PNWColors)

### Load data
mouse <- read_csv(here("RNAseq","round1","data","Mus_fpkm_pain2.csv"))
stingray <- read_csv(here("RNAseq","round1","data","Uh_fpkm_pain.csv"))

### Wrangle data
mouse <- mouse %>% 
  select(2,9,14)

stingray <- stingray %>% 
  select(2,3,4) %>% 
  mutate(Markers = recode(Markers,
                          "TrpC3" = 'Trpc3',
                          "Nav1.8" = 'Scn10a',
                          "TrkA" = "Ntrk1",
                          'PlxnC1' = "Plxnc1",
                          'RET' = "Ret",
                          'SST' = "Sst",
                          'TrpA1' = "Trpa1")) %>% 
  filter(Markers != "KIT") %>% 
  filter(Markers != "Gpsm3") %>% 
  filter(Markers != "Nefm1") %>% 
  filter(Markers != "Nefm2") %>% 
  filter(Markers != "Nefm3")

master <- mouse %>% 
  full_join(stingray) %>% 
  pivot_longer(cols =2:5,
               names_to = "species",
               values_to = "FPKM") %>% 
  mutate(species = recode(species,
                          'DRG_ave' = "Mouse DRG",
                          'TG_ave' = "Mouse TG",
                          'Uh_1_DRG' = "Stingray DRG",
                          'Uh_2_Tg' = "Stingray TG")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(species = factor(species, levels =
                            c("Mouse TG", "Mouse DRG",
                              "Stingray TG", "Stingray DRG"))) %>% 
  write_csv(here("RNAseq","round1","data","20220426_Master_fpkm.csv"))

### Plot data
pal <- pnw_palette("Shuksan")
pal2 <- pnw_palette("Winter")

p <- master %>% 
  ggbarplot(x = "Markers", y = "FPKM", fill = "species",
            legend.title = "Species") +
  facet_wrap(~species)+
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=1, vjust = .5,
                                   size = 24, face = 2),
        axis.title.x = element_text(size = 30, face = 2),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 2),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))+
  coord_flip()+
  scale_fill_manual(values = pal) +
  theme_get()+
  labs(x = " ", y = "FPKM")+
  guides(fill = FALSE)

p

### Z-score
masterz <- master %>% 
  mutate(species2 = recode(species,
                           "Mouse DRG" = 'Mouse',
                           "Mouse TG" = 'Mouse',
                           "Stingray DRG" = 'Stingray',
                           "Stingray TG" = 'Stingray')) %>% 
  #group_by(species) %>% 
  mutate(z = (FPKM-mean(FPKM))/sd(FPKM)) 

masterz2 <- masterz %>% 
  mutate(levels = factor(ifelse(z < 0, "low", "high"),
                         levels = c("low","high"))) %>% 
  mutate(levels = as.character(levels)) %>% 
  mutate(species = factor(species, levels =
                            c("Mouse DRG","Stingray DRG",
                              "Mouse TG","Stingray TG")))

pz <- masterz2 %>%
  ggbarplot(x = "Markers", y = "z",
            fill = "species",
            legend.title = "Species",
            ylab = "z-score")+
  facet_wrap(~species, ncol=2)+
  coord_flip()+
  scale_fill_manual(values = pal2)+
  theme_get()

pz
