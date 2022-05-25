### Shark Soma size
### Created by Roland Lacap
### Created on 2022-01-07
### Updated on 2022-05-24

###This is an updated version of a previous script containing the drg and tg
###measurements of bamboo shark and cat shark. This contains two different dev stages.

### Load libraries
library(tidyverse)
library(here)
library(ggpubr)
library(report) 
library(hablar)

### Load data
master <- read_csv(here("drg_measurements_new","chilo","20220524_masterlist.csv"))

### statistics
master <- master %>%
  filter(exp != "tg 1 ngn1-594",
         exp != "4 tg 26 3 20x 3",
         exp != "4 tg 26 2 20x 1") %>% 
  mutate(avg_dia = x+y/2) %>%   #create average diameters between x and y
  filter(area <= 500,
         area >= 10) %>% 
  select(1:5,8,9) %>% 
  mutate(dev_stage = recode(dev_stage,
                           'Pre-hatch' = "Hatched",
                           '32' = "Stage 32")) %>% 
  mutate(dev_stage = factor(dev_stage,
                            levels = c("Stage 32",
                                       "Hatched"))) %>% 
  unique() #remove any duplicates
  
duplicates <- master %>% #finding any duplicates
  find_duplicates() %>% 
  unique()
  
# average
cell_ave <- master %>% 
  group_by(tissue,dev_stage) %>% 
  summarise(avg = mean(area),
            sd = sd(area),
            var = var(area),
            med = median(area))

# t-test
area_ttest <- master %>% 
  select(3,4,5) %>% 
  group_by(tissue,dev_stage) %>% 
  report() %>% 
  as.data.frame()

### plot data

area_p <- master %>% 
  gghistogram(x = "area", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 30)+
  labs(x = "Soma Size (μm2)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage)+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 2),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 2),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))
area_p

snow_p <- master %>% 
  gghistogram(x = "avg_dia", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Avg. Diameter (μm)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage)+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 2),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 2),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))
snow_p

peri_p <- master %>% 
  gghistogram(x = "perimeter", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Perimeter (μm)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage)+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 2),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 2),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2))

peri_p

### per experiment
area_p2 <- master %>% 
  gghistogram(x = "area", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 60)+
  labs(x = "Soma Size (μm2)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~exp+dev_stage, scales = "free")
  #theme(axis.title.x = element_text(size = 30, face = 2),
        #axis.text.x = element_text(size = 24, face = 2),
        #axis.title.y = element_text(size = 30, face = 2),
        #axis.text.y = element_text(size = 24, face = 2),
        #legend.text=element_text(size = 24, face = 2),
        #legend.title = element_text(size = 24, face = 2))
area_p2

