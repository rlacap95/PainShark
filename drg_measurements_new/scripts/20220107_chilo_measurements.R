### Chilo Soma size
### Created by Roland Lacap
### Created on 2022-01-07

###This is an updated version of a previous script containing the drg and tg
###measurements of Chiloscyllium punctatum. This contains two different dev stages.

### Load libraries
library(tidyverse)
library(here)
library(ggpubr)
library(report)

### Load data
master <- read_csv(here("drg_measurements_new","chilo","20220308_masterlist.csv"))

### statistics
master <- master %>% 
  mutate(avg_dia = x+y/2) #create average diameters between x and y

cell_ave <- master %>%  
  group_by(tissue) %>% 
  summarise(avg = mean(area),
            sd = sd(area),
            var = var(area),
            med = median(area))

# t-test
area_ttest <- master %>% 
  select(3,4) %>% 
  group_by(tissue) %>% 
  report() %>% 
  as.data.frame()

### plot data

area_p <- master %>% 
  gghistogram(x = "area", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 60)+
  labs(x = "Soma Size (μm2)", y = "Number of cells")+
  theme_classic()+
  #facet_wrap(~dev_stage)+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 2),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 2),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2))
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
        legend.title = element_text(size = 24, face = 2))
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

