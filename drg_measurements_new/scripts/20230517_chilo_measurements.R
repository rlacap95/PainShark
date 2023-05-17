### Shark Soma size
### Created by Roland Lacap
### Created on 2022-01-07
### Updated on 2023-05-17

###This is an updated version of a previous script containing the drg and tg
###measurements of bamboo shark, cat shark and round stingray. This contains the different dev stages.

### Load libraries
library(tidyverse)
library(here)
library(ggpubr)
library(hablar)

### Load data
master <- read_csv(here("drg_measurements_new","chilo","20230517_masterlist.csv"))

### statistics
master_sharks <- master %>% 
  mutate(avg_dia = x+y/2) %>%   #create average diameters between x and y
  filter(area >= 10,
         exp != "P2X3-488 AceTub-594 20x 2a") %>% 
  filter(species != "Rat") %>% 
  select(1:5,8) %>% 
  mutate(dev_stage = recode(dev_stage,
                           'Pre-hatch' = "Hatched",
                           '32' = "Stage 32")) %>% 
  mutate(dev_stage = factor(dev_stage,
                            levels = c("Stage 32",
                                       "Hatched",
                                       "Adult"))) #%>% 
 #unique() #remove any duplicates

duplicates_sharks_raw <- master_sharks %>%
  find_duplicates()#finding any duplicates

master_sharks_unique <- master_sharks %>% 
  unique()
  
# average
cell_ave <- master1 %>% 
  group_by(tissue,dev_stage) %>% 
  summarise(avg = mean(area),
            sd = sd(area),
            var = var(area),
            med = median(area)) 

cell_count <- master1 %>% 
  group_by(tissue,dev_stage,exp) %>% 
  count()

# t-test
area_ttest <- master1 %>% 
  select(3,4,5) %>% 
  group_by(tissue,dev_stage) %>% 
  as.data.frame()

### plot data

area_p <- master1 %>% 
  gghistogram(x = "area", add = "mean", rug = TRUE,
              combine = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Soma Size (μm2)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage, scales = "free")+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 1),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 1),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))
area_p

snow_p <- master1 %>% 
  gghistogram(x = "avg_dia", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Avg. Diameter (μm)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage, scales = "free")+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 1),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 1),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))
snow_p

peri_p <- master1 %>% 
  gghistogram(x = "perimeter", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Perimeter (μm)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage)+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 1),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 1),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))

peri_p

#####rat analysis
master_rats <- master %>% 
  mutate(avg_dia = x+y/2) %>%   #create average diameters between x and y
  filter(area >= 10,
         exp != "P2X3-488 AceTub-594 20x 2a") %>% 
  filter(species == "Rat") %>% 
  select(1:5,8,9) %>% 
  unique()#remove any duplicates

#plot rat control

area_p_rat <- master_rats %>% 
  gghistogram(x = "area", add = "mean", rug = TRUE,
              combine = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Soma Size (μm2)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage, scales = "free")+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 1),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 1),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))

area_p_rat

snow_p_rat <- master_rats %>% 
  gghistogram(x = "avg_dia", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Avg. Diameter (μm)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage, scales = "free")+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 1),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 1),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))

snow_p_rat

peri_p_rat <- master_rats %>% 
  gghistogram(x = "perimeter", add = "mean", rug = TRUE,
              color = "tissue", fill = "tissue",
              bins = 40)+
  labs(x = "Perimeter (μm)", y = "Number of cells")+
  theme_classic()+
  facet_wrap(~dev_stage)+
  theme(axis.title.x = element_text(size = 30, face = 2),
        axis.text.x = element_text(size = 24, face = 1),
        axis.title.y = element_text(size = 30, face = 2),
        axis.text.y = element_text(size = 24, face = 1),
        legend.text=element_text(size = 24, face = 2),
        legend.title = element_text(size = 24, face = 2),
        strip.text = element_text(size = 24))

peri_p_rat



