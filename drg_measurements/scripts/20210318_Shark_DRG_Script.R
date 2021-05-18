### Shark DRG Diameter
### Created by: Roland L
### Created on: 2021-03-18

### Load Libraries
library(tidyverse)
library(here)
library(ggstatsplot)
library(ggplot2)

### get data
shark <- read.csv(here("drg_measurements","20210517_Masterlist.csv"))

sharkHist <- shark %>% 
  select(Cell_count,Avg_Dia_um,Avg_Dia_px,Mag) %>%
  ggplot(aes(x=Avg_Dia_px))+
  geom_histogram(color="black", fill="white")+
  theme_grey()+
  labs(title="Shark DRG Diameter",
       x = "Average Cell Diameter (um)",
       y = "Cell Count")+
  theme(plot.title=element_text(hjust=0.5, #centered figure title and bolded it
                                face="bold")) +
  ggsave(here("drg_measurements","output","20210318_Shark_DRG_Diameter.png"))

sharkHist

