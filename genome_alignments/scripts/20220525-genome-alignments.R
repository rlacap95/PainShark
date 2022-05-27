### Genome alignments
### Created by Roland Lacap
### Created on 2022-05-25
### Updated on 2022-05-25

### This is primarily to show the NCBI alignments of nociceptive genes to 
### elasmobranch genomes. Here, I show the percent identities.

### Load libraries
library(tidyverse)
library(here)
library(ggpubr)
library(scales)
library(PNWColors)

### Load data
cp_align <- read_csv(here("genome_alignments","data","20220523-Cp.csv"))
le_align <- read_csv(here("genome_alignments","data","20220523-Le.csv"))
sc_align <- read_csv(here("genome_alignments","data","20220523-Sc.csv"))

cpfilter <- cp_align %>% 
  select(1,3:5) %>% 
  group_by(Query) %>% 
  filter(Perc_Identiy == max(Perc_Identiy)) %>%
  filter(E_value == min(E_value)) %>% 
  mutate(Gene = recode(Query,
                      'Mus musculus neurotrophic tyrosine kinase, receptor, type 2 (Ntrk2), transcript variant X4, mRNA'="Ntrk2",
                      'Mus musculus SRY (sex determining region Y)-box 10 (Sox10), mRNA'="Sox10",
                      'Mus musculus ISL1 transcription factor, LIM/homeodomain (Isl1), mRNA'="Isl1",
                      'Mus musculus neurofilament, medium polypeptide (Nefm), mRNA'="Nefm",
                      'Mus musculus neurogenin 1 (Neurog1), mRNA'="Neurog1",
                      'Mus musculus neurogenin 2 (Neurog2), mRNA'="Neurog2",
                      'Mus musculus neurotrophic tyrosine kinase, receptor, type 1 (Ntrk1), transcript variant X1, mRNA'="Ntrk1",
                      'Mus musculus neurotrophic tyrosine kinase, receptor, type 3 (Ntrk3), transcript variant X10, mRNA'="Ntrk3",
                      'Mus musculus purinergic receptor P2X, ligand-gated ion channel, 3 (P2rx3), transcript variant X1, mRNA'="P2x3",
                      'Mus musculus PR domain containing 12 (Prdm12), mRNA'="Prdm12",
                      'Mus musculus ret proto-oncogene (Ret), transcript variant 4, mRNA'="Ret",
                      'Mus musculus runt related transcription factor 1 (Runx1), transcript variant 3, mRNA'="Runx1",
                      'Mus musculus runt related transcription factor 3 (Runx3), transcript variant X1, mRNA'="Runx3",
                      'Mus musculus tachykinin 1 (Tac1), transcript variant X1, mRNA'="Tac1")) %>% 
  mutate(name=c("Chiloscyllium punctatum")) %>% 
  distinct()

lefilter <- le_align %>% 
  select(1,3:5) %>% 
  group_by(Query) %>% 
  filter(Perc_Identiy == max(Perc_Identiy)) %>% 
  filter(E_value == min(E_value)) %>% 
  mutate(Gene = recode(Query,
                       'PREDICTED: Mus musculus neurotrophic tyrosine kinase, receptor, type 2 (Ntrk2), transcript variant X4, mRNA'="Ntrk2",
                       'Mus musculus SRY (sex determining region Y)-box 10 (Sox10), mRNA'="Sox10",
                       'Mus musculus ISL1 transcription factor, LIM/homeodomain (Isl1), mRNA'="Isl1",
                       'Mus musculus neurofilament, medium polypeptide (Nefm), mRNA'="Nefm",
                       'Mus musculus neurogenin 1 (Neurog1), mRNA'="Neurog1",
                       'Mus musculus neurogenin 2 (Neurog2), mRNA'="Neurog2",
                       'PREDICTED: Mus musculus neurotrophic tyrosine kinase, receptor, type 1 (Ntrk1), transcript variant X1, mRNA'="Ntrk1",
                       'PREDICTED: Mus musculus neurotrophic tyrosine kinase, receptor, type 3 (Ntrk3), transcript variant X10, mRNA'="Ntrk3",
                       'PREDICTED: Mus musculus purinergic receptor P2X, ligand-gated ion channel, 3 (P2rx3), transcript variant X1, mRNA'="P2x3",
                       'Mus musculus PR domain containing 12 (Prdm12), mRNA'="Prdm12",
                       'Mus musculus ret proto-oncogene (Ret), transcript variant 4, mRNA'="Ret",
                       'Mus musculus runt related transcription factor 1 (Runx1), transcript variant 3, mRNA'="Runx1",
                       'PREDICTED: Mus musculus runt related transcription factor 3 (Runx3), transcript variant X1, mRNA'="Runx3",
                       'Mus musculus tachykinin 1 (Tac1), transcript variant X1, mRNA'="Tac1"))%>% 
  mutate(name=c("Leucoraja erinacea")) %>% 
  distinct()

scfilter <- sc_align %>% 
  select(1,3:5) %>% 
  group_by(Query) %>% 
  filter(Perc_Identiy == max(Perc_Identiy)) %>% 
  filter(E_value == min(E_value)) %>% 
  mutate(Gene = recode(Query,
                       'Mus musculus neurotrophic tyrosine kinase, receptor, type 2 (Ntrk2), transcript variant X4, mRNA'="Ntrk2",
                       'Mus musculus SRY (sex determining region Y)-box 10 (Sox10), mRNA'="Sox10",
                       'Mus musculus ISL1 transcription factor, LIM/homeodomain (Isl1), mRNA'="Isl1",
                       'Mus musculus neurofilament, medium polypeptide (Nefm), mRNA'="Nefm",
                       'Mus musculus neurogenin 1 (Neurog1), mRNA'="Neurog1",
                       'Mus musculus neurogenin 2 (Neurog2), mRNA'="Neurog2",
                       'PREDICTED: Mus musculus neurotrophic tyrosine kinase, receptor, type 1 (Ntrk1)'="Ntrk1",
                       'Mus musculus neurotrophic tyrosine kinase, receptor, type 3 (Ntrk3), transcript variant X10, mRNA'="Ntrk3",
                       'Mus musculus purinergic receptor P2X, ligand-gated ion channel, 3 (P2rx3), transcript variant X1, mRNA'="P2x3",
                       'Mus musculus PR domain containing 12 (Prdm12), mRNA'="Prdm12",
                       'Mus musculus ret proto-oncogene (Ret), transcript variant 4, mRNA'="Ret",
                       'Mus musculus runt related transcription factor 1 (Runx1), transcript variant 3, mRNA'="Runx1",
                       'Mus musculus runt related transcription factor 3 (Runx3), transcript variant X1'="Runx3",
                       'Mus musculus tachykinin 1 (Tac1), transcript variant X1, mRNA'="Tac1"))%>% 
  mutate(name=c("Scyliorhinus canicula"))

master <- cpfilter %>% 
  full_join(lefilter) %>% 
  full_join(scfilter) %>% 
  mutate(Query_cover = as.numeric(parse_number(Query_cover))) 

  
# Plot
pal <- pnw_palette("Winter")

p <- master %>% 
  ggbarplot(x = "Gene", y = "Perc_Identiy", fill = "E_value") +
  facet_wrap(~name)+
  coord_flip() +
  theme_pubclean()+
  theme(legend.position = "right")+
  gradient_fill(pal)

p

pq <- master %>% 
  ggbarplot(x = "Gene", y = "Perc_Identiy", fill = "Query_cover") +
  labs(y="Percent Identity", x=" ", fill = "Query cover")+
  facet_wrap(~name)+
  coord_flip() +
  theme_pubclean()+
  theme(legend.position = "right")+
  gradient_fill(pal)

pq
