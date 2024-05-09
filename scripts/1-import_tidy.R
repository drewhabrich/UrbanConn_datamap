## HEADER---------------------------
## Script name: 1-import_tidy.R
##
## Purpose of script: Import and tidy fullscreening data
##
## Author: Andrew Habrich
##
## Date Created: 2024-05-09
## Date last Modified: 2024-05-09
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
library(tidyverse)
library(readxl)
library(DataExplorer)

## 2. Import data-------------------
fs_dat <- read_xlsx("rawdata/fullscreening_inexclusion_dat.xlsx")

glimpse(fs_dat)
plot_intro(fs_dat)
plot_missing(fs_dat)

## replace na-duplicated values in exclreason with dupl
fs_dat <- fs_dat %>% 
  mutate(exclreason = ifelse(exclreason == "na-duplicated", "dupl", exclreason))

## 3. Data exploration ---------------------
### How many unique studies? #SHOULD BE 450
fs_dat %>% 
  select(studyID) %>% distinct() %>%  nrow()

### How many studies by exclreason? For studies with multiple exclusion reasons, use the first reason (reasons are separated by ,).
fs_dat %>% 
  mutate(exclreason = str_split(exclreason, ",") %>% map_chr(1)) %>% 
  count(exclreason) %>% 
  arrange(desc(n))
#### create a plot of the above, with exclusion reasons on the x-axis and the number of studies on the y-axis, descending order
fs_dat %>% 
  mutate(exclreason = str_split(exclreason, ",") %>% map_chr(1)) %>% count(exclreason) %>% 
  ggplot(aes(x = fct_reorder(exclreason, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Exclusion reason", y = "Number of studies") + 
    theme_bw()

### How many studies by studyobj?
fs_dat %>% 
  count(studyobj) %>% 
  arrange(desc(n))
# create a new factor based on exclreasons, with KEEP for all 'na' and REJECT for all other reasons
fs_dat <- fs_dat %>% 
  mutate(decision = ifelse(exclreason == "na", "KEEP", "REJECT"))
# create a facet plot of the above, with studyobj on the x-axis and the number of studies on the y-axis, descending order,
# with the bars colored exclreason, removing the 'na' studyobj, facet by decision
fs_dat %>% 
  filter(studyobj != "na") %>% 
  count(studyobj, decision, exclreason) %>% 
  ggplot(aes(x = fct_reorder(studyobj, n), y = n, fill = exclreason)) +
  geom_col(position = "stack") +
  coord_flip() +
  facet_wrap(~ decision) + 
  labs(x = "Study objective", y = "Number of studies") + 
  theme_bw() +  theme(legend.position = "bottom", strip.background = element_rect(colour="black", fill="goldenrod"))

## 4. create subsets for KEEP and REJECT
keepdat <- fs_dat %>% 
  filter(decision == "KEEP")
rejtdat <- fs_dat %>%
  filter(decision == "REJECT")

### what are the study taxa in the KEEP subset? Create a plot
keepdat %>% 
  count(study_taxa) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x = fct_reorder(study_taxa, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Study taxa", y = "Number of studies") + 
  theme_bw()
