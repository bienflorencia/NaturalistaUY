library(lubridate)
library(sf)
library(stringi)
library(tidyverse)

Uruguay <- st_read('../Visualizations/data/Uruguay.shp')
iNatUY <- read_csv('../Data_Analysis/data/observations-210577.csv', guess_max = 39000)


iNatUY %>% 
  distinct(user_id) %>% count()

# Species List
iNatUY %>% distinct(iconic_taxon_name) %>% count()
iNatUY %>% distinct(taxon_species_name) %>% count()

allSpecies <- iNatUY %>% 
  filter(!is.na(taxon_species_name) & !is.na(iconic_taxon_name)) %>% 
  select(scientific_name, 
         taxon_id,
         iconic_taxon_name,
         kingdom = taxon_kingdom_name,
         class = taxon_class_name,
         order = taxon_order_name,
         family = taxon_family_name,
         genus = taxon_genus_name,
         species= taxon_species_name,
         common_name) %>% 
  mutate(taxon_rank=ifelse(scientific_name!=species, 'subspecies', 'species')) %>% 
  distinct(scientific_name, .keep_all = T) %>% 
  arrange(kingdom, class, order, family, genus, scientific_name)

allSpecies %>% 
  write_excel_csv('../Data_Analysis/data/Lista_de_especies_2022-01-29.csv', na = '')


allSpecies %>% distinct(iconic_taxon_name)

allSpecies %>% filter(iconic_taxon_name=='Actinopterygii') %>% 
  write_excel_csv('../Data_Analysis/data/Peces_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Amphibia') %>% 
  write_excel_csv('../Data_Analysis/data/Anfibios_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Arachnida') %>% 
  write_excel_csv('../Data_Analysis/data/Arañas_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Aves') %>% 
  write_excel_csv('../Data_Analysis/data/Aves_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Mollusca') %>% 
  write_excel_csv('../Data_Analysis/data/Moluscos_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Reptilia') %>% 
  write_excel_csv('../Data_Analysis/data/Reptiles_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Fungi') %>% 
  write_excel_csv('../Data_Analysis/data/Hongos_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Plantae') %>% 
  write_excel_csv('../Data_Analysis/data/Plantas_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Insecta') %>% 
  write_excel_csv('../Data_Analysis/data/Insectos_2022-01-29.csv', na = '')

allSpecies %>% filter(iconic_taxon_name=='Mammalia') %>% 
  write_excel_csv('../Data_Analysis/data/Mamiferos_2022-01-29.csv', na = '')

iNatUY %>% 
  filter(!is.na(taxon_species_name) & !is.na(iconic_taxon_name)) %>% 
  select(scientific_name, 
         taxon_id,
         iconic_taxon_name,
         kingdom = taxon_kingdom_name,
         class = taxon_class_name,
         order = taxon_order_name,
         family = taxon_family_name,
         genus = taxon_genus_name,
         species= taxon_species_name,
         common_name) %>% 
  mutate(taxon_rank=ifelse(scientific_name!=species, 'subspecies', 'species')) %>% 
  distinct(scientific_name, .keep_all = T) %>% 
  arrange(kingdom, class, order, family, genus, scientific_name) 

