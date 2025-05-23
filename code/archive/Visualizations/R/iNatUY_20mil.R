library(lubridate)
library(extrafont)
library(forcats)
library(sf)
library(stringi)
library(tidyverse)

Uruguay <- st_read('data/Uruguay.shp')
iNatUY <- read_csv('data/observations-131793.csv', guess_max = 20000)

iNatUY <- iNatUY %>% head(20000)


iNatUY %>% arrange(desc(created_at)) %>% select(created_at)

# Number of records created a year from today() '2021-02-13'
iNatUY %>% 
  filter(created_at>=(ymd('2021-02-13')-years(1))) %>% nrow()

# Percentage of the records created a year from today over the total amount of records
(100 * iNatUY %>% 
  filter(created_at>=(ymd('2021-02-13')-years(1))) %>% nrow())/nrow(iNatUY) 

# Number of users a year from today
iNatUY %>% 
  filter(created_at>=(ymd('2021-02-13')-years(1))) %>% 
  distinct(user_id) %>% nrow()

# Percentage of the number of users a year from today over the total amount of users in the platform from Uruguay
(100 * iNatUY %>% 
    filter(created_at>=(ymd('2021-02-13')-years(1))) %>% 
    distinct(user_id) %>% nrow() / nrow(iNatUY %>% distinct(user_id)))


# Number of observations for each department
iNatUY %>% 
  group_by(place_admin1_name) %>% count() %>% arrange(desc(n))

# Number of observations for each department (MAP)
left_join(Uruguay %>% mutate(place_admin1_name=tolower(nombre)), 
          iNatUY %>% 
            group_by(place_admin1_name) %>% count() %>% 
            mutate(place_admin1_name=stri_trans_general(tolower(place_admin1_name), id = 'Latin-ASCII')), 
          by='place_admin1_name') %>% 
  ggplot() + 
  geom_sf(aes(fill=n)) +
  labs(x='', y= '', fill = 'Number of\nObservations') +
  theme_bw() +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 10, type = "continuous")) +
  theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


# The first 10 taxa with more observations 
iNatUY %>% 
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, taxon_class_name) %>% count() %>% 
  arrange(desc(n)) %>% head(10) %>% 
  ggplot(., aes(x=n, y=fct_reorder(taxon_class_name, taxon_kingdom_name), fill=fct_reorder(taxon_class_name, taxon_kingdom_name))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x='Number of Observations', y= '', fill = '') +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 6000, 500)) +
  scale_fill_brewer(palette ='Spectral') +
  theme(text=element_text(family='Calibri')) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

# Plot records per month from major groups
iNatUY %>% 
  filter(taxon_kingdom_name=='Animalia' | taxon_kingdom_name=='Fungi' | taxon_kingdom_name=='Plantae') %>%
  mutate(updated_at=ymd_hms(created_at)) %>%
  filter(!is.na(taxon_class_name) & year(updated_at)>2017) %>% 
  add_count(taxon_kingdom_name, year=year(updated_at), month=month(updated_at), name='records_per_month') %>% 
  ggplot(., aes(x=updated_at, y=records_per_month, color=taxon_kingdom_name)) +
  geom_line(show.legend = FALSE, size=1) +
  facet_grid(taxon_kingdom_name~., scales = "free", switch='x' ,drop=TRUE) +
  scale_x_datetime(date_breaks = "2 month", date_labels = "%b%y") +
  theme_bw()+
  labs(x='', y= 'Número de registros por mes', color = '') +
  theme(text=element_text(family='Calibri')) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))

#############################
############
###########

## Data for figures

# TOTAL 20000
# CADA PUNTO 200 REGISTROS

#Plantas
#Hongos

iNatUY %>% 
  filter(taxon_kingdom_name=='Plantae'|taxon_kingdom_name=='Fungi') %>% 
  group_by(taxon_kingdom_name) %>%
  count() %>% 
  mutate(dots=round(n*100/nrow(iNatUY), 0)) 

#Moluscos
iNatUY %>% 
  filter(taxon_phylum_name=='Mollusca') %>% 
  group_by(taxon_phylum_name) %>% 
  count() %>% 
  mutate(dots=round(n*100/nrow(iNatUY), 0)) 

#Mamíferos Mammalia
#Reptiles Reptilia
#Peces Actinopterygii
#Anfibios Amphibia
#Aves Aves
#Aranhas Arachnida
#Insectos Insecta

iNatUY %>% 
  filter(taxon_class_name=='Mammalia' | 
           taxon_class_name=='Reptilia' | 
           taxon_class_name=='Actinopterygii' |
           taxon_class_name=='Amphibia' |
           taxon_class_name=='Aves' |
           taxon_class_name=='Arachnida' |
           taxon_class_name=='Insecta') %>% 
  group_by(taxon_class_name) %>%
  count() %>% 
  mutate(dots=round(n*100/nrow(iNatUY), 0)) 

#Otros animales: Malacostraca, Polychaeta, Scyphozoa, Anthozoa, Chilopoda, Clitellata, Diplopoda
iNatUY %>% 
  filter(taxon_kingdom_name=='Animalia' & !taxon_phylum_name=='Mollusca') %>% 
  filter(taxon_class_name!='Mammalia' & 
           taxon_class_name!='Reptilia' & 
           taxon_class_name!='Actinopterygii' &
           taxon_class_name!='Amphibia' &
           taxon_class_name!='Aves' &
           taxon_class_name!='Arachnida' &
           taxon_class_name!='Insecta') %>% 
  mutate(taxon_class_name_general='Other Animals') %>% 
  group_by(taxon_class_name_general) %>%
  count() %>% 
  mutate(dots=n*100/nrow(iNatUY)) 


#############

# Geographic location
iNatUY %>% 
  group_by(place_admin1_name) %>% 
  count() %>% arrange(desc(n)) %>% 
  mutate(dots=n*100/nrow(iNatUY))%>% ungroup() %>% 
  mutate(dots=round(n*100/nrow(iNatUY), 0)) 

# Geographic location - others than most recorded
iNatUY %>% 
  filter(!place_admin1_name=='Montevideo' &
           !place_admin1_name=='Maldonado' & !place_admin1_name=='Río Negro' & 
           !place_admin1_name=='Rocha' & !place_admin1_name=='Canelones') %>% 
  summarise(n=n()) %>% 
  mutate(dots=n*100/nrow(iNatUY)) 

# Year of creation
iNatUY %>% 
  mutate(year=year(created_at)) %>%
  group_by(year) %>% 
  summarise(n=n()) %>% 
  mutate(dots=n*100/nrow(iNatUY)) 

# Year of creation - prior to 2019
iNatUY %>% 
  filter(created_at<=(ymd('2018-12-31'))) %>% 
  summarise(n=n()) %>% 
  mutate(dots=n*100/nrow(iNatUY)) 


iNatUY %>% 
  head(10000) %>% 
  group_by(quality_grade) %>% count() %>% ungroup() %>% mutate(total=sum(n)) %>% 
  mutate(dots=round(n*100/total, 0))

# Observation quality
iNatUY %>% 
  group_by(quality_grade) %>% count() %>% ungroup() %>% mutate(total=sum(n)) %>% 
  mutate(dots=round(n*100/total, 0)) 

# Observation quality Vertebrates
iNatUY %>% 
  filter(taxon_class_name=='Mammalia' | 
           taxon_class_name=='Reptilia' | 
           taxon_class_name=='Actinopterygii' |
           taxon_class_name=='Amphibia' |
           taxon_class_name=='Aves') %>%
  group_by(taxon_kingdom_name, quality_grade) %>% 
  count() %>% ungroup() %>% mutate(total=sum(n)) %>% 
  mutate(dots=round(n*100/total, 0))

# Observation quality Fungi
iNatUY %>% 
  filter(taxon_kingdom_name=='Fungi') %>%
  group_by(taxon_kingdom_name, quality_grade) %>% 
  count() %>% ungroup() %>% mutate(total=sum(n)) %>% 
  mutate(dots=round(n*100/total, 0))

# Observation quality Plantae
iNatUY %>% 
  filter(taxon_kingdom_name=='Plantae') %>% 
  group_by(taxon_kingdom_name, quality_grade) %>% 
  count() %>% ungroup() %>% mutate(total=sum(n)) %>% 
  mutate(dots=round(n*100/total, 0))

# Observation quality Plantae
iNatUY %>% 
  filter(taxon_class_name=='Insecta')  %>% 
  group_by(taxon_kingdom_name, quality_grade) %>% 
  count() %>% ungroup() %>% mutate(total=sum(n)) %>% 
  mutate(dots=round(n*100/total, 0))

