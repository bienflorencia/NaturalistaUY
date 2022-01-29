library(lubridate)
library(tidyverse)

iNatUY <- read_csv('observations-103208.csv')

# Number of records created a year from today (2020-08-28)
iNatUY %>% 
  filter(created_at<=ymd('2019-08-28')) %>% nrow()
  
# Number of users a year from today (2020-08-28)
iNatUY %>% 
  filter(created_at<=ymd('2019-08-28')) %>% 
  distinct(user_id) %>% nrow()

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
  theme(text=element_text(family='Calibri', size = 14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
