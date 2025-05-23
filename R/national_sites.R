# National sites
########################################################################
#### Auxiliary
getPlaceIDiNat <- function(country_name){
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  call_url_place <- str_glue('{api}/places/autocomplete?q={country_name}{page}')
  get_json_call_place <- GET(url = URLencode(call_url_place)) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  results_place <- as_tibble(get_json_call_place$results)
  if(nrow(results_place) != 0){
    place_id <- results_place$id[results_place$admin_level==0]
  } else {
    place_id <- NA
  }
  return(place_id)
}

########################################################################
# Número de registros de iNaturalist en GBIF por país: `n_inat_gbif_country`.  
recordsPerCountryGBIF <- function(list_of_country_codes){
  
  iNatKey <- '50c9509d-22c7-4a22-a47d-8c48425ef4a7'
  
  records_per_country <- tibble(country_code = character(),
                                n_records_gbif = numeric(),
                                n_records_gbif_iNat = numeric())
  
  for(code in list_of_country_codes){

    n_records_country <- occ_count(country_code=code,
                                   hasCoordinate=TRUE, hasGeospatialIssue=FALSE)
    
    n_records_country_iNat <- occ_count(country_code=code,
                                        datasetKey=iNatKey,
                                        hasCoordinate=TRUE, hasGeospatialIssue=FALSE)
    
    records_per_country_i <- tibble(country_code = code,
                                    n_records_gbif = n_records_country,
                                    n_records_gbif_iNat = n_records_country_iNat)
    
    records_per_country <- rbind(records_per_country, records_per_country_i)
  }
  return(records_per_country)
}

########################################################################
# Número de registros en iNaturalist por país: `n_inat_country`.  
recordsPerCountryiNat <- function(list_of_countries){
  
  countries_iNat_records <- tibble(country_name = character(),
                                   n_records_inat = numeric())
  
  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_countries){

    place_id <- getPlaceIDiNat(country_name)
    
    if(!is.na(place_id)){
      call_url_observations <- str_glue('{api}/observations?verifiable=true&place_id={place_id}')
      get_json_call_observations <- GET(url = call_url_observations) %>%
        content(as = "text") %>% fromJSON(flatten = TRUE)
      results_observations <- as_tibble(get_json_call_observations$total_results) 
      
      countries_iNat_records_i <- tibble(country_name = country_name,
                                         n_records_inat = results_observations$value)
      countries_iNat_records <- rbind(countries_iNat_records, countries_iNat_records_i)
    } else {
      countries_iNat_records_i <- tibble(country_name = country_name,
                                         n_records_inat = NA)
      countries_iNat_records <- rbind(countries_iNat_records, countries_iNat_records_i)
    }
  }
  return(countries_iNat_records)
}

########################################################################
# Número de usuarios de iNaturalist por país: `n_users_country`.  
usersPerCountryiNat <- function(list_of_countries){
  
  users_iNat_records <- tibble(country_name = character(),
                               n_users = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/observers'
  page <- '&page=1&per_page=1'
  
  for(country_name in list_of_countries){
  
      place_id <- getPlaceIDiNat(country_name)
        
      if(!is.na(place_id)){
        call_url_observations <- str_glue('{api}?verifiable=true&place_id={place_id}')
        get_json_call_observations <- GET(url = call_url_observations) %>%
          content(as = "text") %>% fromJSON(flatten = TRUE)
        n_users <- as_tibble(get_json_call_observations$total_results) 
        
        users_iNat_records_i <- tibble(country_name = country_name,
                                       n_users = n_users$value)
        users_iNat_records <- rbind(users_iNat_records, users_iNat_records_i)
      } else {
        countries_iNat_records_i <- tibble(country_name = country_name,
                                           n_users = NA)
        users_iNat_records <- rbind(users_iNat_records, users_iNat_records_i)
      }
    }
  return(users_iNat_records)
}

########################################################################
# Área del país en km^2^: `area_country`.   
areaPerCountry <- function(list_of_country_codes){
  
  area_country <- tibble(country_code = character(),
                         area = numeric())
  
  for(country_code in list_of_country_codes){
    
    surface <- try(WDI::WDI(country = country_code, 
                            indicator = 'AG.SRF.TOTL.K2'), silent=T)
    
    if(class(surface)=='try-error'){
      area_country_i <- tibble(country_code = country_code,
                               area = NA)
      area_country <- rbind(area_country, area_country_i)
    } else {
      surface <- surface %>% 
        filter(!is.na(AG.SRF.TOTL.K2)) %>% 
        slice_head(n=1) %>% pull(AG.SRF.TOTL.K2)
      area_country_i <- tibble(country_code = country_code,
                               area = surface)
      area_country <- rbind(area_country, area_country_i) 
    }
  }
  return(area_country)
}

########################################################################
# Población del país: `population`.  
populationPerCountry <- function(list_of_country_codes){
  pop_country <- tibble(country_code = character(),
                         area = numeric())
  
  for(country_code in list_of_country_codes){
    population <- try(WDI::WDI(country = country_code, 
                               indicator = 'SP.POP.TOTL'), silent = T)
    if(class(population)=='try-error'){ 
      pop_country_i <- tibble(country_code = country_code,
                              pop = NA)
      pop_country <- rbind(pop_country, pop_country_i) 
    } else {
      population <- population %>% 
        filter(!is.na(SP.POP.TOTL)) %>% 
        slice_head(n=1) %>% pull(SP.POP.TOTL)
      pop_country_i <- tibble(country_code = country_code,
                              pop = population)
      pop_country <- rbind(pop_country, pop_country_i) 
      }

  }
  return(pop_country)
}

########################################################################
# PBI por país: `gdp_per_capita_country`.  
gdpPerCapitaCountry <- function(list_of_country_codes){
  gdp_per_capita_country <- tibble(country_code = character(),
                                   gdp = numeric())
  
  for(country_code in list_of_country_codes){
    gdp <- try(WDI::WDI(country = country_code, 
                                 indicator = 'NY.GDP.PCAP.CD'), silent = T)
    if(class(gdp)=='try-error'){ 
      gdp_country_i <- tibble(country_code = country_code,
                              gdp = NA)
      gdp_per_capita_country <- rbind(gdp_per_capita_country, gdp_country_i) 
    } else {
      gdp <- gdp %>% 
        filter(!is.na(NY.GDP.PCAP.CD)) %>% 
        slice_head(n=1) %>% pull(NY.GDP.PCAP.CD)
      gdp_country_i <- tibble(country_code = country_code,
                              gdp = gdp)
      gdp_per_capita_country <- rbind(gdp_per_capita_country, gdp_country_i)
    }
    
  }
  return(gdp_per_capita_country)
}

########################################################################
# Porcentaje del PBI dedicado a la investigación por país: `gdp_research_country`.  
gdpResearchPerCountry <- function(list_of_country_codes){
  gdp_r_country <- tibble(country_code = character(),
                          gdp_research = numeric())
  
  for(country_code in list_of_country_codes){
    gdp_research <- try(WDI::WDI(country = country_code, 
                               indicator = 'GB.XPD.RSDV.GD.ZS'), silent = T)
    if(class(gdp_research)=='try-error'){ 
      gdp_r_country_i <- tibble(country_code = country_code,
                              gdp_research = NA)
      gdp_r_country <- rbind(gdp_r_country, gdp_r_country_i) 
    } else {
      gdp_research <- gdp_research %>% 
        filter(!is.na(GB.XPD.RSDV.GD.ZS)) %>% 
        slice_head(n=1) %>% pull(GB.XPD.RSDV.GD.ZS)
      gdp_r_country_i <- tibble(country_code = country_code,
                                gdp_research = gdp_research)
      gdp_r_country <- rbind(gdp_r_country, gdp_r_country_i)
    }
    
  }
  return(gdp_r_country)
}

########################################################################
# Latitud media del país (como proxy de biodiversidad esperada): `latitude`.
latitudePerCountry <- function(list_of_country_codes){
  
  country_lat <- tibble(country_code = character(),
                        latitude = numeric())
  
  for(country_code in list_of_country_codes){
    country_name <- countrycode::countrycode(country_code, 
                                             'iso2c',
                                             'country.name')
    
    latitude <- try(rnaturalearth::ne_countries(country = country_name, 
                                                returnclass = 'sf'),
                    silent = T)
    
    if(class(latitude)[1]=='try-error'){ 
      country_lat_i <- tibble(country_code = country_code,
                              latitude = NA)
      country_lat <- rbind(country_lat, country_lat_i) 
    } else {
      latitude <- sf::st_coordinates(sf::st_centroid(latitude$geometry))[1,1]
      
      country_lat_i <- tibble(country_code = country_code,
                              latitude = latitude)
      country_lat <- rbind(country_lat, country_lat_i) 
    }
  }
  return(country_lat)
}


