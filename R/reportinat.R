### PROJECTS
getProjectInfo <- function(project_id){
  
  projectInfo <- tibble(project_id = numeric(),
                        project_slug = character(),
                        project_creator = character(),
                        project_title = character(),
                        project_type = character(),
                        created_at = character())
  
  api <- 'https://api.inaturalist.org/v1/projects'
  call_url <- str_glue('{api}?q={project_id}')
  
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  projectInfo <- tibble(project_id = results$id,
                        project_slug = results$slug,
                        project_creator = results$user_id,
                        project_title = results$title,
                        project_type = results$project_type,
                        created_at = as_date(results$created_at))
  projectInfo <- projectInfo %>% 
    mutate(project_type =
             case_when(project_type == 'umbrella' ~ 'paraguas',
                       project_type == 'collection' ~ 'colección',
                       .default = NA))
  return(projectInfo)
}
getProjectNumbers <- function(project_id){
  
  projectNumbers <- tibble(project_id = numeric(),
                           observervations = numeric(),
                           species = numeric(),
                           observers = numeric(),
                           identifiers = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations'
  page <- '&page=1&per_page=1'
  call_url_observervations <- str_glue('{api}?project_id={project_id}{page}')
  call_url_species <- str_glue('{api}/species_counts?project_id={project_id}&rank=species{page}')
  call_url_observers <- str_glue('{api}/observers?project_id={project_id}{page}')
  call_url_identifiers <- str_glue('{api}/identifiers?project_id={project_id}{page}')
  
  get_json_call_observervations <- GET(url = call_url_observervations) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  get_json_call_species <- GET(url = call_url_species) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  get_json_call_observers <- GET(url = call_url_observers) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  get_json_call_identifiers <- GET(url = call_url_identifiers) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  projectNumbers <- tibble(project_id = project_id,
                           observervations = get_json_call_observervations$total_results,
                           species  = get_json_call_species$total_results,
                           observers = get_json_call_observers$total_results,
                           identifiers = get_json_call_identifiers$total_results)
  return(projectNumbers)
}

getStateName <- function(place_id){
  
  api <- 'https://api.inaturalist.org/v1/places'
  call_url <- str_glue('{api}/{place_id}?admin_level=10')
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  
  if(get_json_call$total_results == 0) {
    place <- tibble(place_id = place_id,
                    place_state_name = NA)
  } else {
    place <- tibble(place_id = results$id,
                    place_state_name = results$name)
  }
  return(place$place_state_name)
}
getCountryName <- function(place_id){
  
  api <- 'https://api.inaturalist.org/v1/places'
  call_url <- str_glue('{api}/{place_id}?admin_level=0')
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  if(get_json_call$total_results == 0) {
    place <- tibble(place_id = place_id,
                    place_country_name = NA)
  } else {
    place <- tibble(place_id = results$id,
                    place_country_name = results$name)
  }
  return(place$place_country_name)
}
getTaxonInfo <- function(taxon_ids){
  
  taxon_ids <- str_c(taxon_ids, collapse=',')
  
  taxonInfo <- tibble(taxon_id = numeric(),
                      taxon_name = character(),
                      taxon_rank = character(),
                      iconic_taxon_name = character(),
                      num_observations = character(),
                      conservation_status = character())
  
  api <- 'https://api.inaturalist.org/v1/taxa'
  call_url <- str_glue('{api}/{taxon_ids}',
                       '?rank_level=10')
  
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  
  taxonInfo <- tibble(taxon_id = results$id,
                      taxon_name = results$name,
                      taxon_rank = results$rank,
                      iconic_taxon_name = results$iconic_taxon_name,
                      num_observations = results$observations_count,
                      conservation_status = results$conservation_statuses)
  
  if(exists('conservation_status', where=taxonInfo)){
    taxonStatus <- taxonInfo %>% 
      unnest(conservation_status, names_sep = "_") %>%
      rename(conservation_place_name=conservation_status_place.name) %>% 
      mutate(conservation_place_name =
               ifelse(exists('conservation_status_place.name',
                             envir = as.environment(.)),
                      taxonStatus$conservation_status_place.name, NA)) %>%
      filter((is.na(conservation_place_name) &
                grepl('iucn', conservation_status_authority, ignore.case = T)) | 
               conservation_place_name == 'Uruguay') %>% 
      select(taxon_id, status=conservation_status_status, 
             authority=conservation_status_authority,
             place_name=conservation_place_name,
             description=conservation_status_description) %>%
      mutate(place_name = ifelse(is.na(place_name), 'global', place_name))
    
  } 
  taxonInfo <- left_join(taxonInfo, taxonStatus) %>% select(-conservation_status) 
  
  Sys.sleep(0.5)
  return(taxonInfo)
}
getProjectData <- function(project_ids){
  
  project_ids <- str_c(project_ids, collapse=',')
  
  projectData <- tibble(observation_id = numeric(),
                        quality_grade = character(),
                        captive = character(),
                        taxon_name = character(),
                        taxon_rank = character(),
                        taxon_id = numeric(),
                        observations_count = numeric(),
                        conservation_status = character(),
                        threatened = character(),
                        endemic = character(),
                        introduced = character(),
                        iconic_taxa = character(),
                        taxon_common_name = character(),
                        created_at = date(), 
                        observed_on = date(), 
                        time_observed_at = date(),
                        uri = character(),
                        user_login = character(),
                        user_name = character(),
                        user_created_at = character(),
                        user_site_id = numeric(),
                        user_observations_count = numeric(),
                        latitude = numeric(), 
                        longitude = numeric(),
                        place_ids = numeric(),
                        geoprivacy = character())
  
  api <- 'https://api.inaturalist.org/v1/observations'
  
  total_results <- GET(url = str_glue('{api}/?',
                                      'project_id={project_ids}&',
                                      'page=1&',
                                      'per_page=1')) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  total_results <- total_results$total_results
  cat(str_glue('{total_results} observations found in total'), 
      '\n downloading ...\n')
  
  per_page = 200 
  for(page in 1:ceiling(total_results/per_page)) {
    cat(str_glue('page {page} of {ceiling(total_results/per_page)} done'), '\n')
    
    call_url <- str_glue('{api}/?',
                         'project_id={project_ids}&',
                         'page={page}&',
                         'per_page={per_page}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    results <- as_tibble(get_json_call$results)
    projectData_i <- tibble(observation_id = results$id,
                            quality_grade = results$quality_grade,
                            captive = results$captive,
                            taxon_name = results$taxon.name,
                            taxon_rank = results$taxon.rank,
                            taxon_id = results$taxon.id,
                            observations_count = results$taxon.observations_count,
                            #conservation status doesn't exist, is NA
                            conservation_status = 
                              ifelse(exists('taxon.conservation_status.status', 
                                            where=results),
                                     results$taxon.conservation_status.status, NA),
                            threatened = results$taxon.threatened,
                            endemic = results$taxon.endemic,
                            introduced = results$taxon.introduced,
                            iconic_taxa = results$taxon.iconic_taxon_name,
                            taxon_common_name = results$taxon.preferred_common_name,
                            created_at = results$created_at,
                            observed_on = results$observed_on,
                            time_observed_at = results$time_observed_at,
                            uri = results$uri,
                            user_login = results$user.login,
                            user_name = results$user.name,
                            user_created_at = results$user.created_at,
                            user_site_id = results$user.site_id,
                            user_observations_count = results$user.observations_count,
                            geojson.coordinates = results$geojson.coordinates,
                            place_ids = results$place_ids,
                            geoprivacy = results$geoprivacy) %>%
      unnest_wider(geojson.coordinates, names_sep = "_") %>%
      rename(longitude=geojson.coordinates_1, 
             latitude=geojson.coordinates_2) %>% 
      unnest_wider(place_ids, names_sep = "_") %>%
      select(-(num_range('place_ids_', c(3:15)))) 
    
    projectData <- rbind(projectData, projectData_i)
    Sys.sleep(2)
  }
  
  projectData <- projectData %>% 
    mutate(user_site_id = case_when(user_site_id == 28 ~ 'NaturalistaUY',
                                    user_site_id == 1 ~ 'iNaturalist',
                                    user_site_id == 2 ~ 'iNaturalistMX',
                                    user_site_id == 3 ~ 'iNaturalistNZ',
                                    user_site_id == 5 ~ 'iNaturalist.ca',
                                    user_site_id == 6 ~ 'NaturalistaCO',
                                    user_site_id == 8 ~ 'BioDiversity4All',
                                    user_site_id == 9 ~ 'iNaturalistAU',
                                    user_site_id == 13 ~ 'iNaturalistPa',
                                    user_site_id == 14 ~ 'iNaturalistEc',
                                    user_site_id == 15 ~ 'iNaturalistil',
                                    user_site_id == 16 ~ 'ArgentiNat',
                                    user_site_id == 17 ~ 'NaturalistaCR',
                                    user_site_id == 18 ~ 'iNaturalistCL',
                                    user_site_id == 20 ~ 'iNaturalistFi',
                                    user_site_id == 21 ~ 'iNaturalist.Se',
                                    user_site_id == 22 ~ 'Natusfera',
                                    user_site_id == 23 ~ 'iNaturalistGR',
                                    user_site_id == 24 ~ 'iNaturalistGT',
                                    user_site_id == 25 ~ 'iNaturalistUK',
                                    user_site_id == 26 ~ 'iNaturalist.LU',
                                    user_site_id == 27 ~ 'iNaturalistTW',
                                    user_site_id == 27 ~ 'iNaturalistTW',
                                    user_site_id == 29 ~ 'iNaturalistsAfr',
                                    .default = NA))
  
  countryAndState <- projectData %>% 
    distinct(place_ids_1, place_ids_2) %>% 
    mutate(country_name = map_chr(place_ids_1, getCountryName),
           state_name = map_chr(place_ids_2, getStateName))
  
  projectData <- left_join(projectData, countryAndState,
                           by = join_by(place_ids_1, place_ids_2)) %>% 
    select(-c(place_ids_1, place_ids_2))
  
  return(projectData)
}

getUserInfo <- function(user_id){
  
  userInfo <- tibble(user_id = numeric(),
                     user_login = character(),
                     user_name = character(),
                     user_created_at = character(),
                     observations_count = numeric(),
                     identifications_count = numeric(),
                     activity_count = numeric(),
                     species_count = numeric(),
                     site_id = numeric())
  
  api <- 'https://api.inaturalist.org/v1/users'
  call_url <- str_glue('{api}/{user_id}')
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  results <- as_tibble(get_json_call$results)
  
  userInfo <- tibble(user_id = results$id,
                     user_login = results$login,
                     user_name = results$name,
                     user_created_at = as_date(results$created_at),
                     observations_count = results$observations_count,
                     identifications_count = results$identifications_count,
                     species_count = results$species_count,
                     activity_count  = results$activity_count,
                     site_id = results$site_id)
  userInfo <- userInfo %>% 
    mutate(user_url = str_glue('https://www.naturalista.uy/people/{user_login}')) %>% 
    mutate(site_id = case_when(site_id == 28 ~ 'NaturalistaUY',
                               site_id == 1 ~ 'iNaturalist',
                               site_id == 2 ~ 'iNaturalistMX',
                               site_id == 3 ~ 'iNaturalistNZ',
                               site_id == 5 ~ 'iNaturalist.ca',
                               site_id == 6 ~ 'NaturalistaCO',
                               site_id == 8 ~ 'BioDiversity4All',
                               site_id == 9 ~ 'iNaturalistAU',
                               site_id == 13 ~ 'iNaturalistPa',
                               site_id == 14 ~ 'iNaturalistEc',
                               site_id == 15 ~ 'iNaturalistil',
                               site_id == 16 ~ 'ArgentiNat',
                               site_id == 17 ~ 'NaturalistaCR',
                               site_id == 18 ~ 'iNaturalistCL',
                               site_id == 20 ~ 'iNaturalistFi',
                               site_id == 21 ~ 'iNaturalist.Se',
                               site_id == 22 ~ 'Natusfera',
                               site_id == 23 ~ 'iNaturalistGR',
                               site_id == 24 ~ 'iNaturalistGT',
                               site_id == 25 ~ 'iNaturalistUK',
                               site_id == 26 ~ 'iNaturalist.LU',
                               site_id == 27 ~ 'iNaturalistTW',
                               site_id == 27 ~ 'iNaturalistTW',
                               site_id == 29 ~ 'iNaturalistsAfr',
                               .default = NA))
  return(userInfo)
}
checkNewUsers <- function(user_list){
  i <- 1
  newUsers <- tibble(user_login = numeric(),
                     user_created_at = character())
  
  cat(str_glue('{length(user_list)} users in total'), 
      '\n downloading info ...\n')
  
  for(user in user_list) {
    info_del_user <- getUserInfo(user)
    cat(str_glue('{i}. @{user} \n\n'))
    newUser_i <- tibble(user_login = info_del_user$user_login,
                        user_created_at =info_del_user$user_created_at)
    newUsers <- rbind(newUsers, newUser_i)
    i <- i+1
    Sys.sleep(0.5)
  }
  return(newUsers)
}
getProjectIdentifiers <- function(project_id){
  
  projectIdentifiers <- tibble(project_id = numeric(),
                               user_id = numeric(),
                               user_login = character(),
                               project_id_count = numeric(),
                               platform_id_count = numeric(),
                               user_site_id = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/identifiers'
  call_url <- str_glue('{api}?project_id={project_id}')
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  results <- as_tibble(get_json_call$results)
  
  projectIdentifiers <- tibble(project_id = project_id,
                               user_id = results$user_id,
                               user_login = results$user.login,
                               project_id_count = results$count,
                               platform_id_count = results$user.identifications_count,
                               user_site_id = results$user.site_id)
  
  projectIdentifiers <- projectIdentifiers %>% 
    mutate(user_site_id = case_when(user_site_id == 28 ~ 'NaturalistaUY',
                                    user_site_id == 1 ~ 'iNaturalist',
                                    user_site_id == 2 ~ 'iNaturalistMX',
                                    user_site_id == 3 ~ 'iNaturalistNZ',
                                    user_site_id == 5 ~ 'iNaturalist.ca',
                                    user_site_id == 6 ~ 'NaturalistaCO',
                                    user_site_id == 8 ~ 'BioDiversity4All',
                                    user_site_id == 9 ~ 'iNaturalistAU',
                                    user_site_id == 13 ~ 'iNaturalistPa',
                                    user_site_id == 14 ~ 'iNaturalistEc',
                                    user_site_id == 15 ~ 'iNaturalistil',
                                    user_site_id == 16 ~ 'ArgentiNat',
                                    user_site_id == 17 ~ 'NaturalistaCR',
                                    user_site_id == 18 ~ 'iNaturalistCL',
                                    user_site_id == 20 ~ 'iNaturalistFi',
                                    user_site_id == 21 ~ 'iNaturalist.Se',
                                    user_site_id == 22 ~ 'Natusfera',
                                    user_site_id == 23 ~ 'iNaturalistGR',
                                    user_site_id == 24 ~ 'iNaturalistGT',
                                    user_site_id == 25 ~ 'iNaturalistUK',
                                    user_site_id == 26 ~ 'iNaturalist.LU',
                                    user_site_id == 27 ~ 'iNaturalistTW',
                                    user_site_id == 27 ~ 'iNaturalistTW',
                                    user_site_id == 29 ~ 'iNaturalistsAfr',
                                    .default = NA))
  return(projectIdentifiers)
}
getTaxonCount <- function(taxon_ids, place_id = 7259){
  
  taxon_ids <- str_c(taxon_ids, collapse=',')
  
  taxonCount <- tibble(taxon_id = numeric(),
                       taxon_name = character(),
                       taxon_rank = character(),
                       observations_place = numeric(),
                       observations_iNat = numeric())
  
  api <- 'https://api.inaturalist.org/v1/observations/species_counts'
  
  total_results <- GET(url = str_glue('{api}/?',
                                      'verifiable=true&',
                                      'place_id={place_id}&', 
                                      'taxon_id={taxon_ids}&',
                                      'page=1&',
                                      'per_page=1')) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  total_results <- total_results$total_results
  
  cat(str_glue('{total_results} observations found in total'), 
      '\n downloading ...\n')
  
  per_page = 500 
  for(page in 1:ceiling(total_results/per_page)) {
    cat(str_glue('page {page} of {ceiling(total_results/per_page)} done'), '\n')
    
    call_url <- str_glue('{api}/?',
                         'verifiable=true&',
                         'place_id={place_id}&', 
                         'taxon_id={taxon_ids}&',
                         'page={page}&',
                         'per_page={per_page}')
    
    get_json_call <- GET(url = call_url) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    results <- as_tibble(get_json_call$results)
    taxonCount_i <- tibble(taxon_id = results$taxon.id,
                           taxon_name = results$taxon.name,
                           taxon_rank = results$taxon.rank,
                           observations_place = results$count,
                           observations_iNat = results$taxon.observations_count)
    
    taxonCount <- rbind(taxonCount, taxonCount_i)
    Sys.sleep(2)
  }
  return(taxonCount)
}
getObservationPhoto <- function(observation_id){
  
  observationInfo <- tibble(observation_id = numeric(),
                            photo_taxon_medium = character(),
                            photo_taxon_square = character(),
                            photo_observation = character())
  
  api <- 'https://api.inaturalist.org/v1/observations'
  call_url <- str_glue('{api}/{observation_id}')
  
  get_json_call <- GET(url = call_url) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  
  observationInfo <- tibble(observation_id = results$id,
                            photo_taxon_medium = results$taxon.default_photo.medium_url,
                            photo_taxon_square = results$taxon.default_photo.url,
                            photo_observation = results$observation_photos[[1]][1,]$photo.url)
  
  Sys.sleep(0.5)
  return(observationInfo$photo_observation)
}
detectPeriod <- function(datos_proyecto, timecol){
  
  period_timecol <- datos_proyecto %>% 
    mutate(timecol=as_datetime(eval(parse(text = timecol)))) %>% 
    summarise(start=min(timecol, na.rm=T),
              end=max(timecol, na.rm=T),
              hours_active=time_length(interval(start, end), 'hours'),
              days_active=time_length(interval(start, end), 'days'),
              years_active=time_length(interval(start, end), 'years'))
  
  if(period_timecol$days_active<=2){
    periodVariables <- tibble(timecol = timecol,
                              count_variable = 'records_per_hour',
                              unit_variable = '1 hour',
                              unit_text = 'hora')
  } else if(period_timecol$days_active>2 &
            period_timecol$days_active<=4){
    periodVariables <- tibble(timecol = timecol,
                              count_variable = 'records_per_hour',
                              unit_variable = '6 hours',
                              unit_text = '6 horas')
  } else if(period_timecol$days_active>4 &
            period_timecol$days_active<=7){
    periodVariables <- tibble(timecol = timecol,
                              count_variable = 'records_per_day',
                              unit_variable = '1 day',
                              unit_text = 'día')
  }else if(period_timecol$days_active>7 &
           period_timecol$days_active<=365){
    periodVariables <- tibble(timecol = timecol,
                              count_variable = 'records_per_week',
                              unit_variable = '1 week',
                              unit_text = 'semana')
  } else if(period_timecol$days_active>365 &
            period_timecol$days_active<=730){
    periodVariables <- tibble(timecol = timecol,
                              count_variable = 'records_per_month',
                              unit_variable = '1 month',
                              unit_text = 'mes')
  }
  else{
    periodVariables <- tibble(timecol = timecol,
                              count_variable = 'records_per_year',
                              unit_variable = '6 months',
                              unit_text = '6 meses')
  }
  return(periodVariables)
}
