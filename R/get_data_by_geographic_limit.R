getStateName <- function(place_id){
  
  api <- 'https://api.inaturalist.org/v1/places'
  call_url <- str_glue('{api}/{place_id}?admin_level=10')
  get_json_call <- GET(url = call_url) %>%
    content(as = `text`) %>% fromJSON(flatten = TRUE)
  
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
    content(as = `text`) %>% fromJSON(flatten = TRUE)
  
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
    content(as = `text`) %>% fromJSON(flatten = TRUE)
  
  results <- as_tibble(get_json_call$results)
  
  taxonInfo <- tibble(taxon_id = results$id,
                      taxon_name = results$name,
                      taxon_rank = results$rank,
                      iconic_taxon_name = results$iconic_taxon_name,
                      num_observations = results$observations_count,
                      conservation_status = results$conservation_statuses)
  
  if(exists('conservation_status', where=taxonInfo)){
    taxonStatus <- taxonInfo %>% 
      unnest(conservation_status, names_sep = `_`) %>%
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


#' iNat records on a bounding box
#' Get the records on iNaturalist for a boundig box
#'
#' @param nelat NE latitude of bounding box
#' @param nelng NE longitude of bounding box
#' @param swlat SW latitude of bounding box
#' @param swlng SW longitude of bounding box 
#'
#' @returns A tibble with the columns `observation_id`, `quality_grade`, `captive`, `taxon_name`, `taxon_rank`, `taxon_id`, `observations_count`, `conservation_status`, `threatened`, `endemic`, `introduced`, `iconic_taxa`, `taxon_common_name`, `created_at`, `observed_on`, `time_observed_at`, `uri`, `user_login`,  `user_name`, `user_created_at`, `user_site_id`, `user_observations_count`, `longitude`,  `latitude`, `geoprivacy`, `country_name`, `state_name`
#' @export
#'
#' @examples
#' getDataByBox(nelat=-33, nelng=-56, swlat=-34, swlng=-58)
#' getDataByBox(nelat=-31.8,nelng=-55.1,swlat=-32.8,swlng=-56.5)
# 

getDataByBox <- function(nelat, nelng, swlat, swlng){
  
  dataByBox <- tibble(observation_id = numeric(),
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
  
  total_results <- GET(url = str_glue('{api}?',
                                      'nelat={nelat}&',
                                      'nelng={nelng}&',
                                      'swlat={swlat}&',
                                      'swlng={swlng}',
                                      'page=1&',
                                      'per_page=1')) %>%
    content(as = `text`) %>% fromJSON(flatten = TRUE)
  total_results <- total_results$total_results
  cat(str_glue('{total_results} observations found in total\n'))
  
  if(total_results > 10000){
    cat(str_glue('Too many observations found ({total_results}). The limit is 10,000\n'))
  } else {
    cat('\n downloading ...\n')
    
    per_page = 200 
    for(page in 1:ceiling(total_results/per_page)) {
      cat(str_glue('page {page} of {ceiling(total_results/per_page)} done'), '\n')
      
      call_url <- str_glue('{api}/?',
                           'nelat={nelat}&',
                           'nelng={nelng}&',
                           'swlat={swlat}&',
                           'swlng={swlng}',
                           'page={page}&',
                           'per_page={per_page}')
      
      get_json_call <- GET(url = call_url) %>%
        content(as = `text`) %>% fromJSON(flatten = TRUE)
      
      results <- as_tibble(get_json_call$results)
      dataByBox_i <- tibble(observation_id = results$id,
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
        unnest_wider(geojson.coordinates, names_sep = `_`) %>%
        rename(longitude=geojson.coordinates_1, 
               latitude=geojson.coordinates_2) %>% 
        unnest_wider(place_ids, names_sep = `_`) %>%
        select(-(num_range('place_ids_', c(3:15)))) 
      
      dataByBox <- rbind(dataByBox, dataByBox_i)
      Sys.sleep(2)
    }
    
    countryAndState <- dataByBox %>% 
      distinct(place_ids_1, place_ids_2) %>% 
      mutate(country_name = map_chr(place_ids_1, getCountryName),
             state_name = map_chr(place_ids_2, getStateName))
    
    dataByBox <- left_join(dataByBox, countryAndState,
                           by = join_by(place_ids_1, place_ids_2)) %>% 
      select(-c(place_ids_1, place_ids_2))
    
    return(dataByBox)
  }
}


########################################################################

#' iNat records for a circular limit
#' Get the records on iNaturalist on a circular (point/radius) limit
#'
#' @param lat latitude of a point
#' @param lng longitude of a point
#' @param radius radius kilometer circle around the latitude/longitude
#'
#' @returns A tibble with the columns `observation_id`, `quality_grade`, `captive`, `taxon_name`, `taxon_rank`, `taxon_id`, `observations_count`, `conservation_status`, `threatened`, `endemic`, `introduced`, `iconic_taxa`, `taxon_common_name`, `created_at`, `observed_on`, `time_observed_at`, `uri`, `user_login`,  `user_name`, `user_created_at`, `user_site_id`, `user_observations_count`, `longitude`,  `latitude`, `geoprivacy`, `country_name`, `state_name`
#' 
#'  
#' @export
#'
#' @examples
#' getDataByPointRadius(lat=-33, lng=-56, radius=30)
#' getDataByPointRadius(lat=-31.8,lng=-55.1, radius=10)
# 

getDataByPointRadius <- function(lat, lng, radius){
  
  dataByPointRadius <- tibble(observation_id = numeric(),
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
  
  total_results <- GET(url = str_glue('{api}?',
                                      'lat={lat}&',
                                      'lng={lng}&',
                                      'radius={radius}&',
                                      'page=1&',
                                      'per_page=1')) %>%
    content(as = `text`) %>% fromJSON(flatten = TRUE)
  total_results <- total_results$total_results
  cat(str_glue('{total_results} observations found in total\n'))
  
  if(total_results > 10000){
    cat(str_glue('Too many observations found ({total_results}). The limit is 10,000\n'))
  } else {
    cat('\n downloading ...\n')
    
    per_page = 200 
    for(page in 1:ceiling(total_results/per_page)) {
      cat(str_glue('page {page} of {ceiling(total_results/per_page)} done'), '\n')
      
      call_url <- str_glue('{api}/?',
                           'lat={lat}&',
                           'lng={lng}&',
                           'radius={radius}&',
                           'page={page}&',
                           'per_page={per_page}')
      
      get_json_call <- GET(url = call_url) %>%
        content(as = `text`) %>% fromJSON(flatten = TRUE)
      
      results <- as_tibble(get_json_call$results)
      dataByPointRadius_i <- tibble(observation_id = results$id,
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
        unnest_wider(geojson.coordinates, names_sep = `_`) %>%
        rename(longitude=geojson.coordinates_1, 
               latitude=geojson.coordinates_2) %>% 
        unnest_wider(place_ids, names_sep = `_`) %>%
        select(-(num_range('place_ids_', c(3:15)))) 
      
      dataByPointRadius <- rbind(dataByPointRadius, dataByPointRadius_i)
      Sys.sleep(2)
    }
    
    countryAndState <- dataByPointRadius %>% 
      distinct(place_ids_1, place_ids_2) %>% 
      mutate(country_name = map_chr(place_ids_1, getCountryName),
             state_name = map_chr(place_ids_2, getStateName))
    
    dataByPointRadius <- left_join(dataByPointRadius, countryAndState,
                           by = join_by(place_ids_1, place_ids_2)) %>% 
      select(-c(place_ids_1, place_ids_2))
    
    return(dataByPointRadius)
  }
}
