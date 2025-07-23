#' Title
#'
#' @param species_name 
#'
#' @return
#' @export
#'
#' @examples
getPhotoFromiNat <- function(species_name){
  
  species_name <- str_to_lower(species_name)

  api <- 'https://api.inaturalist.org/v1'
  page <- '&page=1&per_page=1'
  
  call_url_species <- str_glue('{api}/taxa?q={species_name}{page}')
  get_json_call_place <- GET(url = URLencode(call_url_species)) %>%
    content(as = "text") %>% fromJSON(flatten = TRUE)
  results <- as_tibble(get_json_call_place$results)
  if(nrow(results) != 0){
    photo <- results$default_photo.medium_url
  } else {
    photo <- NA
  }
  return(photo)
}



getPhotoFromiNat <- function(species_list, place_id, license){
  
  species_photos <- tibble(species_name = character(),
                           taxon_id = numeric(),
                           observation_url = character(),
                           photo_url = character())

  api <- 'https://api.inaturalist.org/v1/observations'
  page <- '&page=1&per_page=10'
  
  for(species_name in species_list){
    
    call_url <- str_glue('{api}?photo_license={license}&place_id={place_id}&q={species_name}&order=desc&order_by=votes{page}')
    
    get_json_call <- GET(url = URLencode(call_url)) %>%
      content(as = "text") %>% fromJSON(flatten = TRUE)
    
    results <- as_tibble(get_json_call$results) %>%
      slice_head(n=1)

    species_photos_i <- tibble(species_name = species_name,
                               taxon_id = results$taxon.id,
                               user_login = results$user.login,
                               observation_url = str_replace(results$uri,
                                                             'inaturalist.org',
                                                             'naturalista.uy'),
                               photo_url = results$photos)
    
    species_photos <- rbind(species_photos, species_photos_i)
  }
  
  species_photos <- species_photos %>% 
    unnest_wider(photo_url, names_sep = "_") %>% 
    unnest_wider(col=c(photo_url_license_code, photo_url_url), names_sep = "_") %>%
    select(-c(photo_url_id, photo_url_attribution, 
              photo_url_flags, photo_url_moderator_actions,
              photo_url_hidden, photo_url_original_dimensions.width, 
              photo_url_original_dimensions.height)) %>% 
    relocate(starts_with('photo_url_url'), 
             .before = starts_with('photo_url_license')) %>% 
    rename_with(~ str_replace(.x, 'photo_url_url_', 'photo_url_'), 
                starts_with('photo_url_url_')) %>% 
    rename_with(~ str_replace(.x, 'photo_url_license', 'photo_license'), 
                starts_with('photo_url_license'))
  
  return(species_photos)
}
