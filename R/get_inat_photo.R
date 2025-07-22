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
