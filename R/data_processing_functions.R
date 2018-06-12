###############################################################################
# Functions used to help process data                                         #
###############################################################################

clean_random_forest_data <- function(data) {
  #' Function to clean the random forests predictors.
  #' 
  #' Some of the random forests predictors need to be certain types, so
  #' this converts them.
  #' 
  #' @param data A data frame of the predictors.
  #' 
  #' @return A dataframe of the cleaned predictors.
  
  data %>%
    mutate(group = factor(group),
           genus = factor(genus),
           family = factor(family),
           order = factor(family),
           realm_value = factor(realm_value),
           species = factor(species)) %>%
    mutate_if(~is.character(.x), as.double) %>%
    mutate(species = as.character(species))
}

deduplicate_by <- function(df, ...) {
  #' Deduplicate a data frame by specified columns.
  #' 
  #' Convenience function to deduplicate the rows of
  #' a data frame, based on the given columns, hopefully
  #' tidy.
  #' 
  #' @param df The data frame to deduplicate.
  #' 
  #' @param ... The names of the columns to deduplicate by.
  #' 
  #' @return The deduplicated data frame.
  
  group_var <- quos(...)
  
  df %>%
    group_by(!!! group_var) %>%
    filter(row_number() == 1) %>%
    ungroup()
}

replace_all_unicode <- function(strings) {
  #' Replace common unicode characters in a string.
  #' 
  #' All unicode characters are replaced in a vector of strings by
  #' removing their accents.
  #' 
  #' @param strings A vector of strings.
  #' 
  #' @return A vector of strings with no unicode special characters in.
  
  to_replace <- c("á", "ã", "é", "í", "ó", "ô")
  replacements <- c("a", "a", "e", "i", "o", "o")
  names(replacements) <- to_replace
  
  strings %>%
    str_replace_all(str_c(to_replace, collapse="|"), 
                    function(x) replace_character(x, replacements))
}

replace_character <- function(letter, replacements) {
  #' Replace a character using a lookup table.
  #' 
  #' @letter The letter to replace.
  #' 
  #' @replacements A named vector of replacements
  
  replacements[letter]
}

process_locations <- function(data) {
  #' Process locality info to give a location for the US method.
  #' 
  #' This cleans and standardises location strings based on the group.
  #' 
  #' @param data A data frame containing the species and location information.
  #' 
  #' @return A data frame with the cleaned location information.
   
  data %>%
    mutate(locality = case_when(is.na(majorarea) & group == "ng_orchids_p" ~ minorarea,
                                TRUE ~ majorarea)) %>%
    select(-majorarea, -minorarea) %>%
    group_by(species) %>%
    filter(any(!is.na(year))) %>%
    mutate(locality = case_when(group == "myrts" ~ clean_myrts(locality),
                                TRUE ~ clean_default(locality))) %>%
    group_by(species) %>%
    mutate(locality = case_when(all(is.na(locality)) ~ "unknown",
                                TRUE ~ locality))
    
}

clean_myrts <- function(location) {
  #' Clean a location string for the Myrcia group.
  #' 
  #' @param location A location string to be cleaned.
  #' 
  #' @return A cleaned location string.
  
  location %>%
    str_to_lower() %>%
    str_remove_all("[\\(\\)\\.]+") %>%
    replace_all_unicode() %>%
    str_squish() %>%
    str_replace("st ", "saint ") %>%
    recode(ba = "bahia",
           df = "distrito federal",
           es = "espirito santo",
           `mata grosso` = "mato grosso",
           `matto grosso` = "mato grosso",
           `peru, loreto, maynas` = "loreto",
           sp = "sao paulo",
           `e berbice- corentyne region` = "east berbice-corentyne",
           `cuyuni-mazaruni` = "cuyuni-maxzaruni")
}

clean_default <- function(location, group=NULL) {
  #' Clean a location string for all other groups.
  #' 
  #' @param location A location string to be cleaned.
  #' 
  #' @return A cleaned location string.
  
  location %>%
    str_remove_all("province") %>%
    str_remove_all("prov") %>%
    str_remove_all("regency") %>%
    str_remove_all("dist") %>%
    str_remove_all("highlands") %>%
    str_remove_all("[\\(\\)\\.\\s]+") %>%
    recode(`brunei-maura` = "brunei and maura",
           `bangka belitung island` = "bangka",
           `belatit` = "belait",
           toliary = "toliara")
}

remove_bad_coordinates <- function(data) {
  #' Remove records with bad coordinates from the group.
  #' 
  #' @param data A data frame containing specimen and coordinate information.
  #' 
  #' @return The data with bad coordinates removed.
  
  data %>%
    filter(!is.na(latitude), 
           !is.na(longitude), 
           latitude != 0 & longitude != 0, 
           latitude > -9999, 
           longitude > -9999)
}














