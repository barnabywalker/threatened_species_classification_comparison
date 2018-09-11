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
           species = factor(species),
           criteria = factor(criteria)) %>%
    mutate_if(~is.character(.x), as.double) %>%
    mutate(species = as.character(species),
           criteria = as.character(criteria))
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

get_locality_from_level <- function(df, level) {
  #' Gets the locality from a specified locality level.
  #' 
  #' Fills missing values in locality column from a specified locality level.
  #' First creates a blank locality column if one didn't exist before.
  #' 
  #' @param df: Dataframe with specimen information at all locality levels.
  #' 
  #' @param level: Locality level to get value from.
  #' 
  #' @return The dataframe with locality information from the specified level.
  
  locality_level <- paste0("locality_", level)
  
  if (!("locality" %in% colnames(df))) {
    df <- mutate(df, locality = NA_character_)
  }
  
  df %>%
    mutate(locality = case_when(is.na(locality) ~ !! rlang::sym(locality_level),
                                TRUE ~ locality))
}

fill_locality <- function(df, locality_level) {
  #' Fill locality from standardised locality levels.
  #' 
  #' Starts by getting locality information from the desired levels,
  #' then fills missing values from successively lower levels.
  #' 
  #' @param df: Dataframe with specimen and locality information.
  #' 
  #' @param desired_level: The level to start filling at.
  #' 
  #' @return The dataframe with a full locality column.
  
  filled_df <- df
  
  while (locality_level >= 0) {
    filled_df <- get_locality_from_level(filled_df, locality_level)
    locality_level <- locality_level - 1
  }
  
  return(filled_df)
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
           (latitude != 0 & longitude != 0), 
           latitude > -9999, 
           longitude > -9999)
}














