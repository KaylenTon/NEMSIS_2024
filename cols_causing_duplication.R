library(dplyr)
library(tidyverse)
library(ggplot2)
library(mgsub)

  
## COMBINE WITH "_"
paste_by_underscore <- function() {
  # Retrieve duplicate PcrKeys from clean_NA
  dupe_pcrkeys <- clean_NA$PcrKey[duplicated(clean_NA$PcrKey)]
  duplicates <- clean_NA %>%
    filter(PcrKey %in% dupe_pcrkeys)
  
  #find_diffs() checks which columns have conflicting values that are causing duplicate PcrKeys
  #example: 
  #         PcrKey | e1 | e2 | e3
  #         123     1     1     2
  #         123     4     1     2
  #         123     2     1     3
  #return: 
  #         PcrKey | e1 | e2 | e3 | diff_cols
  #         123     1     1     2   e1; e3
  #         123     4     1     2   e1; e3
  #         123     2     1     3   e1; e3
  #explanation: diff_cols is e1; e3 because e1 and e3 have conflicting values (2 and 3) for PcrKey 123
  
  find_diffs <- function(df) {
    varying <- names(Filter(function(col) n_distinct(col) > 1, df)) #keep col that has more than 1 unique value (i.e., all values for that col are NOT the same)
    paste(varying, collapse = "; ") #take the headers and glue them to one string, e.g., "ePatient_14; eResponse_09"
  }
  
  dupe_cols_df <- duplicates %>%
    group_by(PcrKey) %>% #group all rows by PcrKey
    mutate(diff_cols = find_diffs(pick(everything()))) %>% #creates "diff_col" column, grabs all data for that specific PcrKey and applies the "find_diffs" function
    ungroup()
  
  frequency_tbl <- dupe_cols_df %>%
    distinct(PcrKey, .keep_all= TRUE) %>%
    filter(diff_cols != "") %>%
    separate_rows(diff_cols, sep = ";\\s*") %>% 
    count(diff_cols, sort = TRUE) 
  
  ## Retrieve only the columns causing duplicate PcrKeys
  
  dupe_cols <- c("PcrKey", frequency_tbl$diff_cols) 
  dupe_cols_only <- duplicates[, (colnames(duplicates) %in% dupe_cols)] 
  
  dupe_cols_only <- dupe_cols_only %>%
    left_join(dupe_cols_df %>% 
                dplyr::select(PcrKey, diff_cols)%>% 
                distinct(PcrKey, .keep_all = TRUE), by = "PcrKey") %>% 
    filter(diff_cols != "")
  
  dupe_col_names <- colnames(dupe_cols_only)
  dupe_col_names <- dupe_col_names[!dupe_col_names %in% c("PcrKey", "diff_cols")]
  
  
  ## Combine conflicting values into one PcrKey (pasting together with "_" into a string)
  
  clean_str <- function(x) {
    x %>%
      str_remove_all("Yes|e.g\\.|,|\\(|\\)") %>% 
      str_replace_all("/|\\-| ", "_") %>%
      str_replace_all("_+", "_") %>%
      str_trim()
  }
  
  dupe_cols_cln <- dupe_cols_df %>%
    mutate(across(.cols = all_of(dupe_col_names) & where(is.character),
                  .fns = ~clean_str(.)))
  
  temp_clean_NA <- clean_NA %>%
    mutate(across(.cols = all_of(dupe_col_names) & where(is.character),
                  .fns = ~clean_str(.)))
 
  
  dupe_cols_mer <- dupe_cols_cln %>%
    group_by(PcrKey) %>%
    summarise(
      #calculate the duration for each pcrkey in datetime
      dt_of_dpaa_duration = if(all(is.na(datetime_of_destination_prearrival_alert_or_activation))) {
        as.numeric(NA)
      } else {
        max_date <- max(datetime_of_destination_prearrival_alert_or_activation, na.rm = TRUE)
        min_date <- min(datetime_of_destination_prearrival_alert_or_activation, na.rm = TRUE)
        
        as.numeric(difftime(max_date, min_date, units = "secs"))
      },
      
      #save only the max (most recent) date for datetime
      datetime_of_destination_prearrival_alert_or_activation = if(all(is.na(datetime_of_destination_prearrival_alert_or_activation))) {
        as.POSIXct(NA)
      }else{
        max(datetime_of_destination_prearrival_alert_or_activation, na.rm = TRUE) 
      },
      
      #merge all other rows duplicated by PcrKey together
      across(.cols = where(is.character) & -any_of(c("PcrKey","diff_cols")),
             .fns = ~paste(unique(na.omit(.)), collapse = "_")),
      #keep original col types of other cols
      across(
        .cols = !where(is.character) & -any_of(c("PcrKey", "dt_of_dpaa_duration", "datetime_of_destination_prearrival_alert_or_activation")),
        .fns = ~first(na.omit(.))
      ),
      .groups = "drop")
  
  ## Combine clean_NA and dupe_cols_mer together
  
  # Remove all duplicated keys from the clean_NA so we can replace it with the rows from dupe_cols_mer
  clean_NA_drop <- temp_clean_NA %>%
    filter(!PcrKey %in% dupe_pcrkeys)
  
  final_clean_NA <- bind_rows(clean_NA_drop, dupe_cols_mer)
  
  #### Check for left over duplicates
  clean_Na_distinct <- clean_NA %>%
    distinct(PcrKey, .keep_all = TRUE)
  
  # final check to make sure merge went properly
  leaked_dupes <- final_clean_NA %>%
    count(PcrKey) %>%
    filter(n > 1)
  cat("Duplicated values in final_clean_NA (If not 0, fix final merge in function):",
      nrow(leaked_dupes),
      "Distinct of clean_NA is equal to the nrow of dropped rows of duplicate PcrKeys + nrow of merged PcrKeys:", 
      nrow(clean_Na_distinct) == nrow(dupe_cols_mer) + nrow(clean_NA_drop), 
      sep = "\n")
    
    return(final_clean_NA)
}

## REPLACE DUPLICATE VALUES WITH "MULTIPLE"
#currently, this function is not working as intended. Please go back and fix it later - 4/7/2026
paste_by_multiple <- function() {
  # Retrieve duplicate PcrKeys from clean_NA
  dupe_pcrkeys <- clean_NA$PcrKey[duplicated(clean_NA$PcrKey)]
  duplicates <- clean_NA %>%
    filter(PcrKey %in% dupe_pcrkeys)
  
  find_diffs <- function(df) {
    varying <- names(Filter(function(col) n_distinct(col) > 1, df)) #keep col that has more than 1 unique value (i.e., all values for that col are NOT the same)
    paste(varying, collapse = "; ") #take the headers and glue them to one string, e.g., "ePatient_14; eResponse_09"
  }
  
  dupe_cols_df <- duplicates %>%
    group_by(PcrKey) %>% #group all rows by PcrKey
    mutate(diff_cols = find_diffs(pick(everything()))) %>% #creates "diff_col" column, grabs all data for that specific PcrKey and applies the "find_diffs" function
    ungroup()
  
  frequency_tbl <- dupe_cols_df %>%
    distinct(PcrKey, .keep_all= TRUE) %>%
    filter(diff_cols != "") %>%
    separate_rows(diff_cols, sep = ";\\s*") %>% 
    count(diff_cols, sort = TRUE) 
  
  ## Retrieve only the columns causing duplicate PcrKeys
  
  dupe_cols <- c("PcrKey", frequency_tbl$diff_cols) 
  dupe_cols_only <- duplicates[, (colnames(duplicates) %in% dupe_cols)] 
  
  dupe_cols_only <- dupe_cols_only %>%
    left_join(dupe_cols_df %>% 
                dplyr::select(PcrKey, diff_cols)%>% 
                dplyr::distinct(PcrKey, .keep_all = TRUE), by = "PcrKey") %>% 
    dplyr::filter(diff_cols != "")
  
  dupe_col_names <- colnames(dupe_cols_only)
  dupe_col_names <- dupe_col_names[!dupe_col_names %in% c("PcrKey", "diff_cols", "datetime_of_destination_prearrival_alert_or_activation")]
  
  #specify columns to exclude from dupe_col_names, so merging skips these columns
  exclude_cols <- c("PcrKey", "datetime_of_destination_prearrival_alert_or_activation")
  
  collapsed_clean_NA <- dupe_cols_df %>%
    group_by(PcrKey) %>%
    summarise(
      dt_of_dpaa_duration = if(all(is.na(datetime_of_destination_prearrival_alert_or_activation))) {
        as.numeric(NA)
      } else {
        max_date <- max(datetime_of_destination_prearrival_alert_or_activation, na.rm = TRUE)
        min_date <- min(datetime_of_destination_prearrival_alert_or_activation, na.rm = TRUE)
        
        as.numeric(difftime(max_date, min_date, units = "secs"))
      },
      
      #save only the max (most recent) date for datetime
      datetime_of_destination_prearrival_alert_or_activation = if(all(is.na(datetime_of_destination_prearrival_alert_or_activation))) {
        as.POSIXct(NA)
      }else{
        max(datetime_of_destination_prearrival_alert_or_activation, na.rm = TRUE) 
      },
      
      across(all_of(dupe_col_names[!dupe_col_names %in% exclude_cols]
      ), 
      ~ {
        unique_vals <- unique(na.omit(.x))
        
        if(length(unique_vals)>1){
          "Multiple"
        } else if (length(unique_vals) == 1) {
          as.character(unique_vals)
        } else {
          NA_character_
        }
      }
      ),
      
      across(-all_of(c(dupe_col_names,
                       "datetime_of_destination_prearrival_alert_or_activation",
                       "dt_of_dpaa_duration")),
             ~first(na.omit(.x))),
      
      .groups = "drop")
  
  
  #remove "diff_cols" from collapsed_clean_NA
  collapsed_clean_NA <- collapsed_clean_NA[,!names(collapsed_clean_NA) %in% "diff_cols"]
  
  # Remove all duplicated keys from the clean_NA so we can replace it with the rows from dupe_cols_mer
  clean_NA_drop <- clean_NA %>%
    filter(!PcrKey %in% dupe_pcrkeys)
  
  final_clean_NA <- bind_rows(clean_NA_drop, collapsed_clean_NA)
  
  #### Check for left over duplicates
  clean_Na_distinct <- clean_NA %>%
    distinct(PcrKey, .keep_all = TRUE)
  
  # final check to make sure merge went properly
  leaked_dupes <- final_clean_NA %>%
    count(PcrKey) %>%
    filter(n > 1)
  cat("Duplicated values in final_clean_NA (If not 0, fix final merge in function):",
      nrow(leaked_dupes),
      "Distinct of clean_NA is equal to the nrow of dropped rows of duplicate PcrKeys + nrow of merged PcrKeys:", 
      nrow(clean_Na_distinct) == nrow(collapsed_clean_NA) + nrow(clean_NA_drop), 
      sep = "\n")
  
  return(final_clean_NA)
}
