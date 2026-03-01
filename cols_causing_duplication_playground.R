library(dplyr)
library(tidyverse)
library(ggplot2)
library(mgsub)
library(philentropy)

## TEST AREA -- DELETE LATER
# this is the playground for testing duplicate pcrkey stuff. This file is temporary, so please delete later when final function for imputing duplicate values has been finished.
# Thanks!

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

# frequency_tbl <- dupe_cols_df %>%
#   distinct(PcrKey, .keep_all= TRUE) %>%
#   filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
#   separate_rows(diff_cols, sep = ";\\s*") %>% #break values with ";" into two rows
#   count(diff_cols, sort = TRUE) #counts the number of each unique value in "diff_cols"

frequency_tbl <- dupe_cols_df %>%
  filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
  separate_rows(diff_cols, sep = ";\\s*") %>% #break values with ";" into two rows
  count(diff_cols, sort = TRUE) #counts the number of each unique value in "diff_cols"

frequency_tbl_p <- dupe_cols_df %>%
  filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
  separate_rows(diff_cols, sep = ";\\s*") %>%
  group_by(diff_cols) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

dupe_example <- dupe_cols_df %>%
  filter(PcrKey == "282501085") %>%
  subset(select = c(PcrKey, 	
                    datetime_of_destination_prearrival_alert_or_activation, 	
                    destination_team_prearrival_alert_or_activation, diff_cols))

frq_ex <- dupe_cols_df %>%
  filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
  separate_rows(diff_cols, sep = ";\\s*") %>% #break values with ";" into two rows
  filter(PcrKey == "282501085") %>%
  subset(select = c(PcrKey, 	
                    datetime_of_destination_prearrival_alert_or_activation, 	
                    destination_team_prearrival_alert_or_activation, diff_cols))

## Retrieve only the columns causing duplicate PcrKeys

dupe_cols <- c("PcrKey", frequency_tbl$diff_cols) #just get the names of the duplicate columns
dupe_cols_only <- duplicates[, (colnames(duplicates) %in% dupe_cols)] #subset the duplicates table with the names from "dupe_cols"

dupe_cols_only <- dupe_cols_only %>%
  left_join(dupe_cols_df %>% #join this to the dupe_cols_only df
              select(PcrKey, diff_cols)%>% #get just the PcrKey col and "diff_col" row
              distinct(PcrKey, .keep_all = TRUE), by = "PcrKey") %>% #select only the distinct PcrKey values (remove duplicates and just keep the reasons)
  filter(diff_cols != "")


dupe_col_names <- colnames(dupe_cols_only)


## Combine conflicting values into one PcrKey (pasting together with "_" into a string)

clean_str <- function(x) {
  x %>%
    str_remove_all("Yes|e.g\\.|,|\\(|\\)") %>% 
    str_replace_all("/|\\-|or| ", "_") %>%
    str_replace_all("_+", "_") %>%
    str_trim()
}

dupe_cols_cln <- dupe_cols_df %>%
  mutate(across(.cols = all_of(dupe_col_names) & where(is.character),
                .fns = ~clean_str(.)))


dupe_col_names <- dupe_col_names[!dupe_col_names %in% c("PcrKey", "diff_cols")]

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
           .fns = ~paste(sort(unique(na.omit(.))), collapse = "_")),
    
    #keep original col types of other cols
    across(
      .cols = !where(is.character) & -any_of(c("PcrKey", "dt_of_dpaa_duration", "datetime_of_destination_prearrival_alert_or_activation")),
      .fns = ~first(na.omit(.))
    ),
    .groups = "drop")

## Combine clean_NA and dupe_cols_mer together

# Remove all duplicated keys from the clean_NA so we can replace it with the rows from dupe_cols_mer
clean_NA_drop <- clean_NA %>%
  filter(!PcrKey %in% dupe_pcrkeys)

final_clean_NA <- bind_rows(clean_NA_drop, dupe_cols_mer)



### NEW FUNCTION: Duplicate values turn into "Mixed"


dupe_pcrkeys <- clean_NA$PcrKey[duplicated(codes_NA$PcrKey)]
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
  filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
  separate_rows(diff_cols, sep = ";\\s*") %>% #break values with ";" into two rows
  count(diff_cols, sort = TRUE) #counts the number of each unique value in "diff_cols"

frequency_tbl_p <- dupe_cols_df %>%
  filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
  separate_rows(diff_cols, sep = ";\\s*") %>%
  group_by(diff_cols) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))


## Retrieve only the columns causing duplicate PcrKeys

dupe_cols <- c("PcrKey", frequency_tbl$diff_cols) #just get the names of the duplicate columns
dupe_cols_only <- duplicates[, (colnames(duplicates) %in% dupe_cols)] #subset the duplicates table with the names from "dupe_cols"

dupe_cols_only <- dupe_cols_only %>%
  left_join(dupe_cols_df %>% #join this to the dupe_cols_only df
              select(PcrKey, diff_cols)%>% #get just the PcrKey col and "diff_col" row
              distinct(PcrKey, .keep_all = TRUE), by = "PcrKey") %>% #select only the distinct PcrKey values (remove duplicates and just keep the reasons)
  filter(diff_cols != "")


dupe_col_names <- colnames(dupe_cols_only)


## Combine conflicting values into one PcrKey (pasting together with "_" into a string)

clean_str <- function(x) {
  x %>%
    str_remove_all("Yes|e.g\\.|,|\\(|\\)") %>% 
    str_replace_all("/|\\-|or| ", "_") %>%
    str_replace_all("_+", "_") %>%
    str_trim()
}

dupe_cols_cln <- dupe_cols_df %>%
  mutate(across(.cols = all_of(dupe_col_names) & where(is.character),
                .fns = ~clean_str(.)))


dupe_col_names <- dupe_col_names[!dupe_col_names %in% c("PcrKey", "diff_cols", "datetime_of_destination_prearrival_alert_or_activation")]

dupe_cols_mer <- dupe_cols_df %>%
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
           .fns = ~paste(sort(unique(na.omit(.))), collapse = "_")),
    
    #keep original col types of other cols
    across(
      .cols = !where(is.character) & -any_of(c("PcrKey", "dt_of_dpaa_duration", "datetime_of_destination_prearrival_alert_or_activation")),
      .fns = ~first(na.omit(.))
    ),
    .groups = "drop")

# MERGE BY MULTIPLE
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
    
    across(
      all_of(dupe_col_names), ~ {unique_vals <- unique(na.omit(.x))
      
      if(length(unique_vals > 1)){
        "Multiple"
      } else if (length(unique_vals) == 1) {
        as.character(unique_vals)
      } else {
        NA_character_
      }
      }),
    .groups = "drop")

## Combine clean_NA and dupe_cols_mer together

# Remove all duplicated keys from the clean_NA so we can replace it with the rows from dupe_cols_mer
clean_NA_drop <- clean_NA %>%
  filter(!PcrKey %in% dupe_pcrkeys)

final_clean_NA <- bind_rows(clean_NA_drop, dupe_cols_mer)
final_clean_NA <- bind_rows(clean_NA_drop, collapsed_clean_NA)

### KL DIVERGENCE - Still needed?
calc_probdist <- function(df){
  df <- df %>%
    select(any_of(c("PcrKey", dupe_col_names))) %>%
    pivot_longer(any_of(dupe_col_names), values_transform = list(value = as.character)) %>%
    distinct() %>%
    count(name, value) %>%
    group_by(name) %>%
    mutate(prob = n/sum(n)) %>%
    ungroup()
  
}

orig_p_dist <- calc_probdist(codes_NA)
orig_p_dist <- orig_p_dist$prob

q_dist <- calc_probdist(final_codes_NA)
q_dist <- q_dist$prob

#calculate kl divergence



#proportion of "Multiple" in each dupe col - DATA LOST
dupe_cols <- c("PcrKey", frequency_tbl$diff_cols) #just get the names of the duplicate columns
dupe_cols <- dupe_cols[!dupe_cols  %in% c("PcrKey", "diff_cols","datetime_of_destination_prearrival_alert_or_activation")]
check_clean_NA <- final_clean_NA[, (colnames(final_clean_NA) %in% dupe_cols)] #subset the duplicates table with the names from "dupe_cols"

frequency_tbl_mult <- check_clean_NA %>%
  pivot_longer(everything(), names_to = "column_name", values_to = "status" ) %>%
  group_by(column_name) %>%
  summarise(
    total = n(),
    multiple = sum(status == "Multiple", na.rm = TRUE),
    prob_multiple = multiple / total,
    .groups = "drop"
  ) %>%
  arrange(desc(prob_multiple))
sum(frequency_tbl_mult$prob_multiple) #12% data lost
