library(dplyr)
library(ggplot2)

duplicates <- use_data[duplicated(use_data$PcrKey),]
nrow(duplicates)

#check which columns have conflicting values that are causing duplicate PcrKeys
#example: 
#         PcrKey | e1 | e2 | e3
#         123     1     1     2
#         123     1     1     2
#         123     2     1     3
#return: 
#         PcrKey | e1 | e2 | e3 | diff_cols
#         123     1     1     2   e1; e3
#         123     1     1     2   e1; e3
#         123     2     1     3   e1; e3

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
  filter(diff_cols != "") %>% #remove rows with no value in the "diffs_col" column (not a duplicate)
  separate_rows(diff_cols, sep = ";\\s*") %>% #break values with ";" into two rows
  count(diff_cols, sort = TRUE) #counts the number of each unique value in "diff_cols"

ggplot(frequency_tbl, aes(x = diff_cols, y = n)) +
  geom_col() +
 coord_flip() +
  labs(title = "Frequency of Cols Causing PcrKey Duplication") +
  theme_minimal()
