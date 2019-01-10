library(tabulizer)
library(dplyr)
library(tidyr)
library(janitor)

dataframe_from_list<-function(mylist){
  data.frame(mylist, stringsAsFactors = FALSE) %>%
    mutate_all(funs(na_if(.,""))) %>% 
    remove_empty("cols") %>% 
    rename(vdrc_id=1, cg_number=2, gene_name=3, primary_score=4, 
           secondary_score=5, physical_abnormality=6,
           mean_activity_difference=7,act_sig=8)
}
  

increasedPI <- tabulizer::extract_tables("~/Documents/R_projects/Walkinshaw/Walkinshaw2016.pdf", 
                                         pages=5) 
# Extract Table 2 located on page 5 of the manuscript


df_increasedPI <- 
  # convert to dataframe
  as_data_frame(increasedPI[[1]]) %>% 
  # Remove rows containing column names
  slice(-1:-2) %>%
  # Add proper column names
  dataframe_from_list() %>% 
  # Add column to indicate sign of change in memory
  mutate(change_in_memory = "+")

decreasedPI <- tabulizer::extract_tables("~/Documents/R_projects/Walkinshaw/Walkinshaw2016.pdf", 
                                         pages=13:38)

df_decreasedPI<-lapply(decreasedPI, dataframe_from_list)
df_decreasedPI<-df_decreasedPI %>%
  bind_rows() %>% 
  # Remove the first row containing column names
  slice(-1) %>%
  # Add column to indicate significant reduction in memory score
  mutate(change_in_memory = "-") %>% 
  mutate(act_sig=if_else(!is.na(mean_activity_difference), "", act_sig))
 

significant_lines <- bind_rows(df_decreasedPI, df_increasedPI) %>% 
  # Put PI and SEM in separate columns 
  separate(primary_score, c("primary_PI", "dummy", "primary_SEM"), " ")%>%
  dplyr::select(-dummy) %>%
  separate(secondary_score, c("secondary_PI", "dummy", "secondary_SEM"), " ") %>%
  # remove +/- character and gene names
  dplyr::select(-dummy, -gene_name) %>%
  # Remove all rows that don't have a valid vdrc_id
  filter(vdrc_id != "") %>%
  # Encode physical abnormality by + or -
  mutate(physical_abnormality = replace(physical_abnormality, 
                                        physical_abnormality != "+", "-")) %>%
  # convert VDRC identifiers to integers
  mutate(vdrc_id = strtoi(vdrc_id)) %>%
  # Convert PI and SEM to numeric
  mutate(primary_PI = as.double(primary_PI), 
         primary_SEM = abs(as.double(primary_SEM))) %>%
  mutate(secondary_PI = as.double(secondary_PI), 
         secondary_SEM = abs(as.double(secondary_SEM))) %>%
  # Convert Mean Activity Difference to numeric and add column to indicate increase in memory
  mutate(mean_activity_difference = as.double(mean_activity_difference)) %>% 
  # Sort the dataframe by the VDRC identifier
  arrange(vdrc_id)