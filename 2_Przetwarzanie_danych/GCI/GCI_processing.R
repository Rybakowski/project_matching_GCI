setwd("C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/2_Przetwarzanie_danych/GCI")

# Biblioteki ----------------------------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
### Przetwarzanie danych WEF - GCI ### ------------------------------------------------------------

GCI_path <- r"(C:\Users\rados\OneDrive - SGH\Nowy_projekt_BNK\1_Dane_surowe\GCI_Dataset_2006-2016_20170116.xlsx)"

GCI_raw <- read_excel(GCI_path, sheet = "Data", skip = 2)

country_codes <- colnames(GCI_raw) %>%
	keep(.p = ~(nchar(.x) <=3))

dict_countries <- GCI_raw %>% 
	pivot_longer( cols = all_of(country_codes))%>% 
	filter(Dataset == "Dataset") %>% 
	select(name, value)

GCI_df <- GCI_raw %>% 
	pivot_longer( cols = all_of(country_codes)) %>% 
	filter(Dataset != "Dataset") %>% 
	select(-starts_with("GCR"), -`...161`) %>% 
	rename_with(.fn = ~str_replace_all(str_to_lower(str_squish(.x))," ","_"))

colnames(GCI_df) %>%
	discard(.p = ~(nchar(.x) <=3))

dict_variables <- GCI_df %>% 
	separate(series_code, sep = "\\.", into = letters[1:4]) %>% 
	select(all_of(c("edition","global_id",letters[1:4],"series","series_unindented"))) %>%
	distinct() 

dict_variables_mod <- dict_variables %>% 
	mutate(b = ifelse(!is.na(b),paste0("0.",b),b)) %>% 
	mutate(b = as.numeric(b)) %>% 
	mutate(b = round(b, 2)) %>%
	# group_by(pick(all_of(c("global_id",letters[1:4],"series","series_unindented")))) %>% 
	# count()
	mutate(index = 1) %>%
	pivot_wider(names_from = edition,
					values_from = index) 

filter_vars <- dict_variables_mod <- dict_variables %>% 
	mutate(b = ifelse(!is.na(b),paste0("0.",b),b)) %>% 
	mutate(b = as.numeric(b)) %>% 
	mutate(b = round(b, 2)) %>%
	group_by(pick(all_of(c("global_id",letters[1:4],"series","series_unindented")))) %>%
	count() %>% 
	filter(n == 10) %>%
	ungroup() %>% 
	select(global_id) %>% 
	unlist()

GCI_df_val$value %>% unique()

GCI_df_val <- GCI_df %>% 
	filter(attribute == "Value") %>% 
	select(global_id, name, value, edition) %>% 
	extract(edition, into = "year", regex = (".+\\-(\\d{4})$")) %>% 
	filter(global_id %in% filter_vars) %>% 
	filter(global_id != "MALARIAPC") %>% 
	mutate(value = ifelse(value %in% c("n/a","not possible","n/appl.","N/Appl."),NA,value)) %>% 
	mutate(value = str_remove_all(value,"\\<|\\>|\\,")) %>% 
	mutate(value = as.numeric(value)) %>% 
	pivot_wider(names_from = global_id,
					values_from = value) %>% 
	rename(cou = name) 


dicts <- list(countries = dict_countries,
				  variables = dict_variables_mod)
# write_xlsx(dicts, "GCI_dicts.xlsx")
# write_xlsx(GCI_df_val, "GCI_data.xlsx")
