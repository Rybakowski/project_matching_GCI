setwd("C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/2_Przetwarzanie_danych/Data_merging")

# Biblioteki ----------------------------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(naniar)
library(neverhpfilter)


### Przetwarzanie danych WEF - GCI ### ------------------------------------------------------------

GCI_path <- "C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/2_Przetwarzanie_danych/GCI/GCI_data.xlsx"
PENN_path <- r"(C:\Users\rados\OneDrive - SGH\Nowy_projekt_BNK\1_Dane_surowe\pwt1001.xlsx)"
GCI_dict <- "C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/2_Przetwarzanie_danych/GCI/GCI_dicts.xlsx"
	
GCI_dict <- read_xlsx(GCI_dict, sheet = "variables")
GCI_raw <- read_xlsx(GCI_path)
PENN_raw <- read_xlsx(PENN_path, sheet = "Data")



# function ------------------------------------------------------------------------------------

my_hamilton <- function(dates, column){
	temp <- column
	names(temp) <- dates
	temp <- as.xts(as.data.frame(temp))
	index(temp) <- dates
	temp <- yth_filter(temp, h = 4, p = 2)
	output <- zoo::coredata(temp)[,2]
	return(output)
}


# Processing GWI ------------------------------------------------------------------------------



GCI_df <- GCI_raw %>% 
	rename_with(-c(year, cou), .fn = ~paste0("wf_",.x))  
	
GCI_na <- naniar::miss_var_summary(GCI_df) %>% 
	filter(pct_miss == 8.750000) %>% 
	select(variable) %>% 
	unlist() %>% 
	unname()

temp <- naniar::miss_case_summary(GCI_df)

to_filter <- temp %>%
	filter(n_miss > 8) %>%
	select(case) %>%
	unlist()

countries_to_rem <- GCI_df %>% 
	select(cou, year, all_of(GCI_na)) %>% # selecting only fool dataset
	slice(to_filter) %>% 
	distinct(cou) %>% 
	unlist() 

GCI_df2 <- GCI_df %>% 
	select(cou, year, all_of(GCI_na)) %>% # selecting only fool dataset
	filter(!cou %in% countries_to_rem)

PENN_df <-  PENN_raw %>%  
	rename(cou = countrycode) %>% 
	rename_with(-c(year, cou), .fn = ~paste0("pn_",.x)) %>% 
	select(year, cou, pn_csh_x, pn_csh_i, pn_csh_g, pn_hc, pn_rgdpna, pn_pop) %>% 
	group_by(cou) %>% 
	mutate(y = log(pn_rgdpna/pn_pop)) %>%
	mutate(date = as.Date(paste0(year,"-01-01", formate = "%Y-%m-%d"))) %>% 
	ungroup()

HAMILTON_df <- PENN_df %>% 
	select(date, cou, y) %>% 
	pivot_wider(names_from = cou,
					values_from = y) %>% 
	mutate(across(
		.cols = where(is.numeric),
		.fns = ~my_hamilton(date,.x)
	)) %>% 
	pivot_longer(
		cols = -date,
		names_to = "cou",
		values_to = "y_ham"
	) %>% 
	arrange(cou, date)

PENN_df_2 <- PENN_df %>% 
	left_join(HAMILTON_df) %>% 
	group_by(cou) %>% 
	mutate(y_d = y/dplyr::lag(y) - 1) %>% 
	mutate(y_ham_d = y_ham/dplyr::lag(y_ham) - 1) %>% 
	ungroup()


data_df <- GCI_df2 %>% 
	mutate(year = as.numeric(year)) %>% 
	left_join(PENN_df_2, by = c("cou", "year")) %>% 
	filter(!cou %in% c("AZE", "GEO", "MKD", "MNE", "OMN", "TCD")) %>% 
	select(year, cou, everything()) %>% 
	arrange(cou, year) 
	# filter(if_any(.cols = everything(),
	# 				  .fn = is.na)) %>% 
	# count(cou) %>% select(cou) %>% unlist()

dict_new <- tibble(variables = colnames(GCI_df2)) %>% 
	filter( !variables %in% c("cou","year")) %>% 
	mutate(variables = str_remove(variables, "^wf_")) %>% 
	left_join(GCI_dict, by = c("variables" = "global_id")) %>% 
	select(variables:series_unindented) 

countries_final <- data_df %>% 
	distinct(cou) %>% 
	left_join(PENN_raw, by = c("cou" = 'countrycode')) %>% 
	distinct(cou, country) 
	
dict_final <- list(countries = countries_final,
						 GCI = dict_new)

# write_xlsx(data_df, "database_2023_12_22.xlsx")

# write_xlsx(dict_final, "dict_master.xlsx")

