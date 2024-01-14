setwd("C:/Users/rados/OneDrive - SGH/project_matching_GCI/2_Przetwarzanie_danych/Data_merging")

# Biblioteki ----------------------------------------------------------------------------------

library(readxl)
library(writexl)
library(tidyverse)
library(naniar)
library(neverhpfilter)
?yth_filter

### Przetwarzanie danych WEF - GCI ### ------------------------------------------------------------

GCI_path <- "C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/2_Przetwarzanie_danych/GCI/GCI_data.xlsx"
PENN_path <- r"(C:\Users\rados\OneDrive - SGH\Nowy_projekt_BNK\1_Dane_surowe\pwt1001.xlsx)"
GCI_dict <- "C:/Users/rados/OneDrive - SGH/Nowy_projekt_BNK/2_Przetwarzanie_danych/GCI/GCI_dicts.xlsx"
tymek_path <- r"(C:\Users\rados\OneDrive - SGH\project_matching_GCI\2_Przetwarzanie_danych\wskaźniki_diff_in_diff.csv)"

GCI_dict <- read_xlsx(GCI_dict, sheet = "variables")
GCI_raw <- read_xlsx(GCI_path)
PENN_raw <- read_xlsx(PENN_path, sheet = "Data")
tymek_raw <- read_csv(tymek_path)


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


# Freedom & Polity 2 --------------------------------------------------------------------------

tymek_df <- tymek_raw %>%
	select(countrycode, year, FP.CPI.TOTL.ZG, polity2, Index.of.Economic.Freedom) %>% 
	rename(pn_wbinf = FP.CPI.TOTL.ZG,
			 pn_freedom = Index.of.Economic.Freedom) %>%
	rename(cou = countrycode) %>% 
	select(year, cou, pn_freedom, pn_wbinf) %>% 
	mutate(pn_freedom = as.numeric(pn_freedom))
# 	
# temp2 <- tymek_df %>%
# 	filter(year > 2006) %>%
# 	select(cou, polity2) %>%
# 	group_by(cou) %>%
# 	miss_var_summary()


# Processing GWI ------------------------------------------------------------------------------

GCI_df <- GCI_raw %>% 
	rename_with(-c(year, cou), .fn = ~paste0("wf_",.x))  
	
GCI_na <- naniar::miss_var_summary(GCI_df) %>% 
	filter(pct_miss == 8.750000) %>% 
	select(variable) %>% 
	unlist() %>% 
	unname()

GCI_na_2 <- setdiff(colnames(GCI_df), c("wf_EOSQ168", "cou","year"))

temp <- naniar::miss_case_summary(GCI_df)

to_filter <- temp %>%
	filter(n_miss > 8) %>%
	select(case) %>%
	unlist()

countries_to_rem <- GCI_df %>% 
	select(cou, year, all_of(GCI_na)) %>% # selecting only full dataset
	slice(to_filter) %>% 
	distinct(cou) %>% 
	unlist() 

GCI_df2 <- GCI_df %>% 
	select(cou, year, all_of(GCI_na_2)) %>% # selecting only full dataset
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
	left_join(tymek_df, by = c("cou", "year")) %>% 
	filter(!cou %in% c("ARG","TWN","ZWE", "VEN", "AZE", "GEO", "MKD", "MNE", "OMN", "TCD", "MLT","BOL", "CYP")) %>% 
	select(year, cou, everything()) %>% 
	arrange(cou, year) #%>% 
	# select(cou, year, pn_wbinf, pn_freedom) %>%
	# filter(if_any(.cols = everything(),
	# 				  .fn = is.na)) #%>%
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

data_df_2 <- data_df %>% 
	mutate(year = as.character(year)) %>% 
	pivot_longer(where(is.numeric)) %>% 
	group_by(cou, name) %>% 
	tidyr::fill(value, .direction = "downup") %>% 
	pivot_wider()

# write_xlsx(data_df_2, "database_2024_01_06.xlsx")

# write_xlsx(dict_final, "dict_master.xlsx")
# write_xlsx(countries_final, "countries_final.xlsx")

# check_GCI <- data_df %>% 
# 	# filter(cou %in% countries_final$cou) %>% 
# 	select(cou, year, where(~any(is.na(.x)))) %>%
# 	# select(-wf_EOSQ168) %>% 
# 	group_by(cou) #%>% 
# 	naniar::miss_var_summary()

