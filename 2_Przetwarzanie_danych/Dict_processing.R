
library(tidyverse)

dict_GCI <- readxl::read_xlsx(r"(C:\Users\rados\OneDrive - SGH\project_matching_GCI\dict_master.xlsx)",
										sheet = "GCI") %>% 
	mutate(variables = paste0("wf_", variables))


dict_GCI_aggs <- dict_GCI %>% 
	filter(str_detect(variables, "GCI"))
level_1 <- dict_GCI_aggs %>% 
	filter(is.na(b))
level_2 <- dict_GCI_aggs %>% 
	filter(is.na(c)) %>% 
	anti_join(level_1) 
level_3 <- dict_GCI_aggs %>% 
	filter(is.na(d)) %>%
	anti_join(level_2) %>% 
	anti_join(level_1)
level_4 <- dict_GCI_aggs %>% 
	filter(!is.na(d))


dict_final <- level_1 %>%
	select(a,variables, series) %>% 
	rename(level_1 = variables,
			 name_1 = series) %>% 
	left_join(level_2 %>% 
					select(a,b,variables, series) %>% 
					rename(level_2 = variables,
							 name_2 = series))%>% 
	left_join(level_3 %>% 
				 	select(variables, series,a,b,c) %>% 
				 	rename(level_3 = variables,
				 			 name_3 = series),
				 	by = join_by(a,b)) %>% 
	left_join(level_4 %>% 
				 	select(variables, series,a,b,c,d) %>% 
				 	rename(level_4 = variables,
				 			 name_4 = series),
				 by = join_by(a,b,c)) %>% 
	select(a,b,c,d, contains("name"))

write_xlsx(dict_final,"dict_hierarchical.xlsx")
