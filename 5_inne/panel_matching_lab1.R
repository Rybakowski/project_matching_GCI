library(PanelMatch)
library(tidyverse)
library(plm)
options(scipen = 10000000)

data <- structure(list(
	name = c("Brittany", "Ethan", "Kyle", "Jacob", "Jessica"), 
	trust.2006 = c(4, 5, 0, 5, 7), 
	trust.2007 = c(4, 6, 7, 3, 9), 
	trust.2008 = c(2, 4, 5, 6, 4), 
	threat.2006 = c(0,1, 0, 0, 1), 
	threat.2007 = c(0, 0, 1, 1, 0), 
	threat.2008 = c(1, 0, 0, 1, 0), 
	education.2006 = c(0, 3, 1, 2, 0), 
	education.2007 = c(0, 3, 1, 2, 0), 
	education.2008 = c(0, 3, 1, 2, 0)), 
	row.names = c(NA, 
					  -5L), class = c("tbl_df", "tbl", "data.frame"))

data <- data %>% pivot_longer(-name, names_to = "variable", values_to = "value") %>%
	separate(col = "variable", into = c("variable", "time"), sep = "\\.") %>% 
	pivot_wider(names_from = variable, values_from = value) 
data <- data %>% mutate(time = as.numeric(time), # Converting time variable
								unit = as.numeric(factor(name))) %>% # Add unit index
	dplyr::select(name, unit, time, everything()) # Reorder variables

fit.pooled <- plm(trust ~ threat, 
						data = data,
						index = c("name", "time"),
						model = "pooling") # pooled model
# Alternatively
fit <- lm(trust ~ threat, data = data)
summary(fit)


#Calculate first diffrences
data_fd <- data %>% dplyr::select(-education) %>%
	group_by(name) %>% 
	mutate(trust_fd = trust - dplyr::lag(trust)) %>% # Why do we need to specify the dplyr package here?
	mutate(threat_fd = threat - dplyr::lag(threat))

fit_fd <- plm(trust ~ threat, data = data,
				  index = c("name", "time"),
				  model = "fd", # first differences
				  effect = "individual") # Check ? plm for specification
summary(fit_fd)
fit_fe <- plm(trust ~ threat, data = data,
				  index = c("name", "time"),
				  model = "within",
				  effect = "individual")
summary(fit_fe)


data_fd_fil <- data_fd %>% mutate(treatment_status = paste(dplyr::lag(threat), "-", threat, sep=""))%>% 
	filter(!treatment_status == "1-0") %>% 
	filter(!treatment_status == "1-1")

fit <- lm(trust_fd ~ threat_fd, data = data_fd_fil)
summary(fit)
summary(fit_fd_2)
