detect_jump <- function(
		vector,
		jump_years = 3,
		check_years = 3,
		min_jump = 0.5){
	len_vector = length(vector)
	vector_output <- rep(0,len_vector)
	for (ii in 1:len_vector){
		# Count years which are necessary for letter actions
		if(len_vector -(ii+jump_years-1) <  check_years){
			# print(ii)
			break
		}
		# Jump period detecting
		
		jump_period <- vector[ii:(ii+jump_years)]
		# Calculate a jump a check threshold (min_jump)
		jump <- jump_period[jump_years+1]-jump_period[1]
		if(jump < min_jump){
			# print(ii)
			next
		}
		len_check <- len_vector
		# Check if in year in control period value level was lower that in the moment of jump
		if((ii+jump_years+check_years) < len_vector){
			len_check <- (ii+jump_years+check_years)
		}
		if(any(vector[(ii+jump_years):len_check] - jump_period[jump_years+1] < 0)){
			# print(ii)
			next
		}
		earlier <- 0
		if(jump_years != 1){
			for(jj in 1:(length(jump_period)-2)){
				jump <- jump_period[jump_years + 1-jj]-jump_period[1]
				if(jump < min_jump){
					# print(ii)
					next
				}
				if(any(vector[(ii+jump_years-jj):len_vector] - jump_period[jump_years-jj] < 0)){
					# print(ii)
					next
				}
				earlier <- jj
			}
		}
		
		# print((ii+jump_years):len_vector)
		# print((ii+jump_years - earlier):len_vector)
		vector_output[(ii+jump_years - earlier):len_vector] <- 1
		if(sum(vector_output) >0){
			break
		}
	}	
	return(vector_output)
}
vector <- c(1,3,3,5,3,1,1,4,4,4,4)
detect_jump(vector = vector,
				jump_years = 1,
				check_years = 3)

jumps_summary <- function(data = data_jumps){
	data %>% 
		summarize(across(
			.cols = starts_with("wf"),
			.fns = max
		)) %>% 
		ungroup() %>% 
		summarize(across(
			.cols = starts_with("wf"),
			.fns = sum
		)) %>% 
		pivot_longer(
			starts_with("wf")
		) %>% 
		arrange(desc(value)) %>% 
		left_join(dict_GCI, by = c("name" = "variables")) %>% 
		select(name, value, series)	
}

treated_columns <- function(column){
	column <- enquo(column)	
	filter_jumps <- data_jumps %>% 
		select(year, cou, !!column) %>%
		pivot_wider(names_from = cou,
						values_from = !!column) %>% 
		select(where(~sum(.x) > 0)) %>% 
		colnames()
}
visual_jumps <- function(column){
	column <- enquo(column)	
	filter_jumps <- treated_columns(!!column)
	data_group %>% 
		select(year, cou, !!column) %>% #select_data
		filter(cou %in% filter_jumps) %>% 
		mutate(year = as.Date(paste0(year,"-01-01"))) %>% 
		ggplot(aes(year, !!column)) +
		geom_line(aes(color = cou)) +
		theme_minimal()
}
visual_panel <- function(column){
	column <- enquo(column)	
	column_str <- quo_name(column)
	panelview(as.formula(paste0("1 ~ ", column_str)),
				 data = data_jumps %>% 
				 	select(year, cou, !!column) %>% 
				 	filter(cou %in% filter_jumps),
				 index = c("cou","year"),
				 legendOff = TRUE,
				 gridOff = TRUE,
				 by.timing = TRUE)
}
my_matching <- function(iterations = 1000,
								outcome = "y_ham",
								size_match = 10,
								df = data_model
								){
	output <- list()
	for (ii in c("mahalanobis", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")){
		PM.results <- PanelMatch(
			lag = 2, 
			time.id = "year", 
			unit.id = "cou", 
			treatment = "treatment", 
			refinement.method = ii,
			data = df, 
			match.missing = TRUE, 
			covs.formula = ~ 
				# I(lag(pn_csh_x, 1:3)) + #Share of merchandise exports at current PPPs
				I(lag(pn_csh_i, 1:2)) + #Share of gross capital formation at current PPPs
				I(lag(y, 1:2)) + 
				I(lag(pn_csh_g, 1:2)) + #Share of government consumption at current PPPs
				I(lag(pn_hc, 1:2)) #Human capital index, based on years of schooling and returns to education
			, 
			size.match = size_match,
			qoi = "att" ,
			outcome.var = outcome,
			lead = 0:2, 
			forbid.treatment.reversal = FALSE,
			use.diagonal.variance.matrix = TRUE
		)
		PM.estimates <- PanelEstimate(sets = PM.results, 
											 data = df,
											 confidence.level = 0.9,
											 number.iterations = iterations)
		output[[ii]] <- summary(PM.estimates)$summary
	}
	return(output)
}

my_matching_2 <- function(iterations = 1000,
								outcome = "y_ham",
								size_match = 10,
								df = data_model
){
	output <- list()
	for (ii in c("mahalanobis", "ps.match", "CBPS.match", "ps.weight", "CBPS.weight")){
		PM.results <- PanelMatch(
			lag = 2, 
			time.id = "year", 
			unit.id = "cou", 
			treatment = "treatment", 
			refinement.method = ii,
			data = df, 
			match.missing = TRUE, 
			covs.formula = ~ 
				# I(lag(pn_csh_x, 1:3)) + #Share of merchandise exports at current PPPs
				I(lag(pn_csh_i, 1:2)) + #Share of gross capital formation at current PPPs
				I(lag(y, 1:2)) + 
				I(lag(pn_csh_g, 1:2)) + #Share of government consumption at current PPPs
				I(lag(pn_hc, 1:2)) #Human capital index, based on years of schooling and returns to education
			, 
			size.match = size_match,
			qoi = "att" ,
			outcome.var = outcome,
			lead = 0:2, 
			forbid.treatment.reversal = FALSE,
			use.diagonal.variance.matrix = TRUE
		)
		PM.estimates <- PanelEstimate(sets = PM.results, 
												data = df,
												confidence.level = 0.9,
												number.iterations = iterations)
		output2 <- list()
		output2[[1]] <- summary(PE.estimates)$summary
		# output2[[2]] <- PM.results
		output[[ii]] <- output2
	}
	return(output)
}

data_to_model <- function(column, 
								  df = data){
	column <- enquo(column)	
	output <- df %>%
		select(year, cou, y, y_ham, y_ham_d, starts_with("pn")) %>% 
		left_join(data_jumps %>% 
					 	select(year, cou, !!column),
					 by = join_by(year, cou)) %>% 
		rename(treatment = !!column) %>% 
		mutate(treatment = ifelse(year == 2008, 0, treatment)) %>% 
		mutate(cou = as.integer(as.factor(cou))) %>% 
		mutate(year = as.integer(year)) %>% 
		select(-pn_pop, -pn_rgdpna) %>% 
		arrange(cou, year) %>% 
		as.data.frame()
	return(output)
}

jump_data_proces <- function(args_list = jumps_args){
	if(args_list$is.dynamic){
		data_jumps <- data_dynamics %>%
			mutate(across(
				.cols = starts_with("wf"),
				.fns = ~detect_jump(
					.x,
					jump_years = args_list$jump_years,
					check_years = args_list$check_years,
					min_jump = args_list$min_jump)))
	}else{
		data_jumps <- data_group %>%
			mutate(across(
				.cols = starts_with("wf"),
				.fns = ~detect_jump(
					.x,
					jump_years = args_list$jump_years,
					check_years = args_list$check_years,
					min_jump = args_list$min_jump)))
	}
	return(data_jumps)
}

jump_data_proces_one <- function(args_list,
											column){
	column <- enquo(column)
	if(args_list$is.dynamic){
		data_jumps <- data_dynamics %>%
			select(!!column, !matches("wf_")) %>% 
			mutate(across(
				.cols = !!column,
				.fns = ~detect_jump(
					.x,
					jump_years = args_list$jump_years,
					check_years = args_list$check_years,
					min_jump = args_list$min_jump),
				.names = glue::glue_collapse(as.numeric(args_list),"_"))) %>% 
			select(-!!column)
	}else{
		data_jumps <- data_group %>%
			select(!!column, !matches("wf_")) %>% 
			mutate(across(
				.cols = !!column,
				.fns = ~detect_jump(
					.x,
					jump_years = args_list$jump_years,
					check_years = args_list$check_years,
					min_jump = args_list$min_jump),
				.names = glue::glue_collapse(as.numeric(args_list),"_"))) %>% 
			select(-!!column)
			
	}
	return(data_jumps)
}
data_to_model_one <- function(column,
								  treatment_df,
								  df = data){
	column <- enquo(column)	
	output <- df %>%
		select(year, cou, y, y_ham, y_ham_d, starts_with("pn")) %>% 
		left_join(treatment_df %>% 
					 	select(year, cou, !!column),
					 by = join_by(year, cou)) %>% 
		rename(treatment = !!column) %>% 
		mutate(treatment = ifelse(year == 2008, 0, treatment)) %>% 
		mutate(cou = as.integer(as.factor(cou))) %>% 
		mutate(year = as.integer(year)) %>% 
		select(-pn_pop, -pn_rgdpna) %>% 
		arrange(cou, year) %>% 
		as.data.frame()
	return(output)
}
