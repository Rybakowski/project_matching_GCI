temp <- c(1:19, NA)
names_temp <- seq(as.Date("2000/1/1"), as.Date("2019/1/1"), by = "year")

# temp_df <- tibble(v1 = list(temp, temp), list = list(names_temp, names_temp))


xd <- my_hamilton(names_temp, temp)
coredata(xd)[,2]



?
# temp_df %>% 
# 	mutate(output  = map2(list, v1, my_hamilton)) %>% 
# 	select(output) %>% 
# 	unnest()
