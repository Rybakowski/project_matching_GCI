library(Matching); library(tidyverse)
br <- causaldata::black_politicians


# Outcome
Y <- br %>%
	pull(responded)
# Treatment
D <- br %>%
	pull(leg_black)
# Matching variables
# Note select() is also in the Matching package, so we specify dplyr
X <- br %>%
	dplyr::select(medianhhincom, blackpercent, leg_democrat) %>%
	as.matrix()

# Weight = 2, oddly, denotes Mahalanobis distance
M <- Match(Y, D, X, Weight = 2, caliper = 1)

# See treatment effect estimate
summary(M)

# Get matched data for use elsewhere. Note that this approach will 
# duplicate each observation for each time it was matched
matched_treated <- tibble(id = M$index.treated,
								  weight = M$weights)
matched_control <- tibble(id = M$index.control,
								  weight = M$weights)
matched_sets <- bind_rows(matched_treated,
								  matched_control) 
# Simplify to one row per observation
matched_sets <- matched_sets %>%
	group_by(id) %>%
	summarize(weight = sum(weight))
# And bring back to data
matched_br <- br %>%
	mutate(id = row_number()) %>%
	left_join(matched_sets, by = 'id')

# To be used like this! The standard errors here are wrong
model <- lm(responded~leg_black, data = matched_br, weights = weight)
summary(model)


library(ebal); library(tidyverse); library(modelsummary)
br <- causaldata::black_politicians

# Outcome
Y <- br %>%
    pull(responded)
# Treatment
D <- br %>%
    pull(leg_black)
# Matching variables
X <- br %>%
    select(medianhhincom, blackpercent, leg_democrat) %>%
    # Add square terms to match variances if we like
    mutate(incsq = medianhhincom^2,
    bpsq = blackpercent^2) %>%
    as.matrix()

eb <- ebalance(D, X)

# Get weights for usage elsewhere
# Noting that this contains only control weights
br_treat <- br %>%
    filter(leg_black == 1) %>%
    mutate(weights = 1)
br_con <- br %>%
    filter(leg_black == 0) %>%
    mutate(weights = eb$w)
br <- bind_rows(br_treat, br_con)

m <- lm(responded ~ leg_black, data = br, weights = weights)
msummary(m, stars = c('*' = .1, '**' = .05, '***' = .01))
