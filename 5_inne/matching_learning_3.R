library(causalweight); library(tidyverse)
br <- causaldata::black_politicians

# We can estimate our own propensity score
m <- glm(leg_black ~ medianhhincom + blackpercent + leg_democrat,
			data = br, family = binomial(link = 'logit'))
# Get predicted values
br <- br %>%
	mutate(ps = predict(m, type = 'response'))
# "Trim" control observations outside of 
# treated propensity score range
# (we'll discuss this later in Common Support)
minps <- br %>%
	filter(leg_black == 1) %>%
	pull(ps) %>%
	min(na.rm = TRUE)
maxps <- br %>%
	filter(leg_black == 1) %>%
	pull(ps) %>%
	max(na.rm = TRUE)
br <- br %>%
	filter(ps >= minps & ps <= maxps)

# Create IPW weights
br <- br %>%
	mutate(ipw = case_when(
		leg_black == 1 ~ 1/ps,
		leg_black == 0 ~ 1/(1-ps)))

# And use to weight regressions (The standard errors will be wrong
# here unless we bootstrap the whole process - See the code examples 
# from the doubly robust estimation section or the simulation chapter)
lm(responded ~ leg_black, data = br, weights = ipw)

# Or we can use the causalweight package!
# First, pull out our variables
# Outcome
Y <- br %>%
	pull(responded)
# Treatment
D <- br %>%
	pull(leg_black)
# Matching variables
X <- br %>%
	select(medianhhincom, blackpercent, leg_democrat) %>%
	as.matrix()

# Note by default this produces average treatment effect,
# not average treatment on the treated, and trims propensity 
# scores based on extreme values rather than matching treated range
IPW <- treatweight(Y, D, X, trim = .001, logit = TRUE)

# Estimate and SE
IPW$effect
IPW$se