# -------------------------------------------------------------------------
# Crash course in Causality
# Data Analysis Project 1
# -------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(tableone)
library(Matching)
library(MatchIt)

# Load Lalonde (1986) data
data("lalonde")
lalonde <- as_tibble(lalonde)

# Add indicator variables black & hispanic
lalonde <- lalonde %>% 
  mutate(
    black = (race == "black"),
    hispan = (race == "hispan"),
  )


# Analysis ----------------------------------------------------------------
all_vars <- colnames(lalonde)
conf_vars <- all_vars[c(2:3, 5:8, 10:11)]

## Ex. 1
# Find standardized differences
t1 <- CreateTableOne(
  vars = c(conf_vars, "re78"),
  strata = "treat",
  data = lalonde
)
print(t1, smd = TRUE)

## Ex. 2
# Difference in earnings treated vs untreated in 1978
6349.14 - 6984.17


## Ex. 3
# Estimate propensity scores using logistic regression
psmodel <- glm(
  formula(paste0("treat~", paste(conf_vars, collapse = "+"))),
  family = binomial(),
  data = lalonde
)

pscores <- psmodel$fitted.values

min(pscores)
max(pscores)

## Ex. 4
# Propensity score matching
set.seed(931139)

logit <- function(x) log(x/(1-x))

psmatch <- Match(
  Tr = lalonde$treat,
  X = pscores,
  M = 1, # pair matching (1:1)  
  replace = FALSE
)

lalonde_match <- lalonde[c(psmatch$index.control, psmatch$index.treated), ]

t2 <- CreateTableOne(
  vars = c(conf_vars, "re78"),
  strata = "treat",
  data = lalonde_match
)
print(t2, smd = TRUE)

## Ex. 5
print(t2, smd = TRUE)

## Ex. 6
# Propensity score matching with caliper
set.seed(931139)

psmatch2 <- Match(
  Tr = lalonde$treat,
  X = pscores,
  M = 1, # pair matching (1:1)  
  caliper = 0.1,
  replace = FALSE
)

lalonde_match2 <- lalonde[c(psmatch2$index.control, psmatch2$index.treated), ]

t3 <- CreateTableOne(
  vars = c(conf_vars, "re78"),
  strata = "treat",
  data = lalonde_match2
)
print(t3, smd = TRUE)

## Ex. 7
# Matched analysis - point estimate
lalonde_match2 %>% 
  group_by(treat) %>% 
  summarise(mean(re78))

# Difference in means
6151 - 4904

## Ex. 8
# Matched analysis - hypothesis testing
t.test(re78 ~ treat, data = lalonde_match2, paired = TRUE)

























