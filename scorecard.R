#install.packages("Hmisc")

install.packages("scorecard")
library(scorecard)
library(tidyverse)


data("germancredit")

germancredit
dim(germancredit)
colnames(germancredit)


# filter variable via missing rate, iv, identical value rate
?var_filter()
dt_f = var_filter(germancredit, y="creditability", missing_limit = 0.95, iv_limit = 0.02,identical_limit = 0.95)

dim(dt_f)
colnames(dt_f)


setdiff(colnames(germancredit),colnames(dt_f))  # Premenne vyhodene funkciou var_filter

Hmisc::describe(    germancredit[ , setdiff(colnames(germancredit),colnames(dt_f))  ]       )
    # foreig.worker vyhodeny kvoli tomu, ze mal len dve hodnoty (yes/no), pricom yes tvorilo az 96,3 % --> neoresek cez identical_limit = 0.95
    # zvysne premenne nepresli kvoli IV limitu 

summary(germancredit[ , setdiff(colnames(germancredit),colnames(dt_f))  ])


# breaking dt into train and test
dt_list = split_df(dt_f, y = "creditability", ratio = 0.6, seed = 30)   # vrati list so zlozkami $train , $test
label_list = lapply(dt_list, function(x) x$creditability)               # list len s targetmi


# woe binning ------
bins = woebin(dt_f, y="creditability")
woebin_plot(bins)

# binning adjustment
# adjust breaks interactively
breaks_adj = woebin_adj(dt_f, "creditability", bins) 

## or specify breaks manually
breaks_adj = list(
  age.in.years=c(26, 35, 40),
  other.debtors.or.guarantors = c("none", "co-applicant%,%guarantor"))   # %,% znamena spajanie dvoch kategorii

bins_adj = woebin(dt_f, y = "creditability", breaks_list = breaks_adj)

# converting train and test into woe values
dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins_adj))

# Logisticka regresia 
# glm ------
m1 = glm(creditability ~ ., family = binomial(), data = dt_woe_list$train)
summary(m1)

# Variance inflation factor  
vif(m1, merge_coef = TRUE)   # Rule of thumb: Ak su gvif vyssie ako 10, tak je vysoka kolinearita. Niekedy sa pouziva aj >5 ako treshold.

# https://en.wikipedia.org/wiki/Variance_inflation_factor
?vif

# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step = step(m1, direction="both", trace = FALSE)  # direction: backward and forward stepwise algorithm
m2 = eval(m_step$call)
# vif(m2, merge_coef = TRUE) # summary(m2)

# # Adjusting for oversampling (support.sas.com/kb/22/601.html)
# library(data.table)
# p1=0.03 # bad probability in population 
# r1=0.3 # bad probability in sample dataset
# dt_woe = copy(dt_woe_list$train)[, weight := ifelse(creditability==1, p1/r1, (1-p1)/(1-r1) )][]
# fmla = as.formula(paste("creditability ~", paste(names(coef(m2))[-1], collapse="+")))
# m3 = glm(fmla, family = binomial(), data = dt_woe, weights = weight)


# performance ks & roc ------
## predicted proability
pred_list = lapply(dt_woe_list, function(x) predict(m2, x, type='response'))

## performance
perf = perf_eva(pred = pred_list, label = label_list)

# score ------
## scorecard
card = scorecard(bins_adj, m2)
## credit score
score_list = lapply(dt_list, function(x) scorecard_ply(x, card))

## psi - population stability index
perf_psi(score = score_list, label = label_list)$pic

# Rule of thumb:
#http://ucanalytics.com/blogs/population-stability-index-psi-banking-case-study/
  
?perf_psi
