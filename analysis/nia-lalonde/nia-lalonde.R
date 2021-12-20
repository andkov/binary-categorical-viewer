rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")
source("./scripts/graphing/graph-presets.R")


# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(readxl)    # for import
library(explore)   # for describe_all()
library(scales)    # formatting
library(rlang)     # tidy evals - see https://edwinth.github.io/blog/dplyr-recipes/ and
library(labelled)  # labels - see https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
library(gtsummary) # tables for regression
library(gt)        # tables
library(pROC)      # ROC curve calculation, `ggroc()`

# check out these packages for model tables
# sjPlot
# jtools

# ---- declare-functions -------------------------------------------------------
# local to this script


# ---- declare-globals ---------------------------------------------------------
# Establish the folder for printing plots
prints_folder <- paste0("./analysis/nia-lalonde/prints-1/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

# Establish the folder for data cache
data_cache_folder <- paste0("./data-private/derived/nia-lalonde/")
if(!file.exists(data_cache_folder)){dir.create(file.path(data_cache_folder))}

# ---- load-data ---------------------------------------------------------------
library(twang)
data(lalonde)
ds0 <- lalonde%>% as_tibble(); rm(lalonde)

# ---- inspect-data ------------------------------------------------------------

ds0 %>% explore::describe_all()

# ---- tweak-data-1 --------------------------------------------------------------

ds1 <-
  ds0 %>%
  mutate(
    educ_cat = case_when(
      educ < 12 ~ "less than HS"
      ,educ == 12 ~ "HS"
      ,educ > 12 ~ "more than HS"
    )
    ,age_cat = case_when(
       age > 15 & age <= 25 ~ "16-25"
      ,age > 25 & age <= 35 ~ "26-35"
      ,age > 35 & age <= 45 ~ "36-45"
      ,age > 45 & age <= 55 ~ "46-55"
    )
    ,re78_cat = case_when(
      re78 >= 10000 ~ "high"
      ,re78 < 10000 ~ "low"
    )
  ) %>%
  mutate_at(
    .vars = c("educ_cat","age_cat", "re78_cat")
    ,.funs = as_factor
  )
ds1 %>% group_by(educ, educ_cat) %>% count() %>% print(n=nrow(.))
ds1 %>% group_by(age, age_cat) %>% count() %>% print(n=nrow(.))

# ---- explore-data ------------------------------------------------------------
glimpse(ds1)
look_for(ds1)
explore::describe_all(ds1)

# Histogram examining conversion to categories
ds1 %>% ggplot(aes(x=age,fill=age_cat))+geom_histogram(binwidth = 1)
ds1 %>% ggplot(aes(x=educ,fill=educ_cat))+geom_histogram(binwidth = 1)
ds1 %>% ggplot(aes(x=re78,fill=re78_cat))+geom_histogram(binwidth = 500)


# ----- game-plan --------------------------------------------------------------
# Steps in the PSA analysis described in [twang() vignette](https://cran.r-project.org/web/packages/twang/vignettes/twang.pdf)
# 1. Determine the estimand (ATT)
# 2. Determine the observed confounding factors to be balanced (covariate list)
# 3. Fit the propensity score model
# 3(a) Evaluate the convergence of the algorithm
# 3(b) Assess the balance of confounding factors before and after applying the propensity score weights
# 3(c) Rerun if needed tweaking algorithm features
# 4. Estimate the net effect

# Estimands

# ATT - Average Treatment Effect on the Treated
# a = what actually happened to the TREATED group (i.e. mean or median)
# b = what would have happened if TREATED group did NOT receive Tx (i.e. mean or median)
# ATT = a - b

# ATE - Average Treatment Effect (on everyone) =
# a =  what would have happend had EVERYONE received Tx
# b =  what would have happened had NOONE received Tx
# ATE = a - b


# ---- estimate-ps -------------------------------------------------------------
dependent <- "treat"
explanatory <- c("age","educ","black","hispan","nodegree","married","re74","re75")
eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) )
eq_formula

# comment out BELOW on reproduction
# d_source <- ds1 %>% as.data.frame() # to emphasize the source of data
# ps1 <-
#   ps(
#     # propensity score model
#     formula            = eq_formula
#     ,estimand          = "ATT" # use ATT because that is consistent with ex-post net impact
#     ,data              = d_source
#     ,sampw             = d_source$survey_weight # !!! optional
#     # gradient boosting
#     ,stop.method       = c("es.mean", "es.max", "ks.mean", "ks.max")
#     ,n.trees           = 5000
#     ,interaction.depth = 2
#     ,shrinkage         = 0.01
#     ,n.minobsinnode    = 10
#     # computational efficiency
#     ,n.keep            = 1
#     ,n.grid            = 25
#     ,ks.exact          = NULL
#     ,version           = "gmb" # gmb, xboost, legacy
#     # other
#     ,verbose           = FALSE
#   )
# rm(d_source)
# ps1 %>%  readr::write_rds(paste0(data_cache_folder,"ps1.rds")) # first time
# comment out ABOVE on reproduction

ps1 <-    readr::read_rds(paste0(data_cache_folder,"ps1.rds")) # on reproduction

# ----- explore-ps -------------------------------------------------------------
summary(ps1)

# ------ estimate-effect -------------------------------------------------------
compute_effect <- function(
  ps_object # object from ps() model
  ,outcome_name = "re78_cat"
  ,treatment_name = "treat"
  ,outcome_dist_family = "gaussian" # gaussian for continuous, binomial for discreate outcomes
){
  ps_object <- ps1
  treatment_name = "treat"

  outcome_name = "re78"
  outcome_dist_family = "gaussian"
  # outcome_name = "re78_cat"
  # outcome_dist_family = "binomial"

  d <-
    ps_object$data %>%
    mutate(
      w =  get.weights(ps_object,stop.method = "es.mean", estimand = "ATT" )
    ) %>%
    as_tibble()
  design.ps <- survey::svydesign(ids= ~1, weights = ~w, data = d)
  # d %>% group_by(!!rlang::sym(treatment_name)) %>% count()
  # d %>% group_by(!!!rlang::syms(treatment_name)) %>% count() # for many

  outcome_n <- d %>%
    group_by(!!rlang::sym(treatment_name)) %>%
    summarize(
      outcome_n = sum(!is.na(!!rlang::sym(outcome_name)))
    )

  ( model_equation <- as.formula(paste0(outcome_name, "~", treatment_name)) )
  model <-
    survey::svyglm(model_equation, design = design.ps, family = outcome_dist_family) %>%
    suppressWarnings()
  # summary(model)
  pattern_starts_with_explanatory <- paste0("^",treatment_name, collapse = "|")
  d_estimates  <-
    model %>%
    broom::tidy(
      conf.int = TRUE
      # ,exp     = TRUE # converts log-odds into odds-ratios (i.e. =exp(estimate))
    ) %>%
    mutate(
      # conv_odds    = (estimate-1) # careful, this relies on broom::tidy(exp=TRUE)
      # otherwise: exp(estimate) - 1
       var_name    = stringr::str_extract(term, pattern_starts_with_explanatory)
      ,value_level = stringr::str_remove( term, pattern_starts_with_explanatory)
    )
  d_estimates

  # applies propensity weights
  d_net <-
    d %>%
    group_by(treat) %>%
    summarize(
      unweighted = mean(treat, na.rm=T)
      ,weighted = sum(treat*w, na.rm =T) / sum(w, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      unw_diff = unweighted - lag(unweighted)
      ,wgt_diff = weighted - lag(weighted)
    )

  d_result <-
    dplyr::left_join(
      d_net %>%
        select(
          !!rlang::sym(treatment_name)
          ,mean_raw      = unweighted
          ,mean_weighted = weighted
          ,net_impact    = wgt_diff
        )
      ,
      d_estimates %>%
        filter(term==paste0(treatment_name,"TRUE")) %>%
        # mutate(tx=TRUE) %>%
        mutate(!!rlang::sym(treatment_name):=TRUE) %>%
        select(!!rlang::sym(treatment_name),conv_odds,p.value)
      ,by = treatment_name
    )

  d_result <-
    d_result %>%
    left_join(outcome_n,by = "tx")%>%
    mutate(
      outcome = outcome_name
    ) %>%
    select(outcome,outcome_n,tx, everything())
  return(d_result)
}
# How to use
# ps1 %>% compute_effect("emp_at_survey")
# ps1 %>% compute_effect("employed_postprogram")
# ps1 %>% compute_effect("social_assistance")

# ---- publish -----------------------------------------------------------------
path <- "./analysis/nia-lalonde/nia-lalonde.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
