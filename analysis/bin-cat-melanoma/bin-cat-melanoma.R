rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")
source("./scripts/graphing/graph-presets.R")
source("./analysis/bin-cat-melanoma/binary-categorical-functions.R")

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
library(cowplot)   # combine graphs
library(pROC)      # ROC curve calculation, `ggroc()`
library(broom)     # for cleaning models
requireNamespace("broom.mixed")  # for cleaning models
requireNamespace("ggstance")  # for plotting effects
requireNamespace("sjPlot")    # plotting effects
requireNamespace("jtools")    # plotting effects
requireNamespace("DescTools") # extracting model fit indices from contingency mode
requireNamespace("rcompanion") # extracting model fit indices from contingency mode
requireNamespace("finalfit") # meta-wrapper from https://moderndive.com/, good for a quickie
requireNamespace("webshot2") # tables

# check out these packages for model tables ( but these are a bit fussy)
# sjPlot
# jtools


# ---- declare-functions -------------------------------------------------------
# local to this script


# ---- declare-globals ---------------------------------------------------------

# default colors to represent effect direction and significance
pal_direction_significance <-  c(
  "Increase (99%)"   = "#2b8cbe"
  ,"Increase (95%)"  = "#7bccc4"
  ,"Increase (90%)"  = "#bae4bc"
  ,"Not Significant" = "NA"
  ,"Decrease (90%)"  = "#fdcc8a"
  ,"Decrease (95%)"  = "#fc8d59"
  ,"Decrease (99%)"  = "#d7301f"
) # some graphing functions expect this definition !!

# Direction of Effect (Significance Level at %)
# To help with meaningful colors of effect interpretation

# Establish the folder for printing plots
prints_folder <- paste0("./analysis/bin-cat-melanoma/prints/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

# Establish the folder for data cache
data_cache_folder <- paste0("./data-private/derived/bin-cat-melanoma/")
if(!file.exists(data_cache_folder)){dir.create(file.path(data_cache_folder))}

path <- "data-private/derived/ellis-melanoma.rds" # data source, see ellis in ./manipulation
# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_rds(path) # run ./manipulation/ellis-melanoma.R to procure the data
# while this data set if famous, I've groomed it to anticipate certain applications

# ---- inspect-data ------------------------------------------------------------

ds0 %>% names_labels() %>% select(name)
look_for(ds0)
# ---- column-labels -----------------------------------------------------------
OuhscMunge::column_rename_headstart(ds0 ) # to help getting started
# white list of project scope (included as needed)
variables_labels_focus <- c(
  "time"           = "time",               # until death
  # "status"         = "status",           # dead or alive?
  # "sex"            = "sex",
  "age"            = "age",
  "year"           = "year",
  # "thickness"      = "thickness",
  # "ulcer"          = "ulcer",
  "sex_factor"     = "sex_factor",         # more convenient levels
  "ulcer_factor"   = "ulcer_factor",       # more convenient levels
  "status_factor"  = "status_factor",      # more convenient levels
  "t_stage_factor" = "t_stage_factor",     # more convenient levels
  "mort_5yr"       = "mort_5yr"            # more convenient levels
)
variables_in_focus <- names(variables_labels_focus)

# ---- tweak-data-1 --------------------------------------------------------------
glimpse(ds0)
look_for(ds0)
explore::describe_all(ds0)

ds1 <- ds0 %>%
  select(variables_in_focus) %>%
  rename(
    "sex" = "sex_factor"
    ,"ulcer"= "ulcer_factor"
    ,"status" = "status_factor"
    ,"t_stage" = "t_stage_factor"
  ) %>%
  mutate(
    person_oid = row_number()
  )
rm(ds0)
# ---- tweak-data-2 --------------------------------------------------------------
glimpse(ds1)
look_for(ds1)
explore::describe_all(ds1)


# ----- function-review-contingency ----------------------
ds1 %>% explore::describe_all() %>% arrange(unique)
# contingency table for any any two categorical variables
ds1 %>% make_bi_freq_table(var1 = "sex", var2 =  "status")
ds1 %>% make_bi_freq_graph("sex", "status") # wraps around `make_bi_freq_table()`
# we can change the order to get a different perspective
ds1 %>% make_bi_freq_graph("status", "sex")
ds1 %>% make_bi_freq_graph("status", "t_stage")
# formal tests of the contingency table
ds1 %>% run_contingency("sex", "status")

# ----- function-review-modeling ----------------------

# These custom functions are designed to carry out a model comparison in which
# FULL    model (FM): binary_criterion ~ Cat1 + Cat2 + ... + CatN
# REDUCED model (RM): binary_criterion ~        Cat2 + ... + CatN
# Where `CatN` - nth categorical confounder in this system
# This way, the relative improvement of fit from FM to RM can be interpreted
# as the capacity of Cat1 to explain variability in `binary_outcome` after
# adjusting for `Cat2 + ... + CatN` counfounders

# Let's demonstrate the machinery of the engine functions
# First, let's designate what variables will play what role in our GLM system:
dependent <- "mort_5yr"
explanatory   <- c("ulcer", "age", "sex", "t_stage") # by default, it will drop the first element
explanatory_r <-c(          "age", "sex", "t_stage") # to create the "reduced" model

# finalfit::finalfit(ds1,dependent, explanatory) # see https://moderndive.com/
ls_model   <- run_logistic_binary(ds1,dependent, explanatory) # relies on `get_model_fit`
ls_model_r <- run_logistic_binary(ds1,dependent, explanatory_r)
# it returns a complex list, one of the element is the product of `stats::glm()`
(model <- ls_model$model)
(model_reduced <- ls_model_r$model)
model         %>% get_rsquared() # custom wrapper for quick model comparison
model_reduced %>% get_rsquared() # custom wrapper for quick model comparison
model         %>% get_model_fit() # custom wrapper to get out key indices
model %>% summary() # built-in summary, sometimes useful

# But broom package gets a better head start
model %>% broom::tidy(exponentiate =T) # predictor level
model %>% broom::glance() # model level

# jtools is another neat packages, but it sometimes conflicts with Rmd reports
model %>% jtools::summ() # formatted summaries (but watch out in Rmd applications!)
g <-
  model %>%
  sjPlot::plot_model() +
  labs(title = "Odds Ratios" )+
  # scale_y_log10( limits = c(.1,3)))%>%
  scale_y_log10()
g %>% quick_save("full-model-estimates",width=6, height=4)
# Interpretation:
# The presence of ulcer multiplies the odds of the outcome (5 year mortality) by 3.21
# In other words, patients with ulcers increase their 5-year mortality rate by 221%
# relative to patients without ulcers, adjusting for age, sex, and cancer stage
model %>% jtools::plot_summs()
model %>% jtools::plot_coefs()
model %>% gtsummary::tbl_regression(exp=T) %>% print()

# To demonstrate model comparison
# jtools::summ(model)
# jtools::plot_summs(model)
# jtools::plot_summs(model,model_reduced, scale=TRUE, plot.distributions=T)

ls_model_comp <-
  ds1 %>%
  run_logistic_binary_model_comparison(
    dependent = dependent
    ,explanatory = explanatory
  )
# there might be warning messages about `sign_direction`, it's a benign error
# now let's study the components of the produced object
# components
# the identical structure  for `ls_model$reduced`
ls_model_comp$full # all from Reduced + 1 focal (exploratory[1])
ls_model_comp$full$equation
ls_model_comp$full$model
ls_model_comp$full$model %>% summary()
ls_model_comp$full$model %>% broom::tidy(exp=T)
ls_model_comp$full$predicted # for all unique combos on explanatory vars
ls_model_comp$full$roc
ls_model_comp$full$estimates
ls_model_comp$full$estimates %>% glimpse()
ls_model_comp$full$model_fit
ls_model_comp$full$focal_factor
ls_model_comp$full$model_fit
# Now the element that compares models
ls_model_comp$compare$test
ls_model_comp$compare$table


# ----- function-review-graphing ----------------------


ls_model_comp %>%
  make_odds_ratios_graph()
# Interpretation:
# The presence of ulcer multiplies the odds of the outcome (5 year mortality) by 3.21
# OR
# After adjusting for age, sex, and stage of cancer, patients with ulcers are
# 3.21 times MORE likely to die withing 5 years of diagnosis.
# OR
# Patients with ulcers increase their 5-year mortality rate by 221% (not 321%)
# relative to patients without ulcers, adjusting for age, sex, and cancer stage

# Now let's make the individual graphs that will comprise the complex display
ls_bi_graph <-
  ds1 %>%
  make_bi_contingency_graph_components("status","ulcer")
ls_bi_graph$stack # bar graph stacked
ls_bi_graph$fill # bar graph filled
ls_bi_graph$measures # of model misfit

# we will use another wrapper to combine graphs into a single display
g <-
  ds1 %>%
  make_bi_contingency_graph("status","ulcer")
g

# ----- function-review-information-display ----------------------
# let's create a "data transfer object" that will capture necessary components
dto <-
  get_bi_test_objects(
     ds1
    ,dependent   = dependent
    ,explanatory = explanatory # first component used for comparison by default
    ,clear_cache = FALSE
    ,get_cache = FALSE # FALSE first time, TRUE afterwards
  )
# there is a lot packed into it, explore it on  your own

# now the top-level wrapper that assembles components into a specific information display
g <- dto %>% make_bi_test_output_A(ds1)
g <- dto %>% make_bi_test_output_B() # saves model comparison table as a graph



### NOTE ###
# Chunks below give examples of serializing these functions in a dynamic report
# They do NOT mean to be self-explanatory, so don't worry too much about them for now

# ---- testing-models -------
# source("./analysis/binary-categorical-1/binary-categorical-1-functions.R")
# dto <- ds2 %>%
#   get_bi_test_objects(
#     dependent    = "has_ea"
#     ,explanatory = c("age_group","sex")
# ,clear_cache = FALSE # delete if exists
# ,get_cache = TRUE # TRUE reads the cached object, FALSE computes it
# )
# dto %>% make_bi_test_output_A()
# dto %>% make_bi_test_output_B()
# table_plot_path <- paste0(prints_folder,dto$model_name,".png" )
# tt %>%    # build gtsummary table
#   as_gt() %>%             # convert to gt table
#   gt::gtsave(             # save table as image
#     filename = table_plot_path
# )



# ----- compute-models --------------------
# Generate all graphs to be inserted during reporting
# dependent <- "has_ea"
# explanatory   <- c("employment_state","age_group", "sex","marital_status","hh_role")
# for(i in seq_along(explanatory)){
#   # i <- 1
#   explanatory_i <- c(explanatory[i],explanatory[-i])
#
#   dto <- ds1 %>%
#     get_bi_test_objects(
#       dependent    = dependent
#       ,explanatory = explanatory_i
#       ,clear_cache = TRUE # delete if exists
#       ,get_cache = FALSE # TRUE reads the cached object, FALSE computes it
#     )
#
# }
# ---- print-results ------------------

# dependent <- "has_ea"
# explanatory   <- c("employment_state","age_group", "sex","marital_status","hh_role")
# for(i in seq_along(explanatory)){
# # for(i in 1 ){
#   # i <- 2
#   explanatory_i <- c(explanatory[i],explanatory[-i])
#
#   dto <- ds2 %>%
#     get_bi_test_objects(
#       dependent    = dependent
#       ,explanatory = explanatory_i
#       ,clear_cache = FALSE # delete if exists
#       ,get_cache = FALSE # TRUE reads the cached object, FALSE computes it
#     )
#   dto %>% make_bi_test_output_A(d=ds2)
#   # dto %>% make_bi_test_output_B()
#   rm(dto)
# }

# ---- paste-results ------------------

# dependent <- "has_ea"
# explanatory   <- c("employment_state","age_group", "sex","marital_status","hh_role")
# for(i in seq_along(explanatory)){
#   # i <- 1
#   explanatory_i <- c(explanatory[i],explanatory[-i])
#   model_name <- paste0(c(dependent,explanatory_i),collapse = "-")
#   display_a <- paste0(prints_folder,model_name,".jpg")
#   display_b <- paste0(prints_folder,model_name,".png")
#  cat("\n##",i,"\n")
#   cat("\n")
#  display_a %>% jpeg::readJPEG() %>% grid::grid.raster()
#  cat("\n")
#  # display_b %>% png::readPNG() %>% grid::grid.raster()
#  # display_b %>% jpeg::readJPEG() %>% grid::grid.raster()
#  cat("\n")
#  }

# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/bin-cat-melanoma/bin-cat-melanoma.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
