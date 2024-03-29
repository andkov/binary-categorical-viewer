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
source("./analysis/binary-categorical-1/binary-categorical-1-functions.R")

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
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)

# default colors to represent effect direction and significance
pal_sign_dir <-  c(
  "Increase (99%)"   = "#2b8cbe"
  ,"Increase (95%)"  = "#7bccc4"
  ,"Increase (90%)"  = "#bae4bc"
  ,"Not Significant" = "NA"
  ,"Decrease (90%)"  = "#fdcc8a"
  ,"Decrease (95%)"  = "#fc8d59"
  ,"Decrease (99%)"  = "#d7301f"
)
# Direction of Effect (Significance Level at %)
# To help with meaningful colors of effect interpretation

# Establish the folder for printing plots
prints_folder <- paste0("./analysis/bin-cat-melanoma/prints-1/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

# Establish the folder for data cache
data_cache_folder <- paste0("./data-unshared/derived/bin-cat-melanoma/")
if(!file.exists(data_cache_folder)){dir.create(file.path(data_cache_folder))}

path <- "data-unshared/derived/0-import-1.rds" # data source, see ellis in ./manipulation
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
  )
ds1 %>% group_by(educ, educ_cat) %>% count() %>% print(n=nrow(.))
ds1 %>% group_by(age, age_cat) %>% count() %>% print(n=nrow(.))

# ---- tweak-data-2 --------------------------------------------------------------
glimpse(ds1)
look_for(ds1)
explore::describe_all(ds1)

ds0 %>% ggplot(aes(x=age))+geom_histogram(binwidth = 1)

# ---- explore-data -----------------

GGally::ggduo(ds1)

# ---- single-model ----------------

dependent <- "re78"

explanatory   <- c("treat", "black","hispan","married", "educ_cat","age","re74", "re75")
explanatory_r <-c(          "black","hispan","married", "educ_cat","age","re74", "re75")
#
# # # finalfit::finalfit(ds2,dependent, explanatory)
ls_model <- run_logistic_binary(ds1,dependent, explanatory)
ls_model_r <- run_logistic_binary(ds1,dependent, explanatory_r)
model <- ls_model$model
model_reduced <- ls_model_r$model
model %>% summary()
model %>% get_rsquared();model_reduced %>% get_rsquared()
model %>% get_model_fit()
model %>% broom::tidy(exponentiate =T)
model %>% broom::glance()
# model %>% jtools::summ()
(model %>% sjPlot::plot_model() +
  labs(title = "Odds Ratios" )+
    # scale_y_log10( limits = c(.1,3)))%>%
    scale_y_log10( ))%>%
  quick_save("full-model-estimates",width=6, height=4)
model %>% jtools::plot_summs()
model %>% jtools::plot_coefs()
model %>% gtsummary::tbl_regression(exp=T) %>% print()

#
# jtools::summ(model)
# jtools::plot_summs(model)
# jtools::plot_summs(model,model_reduced, scale=TRUE, plot.distributions=T)
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
dependent <- "has_ea"
explanatory   <- c("employment_state","age_group", "sex","marital_status","hh_role")
for(i in seq_along(explanatory)){
  # i <- 1
  explanatory_i <- c(explanatory[i],explanatory[-i])

  dto <- ds2 %>%
    get_bi_test_objects(
      dependent    = dependent
      ,explanatory = explanatory_i
      ,clear_cache = TRUE # delete if exists
      ,get_cache = FALSE # TRUE reads the cached object, FALSE computes it
    )

}
# ---- print-results ------------------

dependent <- "has_ea"
explanatory   <- c("employment_state","age_group", "sex","marital_status","hh_role")
for(i in seq_along(explanatory)){
# for(i in 1 ){
  # i <- 2
  explanatory_i <- c(explanatory[i],explanatory[-i])

  dto <- ds2 %>%
    get_bi_test_objects(
      dependent    = dependent
      ,explanatory = explanatory_i
      ,clear_cache = FALSE # delete if exists
      ,get_cache = FALSE # TRUE reads the cached object, FALSE computes it
    )
  dto %>% make_bi_test_output_A(d=ds2)
  # dto %>% make_bi_test_output_B()
  rm(dto)
}

# ---- paste-results ------------------

dependent <- "has_ea"
explanatory   <- c("employment_state","age_group", "sex","marital_status","hh_role")
for(i in seq_along(explanatory)){
  # i <- 1
  explanatory_i <- c(explanatory[i],explanatory[-i])
  model_name <- paste0(c(dependent,explanatory_i),collapse = "-")
  display_a <- paste0(prints_folder,model_name,".jpg")
  display_b <- paste0(prints_folder,model_name,".png")
 cat("\n##",i,"\n")
  cat("\n")
 display_a %>% jpeg::readJPEG() %>% grid::grid.raster()
 cat("\n")
 # display_b %>% png::readPNG() %>% grid::grid.raster()
 # display_b %>% jpeg::readJPEG() %>% grid::grid.raster()
 cat("\n")
 }

# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
path <- "./analysis/binary-categorical-1/binary-categorical-1.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
