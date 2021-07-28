rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
cat("/014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default, unless overwritten

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")# functions sourced throughout the project

# ---- load-packages -----------------------------------------------------------
# disable tho
# core packages - turn ON/OFF to help overview the scope of the script
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(readxl)    # data import
library(explore)   # for `describe_all()`
library(scales)    # formatting
library(labelled)  # labels - https://cran.r-project.org/web/packages/labelled/vignettes/intro_labelled.html
library(rlang)     # tidy evaluations -  https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
library(finalfit)  # modeling
library(patchwork) # binding graphs together
# ---- declare-globals ---------------------------------------------------------

# ---- declare-functions -------------------------------------------------------
# store script-specific function here

# ---- load-data ---------------------------------------------------------------
meldata <- boot::melanoma


# minimize (if needed) and store a local copy for quick assess and development
# dto %>% readr::write_rds("./data-unshared/derived/00-import-1-small.rds",compress = "xz")


# ---- inspect-data ------------------------------------------------------------


# ---- tweak-data --------------------------------------------------------------
library(tidyverse)
library(finalfit)
melanoma <- boot::melanoma %>%
  mutate(sex.factor = factor(sex) %>%
           fct_recode("Female" = "0",
                      "Male"   = "1") %>%
           ff_label("Sex"),

         ulcer.factor = factor(ulcer) %>%
           fct_recode("Present" = "1",
                      "Absent"  = "0") %>%
           ff_label("Ulcerated tumour"),

         age  = ff_label(age,  "Age (years)"),
         year = ff_label(year, "Year"),

         status.factor = factor(status) %>%
           fct_recode("Died melanoma"  = "1",
                      "Alive" = "2",
                      "Died - other" = "3") %>%
           fct_relevel("Alive") %>%
           ff_label("Status"),

         t_stage.factor =
           thickness %>%
           cut(breaks = c(0, 1.0, 2.0, 4.0,
                          max(thickness, na.rm=TRUE)),
               include.lowest = TRUE)
  )

melanoma <- melanoma %>%
  mutate(
    t_stage.factor =
      fct_recode(t_stage.factor,
                 "T1" = "[0,1]",
                 "T2" = "(1,2]",
                 "T3" = "(2,4]",
                 "T4" = "(4,17.4]") %>%
      ff_label("T-stage")
  )

# 5-year mortality
melanoma <- melanoma %>%
  mutate(
    mort_5yr =
      if_else((time/365) < 5 &
                (status == 1),
              "Yes",          # then
              "No") %>%       # else
      fct_relevel("No") %>%
      ff_label("5-year survival")
  )


ds1 <- melanoma %>%
  as_tibble() %>%
  mutate(

  )

# ---- inspect-data-1 ------
ds1 %>% glimpse()
ds1 %>% look_for()
ds1 %>% explore::describe_all()
# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------


# ---- model-1 -----------------------------------------------------------------
# model to fit:
# mort_5yr ~ sex.factor + ulcer.factor + status.factor + t_stage.factor

# ---- model-2 -----------------------------------------------------------------
library(finalfit)
dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "age", "sex.factor", "t_stage.factor")
fit2 = melanoma %>%
  finalfit(dependent, explanatory, metrics = TRUE)
# ---- model-3 -----------------------------------------------------------------
# ---- model-4 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------
# naming convention: step_id - step_name - cohort_id
path_data_out <- "./data-unshared/derived/0-import-1.rds"
ds1 %>% readr::write_rds(path_data_out)
# ---- publish -----------------------------------------------------------------
# naming convention: step_id - data_transfer_object - cohort_id
# one report (.Rmd) per script (.R), unless report series
path_report_out <- "./manipulation/reports/0-import-1/0-import-1.Rmd"
rmarkdown::render(
  input = path_report_out ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
