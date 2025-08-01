knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(here)
library(MASS)
library(tidyverse)
library(survey)
library(questionr)
library(srvyr)
#install.packages("remotes")
#remotes::install_github("carlganz/svrepmisc")
library(svrepmisc)
library(svyVGAM)
#library(apsrtable)
#library(stargazer)
#library(texreg)
#install.packages("devtools")
#devtools::install_github("pewresearch/pewmethods", build_vignettes = TRUE)
library(pewmethods)
library(ri2)
library(modelsummary)
library(car)
library(DescTools)
library(effects)
library(readr)
library(openxlsx)
library(readxl)
library(renv)
library(pewmethods)
library(qualtRics)
library(gridExtra)
library(codebook)
library(likert)
library(excluder)
library(naniar)
library(codebookr)
library(kableExtra)
library(RCT)
library(binom)
#library(psych)
ggplot2::theme_set(ggplot2::theme_bw(base_size = 13))
here()
set.seed(98)
options("modelsummary_factory_default" = "kableExtra")
#options(modelsummary_get = "all")

#renv::restore()
#renv::update()
renv::snapshot()
