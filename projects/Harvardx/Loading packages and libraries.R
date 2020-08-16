# Preparation for R analysis

# Course lessons
# installing the caret package (that requires ggplot2)
# install.packages("caret")
 install.packages("ggplot2", dependencies = TRUE)
# install.packages("magrittr")
# install.packages("purrr")
# install.packages("MASS")
# install.packages("pdftools")
 install.packages("dslabs")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("matrixStats")
# devtools::install_bioc("genefilter")
# install.packages("rpart")
# install.packages("randomForest")
# install.packages("__Rborist__")
# install.packages("gtools")
# install.packages("tidyverse")
# install.packages("utils")
# install.packages("survey")

#loading the libraries that will be used for the lesson - caret and dslabs
library(caret)
 library(dslabs)
 library(magrittr)
# library(purrr)
library(dplyr)
# library(matrixStats)
# library(devtools)
# library(genefilter)
# library(rpart)
# library(Rborist)
# library(gtools)
library(ggplot2)
library(tidyverse)
library (utils)
library (survey)

# loading the libraries with the code provided by the course
# library(maps)## load maps first to avoid map conflict with purrr
# library(MASS) ## load MASS and matrixStats first to avoid select and count conflict
# library(matrixStats) 
 library(tidyverse)
 library(dslabs)
 ds_theme_set()

## Copied from Hadley Wickham and Garrett Grolemund's r4ds
options(digits = 3)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6,
  fig.height = 3.708,  # width * 1 / phi
  fig.show = "hold")

options(dplyr.print_min = 6, dplyr.print_max = 6)

# see the objects that i created
# ls()

# remove the objects that i created
remove(list = ls())

# Preparation for R analysis

# Course lessons
# installing the caret package (that requires ggplot2)
# install.packages("caret")
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("magrittr")
# install.packages("purrr")
# install.packages("MASS")
# install.packages("pdftools")
# install.packages("dslabs")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("matrixStats")
# devtools::install_bioc("genefilter")
# install.packages("rpart")
# install.packages("randomForest")
# install.packages("__Rborist__")
# install.packages("gtools")
# install.packages("tidyverse")

#loading the libraries that will be used for the lesson - caret and dslabs
# library(caret)
library(dslabs)
# library(magrittr)
# library(purrr)
library(dplyr)
# library(matrixStats)
# library(devtools)
# library(genefilter)
# library(rpart)
# library(Rborist)
library(gtools)
library(ggplot2)

# loading the libraries with the code provided by the course
# library(maps)## load maps first to avoid map conflict with purrr
# library(MASS) ## load MASS and matrixStats first to avoid select and count conflict
# library(matrixStats) 
 library(tidyverse)
 library(dslabs)
# ds_theme_set()

## Copied from Hadley Wickham and Garrett Grolemund's r4ds
options(digits = 3)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6,
  fig.height = 3.708,  # width * 1 / phi
  fig.show = "hold")

options(dplyr.print_min = 6, dplyr.print_max = 6)

# see the objects that i created
# ls()

# remove the objects that i created
remove(list = ls())

