# The following script is used for EDA of the common data sets used throughout
# the book.  It is not meant to perform any modeling but to get to know the data


rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# if the line above DOES NOT generates the project root, re-map by selecting
# Session --> Set Working Directory --> To Project Directory location
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
library(magrittr)  # pipes
library(dplyr)     # data wrangling
library(ggplot2)   # graphs


# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")


# ---- declare-globals ---------------------------------------------------------

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)

# ---- load-data ---------------------------------------------------------------

# Please see section 1.4 of book for data set descriptions

# AMES HOUSING

# access data
ames <- AmesHousing::make_ames()

# initial dimension
dim(ames)

# response variable
head(ames$Sale_Price)

#glimpse at data
glimpse(ames)

ames %>% neat_DT()

# Employee Attrition
# Note this is different from the book as the data has moved

# Can not pull data without using utlity pacakge
data("attrition", package = "modeldata")


attrition %>% neat_DT()


#MNIST Data

mnist <- dslabs::read_mnist()

# This data is split into test and train automatically.
# Data is too large to view

# Grocery Data

# URL to download/read in the data
url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"

# Access data
my_basket <- readr::read_csv(url)

my_basket %>% neat_DT()



# ---- tweak-data --------------------------------------------------------------


# ---- table-1 -----------------------------------------------------------------

# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------


# ----- publish ----------------------------------------------------------------
path <- "./analysis/report/report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
