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
library(janitor)   # tidy data
library(tidyr)     # data wrangling
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(caret) # cross-validation
library(vip) # variable importance
# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")


# ---- declare-globals ---------------------------------------------------------
# custom function for HTML tables
neat <- function(x, output_format = "html"){
  # knitr.table.format = output_format
  if(output_fo...rmat == "pandoc"){
    x_t <- knitr::kable(x, format = "pandoc")
  }else{
    x_t <- x %>%
      # x %>%
      # neat() %>%
      knitr::kable(format=output_format) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  }
  return(x_t)
}
# Note: when printing to Word or PDF use `neat(output_format =  "pandoc")`


prints_folder <- paste0("./analysis/04-linear-regression/prints/", strftime(Sys.Date()))
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    filename = paste0(name,".jpg"),
    plot     = g,
    device   = "jpg",
    path     = prints_folder,
    # width    = width,
    # height   = height,
    # units = "cm",
    dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}

# data source: https://www.kaggle.com/yamaerenay/world-happiness-report-preprocessed
path <- "https://github.com/mmmmtoasty19/nc-diabetes-epidemic-2020/raw/master/data-public/derived/diabetes-modeling-data.csv.gz"

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv(path)

# ---- tweak-data --------------------------------------------------------------

ds1 <- ds0 %>%
  filter(year == 2012) %>%
  select(
    # county_fips
    # ,county_name
    # ,state_name
     # state_abb
     total_population
    ,adult_pct_total_male_population
    ,adult_pct_total_female_population
    ,adult_pct_white_male_population
    ,adult_pct_white_female_population
    ,adult_pct_black_male_population
    ,adult_pct_black_female_population
    ,adult_pct_american_indian_male_population
    ,adult_pct_american_indian_female_population
    ,adult_pct_asian_male_population
    ,adult_pct_asian_female_population
    ,adult_pct_native_hawaiian_male_population
    ,adult_pct_native_hawaiian_female_population
    ,adult_pct_not_hispanic_male_population
    ,adult_pct_not_hispanic_female_population
    ,adult_pct_hispanic_male_population
    ,adult_pct_hispanic_female_population
    ,pct_total_age_20_44
    ,pct_total_age_45_64
    ,pct_total_age_65_74
    ,pct_total_age_75_over
    ,obesity_percentage
    # ,obesity_lower_limit
    # ,obesity_upper_limit
    ,inactivity_percentage
    # ,inactivity_lower_limit
    # ,inactivity_upper_limit
    ,diabetes_percentage
    # ,diabetes_lower_limit
    # ,diabetes_upper_limit
    ,pct_rural
    # ,rural
  ) %>%
  filter(
    complete.cases(obesity_percentage)
  )

ds1 %>% skimr::skim()

#
d <- ds1 %>% select(adult_pct_total_male_population,adult_pct_total_female_population) %>% as.data.frame()
corr(d$adult_pct_total_male_population, d$adult_pct_total_female_population)

d %>% ggplot(aes(x = adult_pct_total_male_population, y = adult_pct_total_female_population))+
  geom_point(shape = 21, alpha = .6)


set.seed(42)

index_1 <- caret::createDataPartition(ds1$diabetes_percentage, p = .7, list =FALSE)
ds_train <- ds1[index_1, ]
ds_test <- ds1[-index_1, ]


# ----- inspect-data -------

ds0 %>% glimpse()

# ---- get-correlations -------
# inspect correlations in the dataset
# https://statsandr.com/blog/correlogram-in-r-how-to-highlight-the-most-correlated-variables-in-a-dataset/


# devtools::install_github("laresbernardo/lares")
library(lares)


corr_cross(ds1, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)

corr_var(ds1, # name of dataset
         diabetes_percentage, # name of variable to focus on
         top = 10 # display top 5 correlations
)

# ---- table-1 -----------------------------------------------------------------
model1 <- lm(diabetes_percentage ~ obesity_percentage, data = ds1)
summary(model1)

model2 <- lm(diabetes_percentage ~ obesity_percentage + inactivity_percentage, data = ds1)
summary(model2)

model3 <- lm(diabetes_percentage ~ obesity_percentage + inactivity_percentage + adult_pct_white_male_population, data = ds1)
summary(model3)

# ---- graph-1 -----------------------------------------------------------------
set.seed(1234)  # for reproducibility
(cv_model1 <- train(
  form = diabetes_percentage ~ obesity_percentage ,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

(cv_model2 <- train(
  form = diabetes_percentage ~ obesity_percentage + inactivity_percentage ,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

(cv_model3 <- train(
  form = diabetes_percentage ~ obesity_percentage + inactivity_percentage + pct_total_age_20_44 +
    pct_total_age_65_74 + pct_total_age_45_64,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

(cv_model_all <- train(
  form = diabetes_percentage ~ .,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2,
  model3 = cv_model3,
  model_all = cv_model_all
)))

vip(cv_model_all, num_features = 30, method = "model")

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
