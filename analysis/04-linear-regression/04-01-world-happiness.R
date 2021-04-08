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
path <- "./data-public/raw/world-happiness/2020_report.csv"
# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv(path)

# ---- inspect-data ------------------------------------------------------------
ds0 %>% glimpse()

# ---- tweak-data --------------------------------------------------------------
set.seed(42)

index_1 <- caret::createDataPartition(ds0$happiness_score, p = .3, list =FALSE)
ds_train <- ds0[index_1, ]
ds_test <- ds0[-index_1, ]


# ---- inspect ---------
# check for multicollinearity
GGally::ggduo(ds0 %>% select(-country,-continent))

library(corrgram)
corrgram(ds0, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie)

library(corrgram)
corrgram(ds0,
         lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density, order=TRUE)

# ---- basic-model -------------------------------------------------------------
names(ds0)
outcome <- "happiness_score"
predictors <- c(
  "gdp_per_capita"
  ,"social_support"
  ,"health"
  ,"freedom"
  ,"generosity"
  ,"government_trust"
  ,"dystopia_residual"
  ,"continent"
)
equation_formula <- paste0(
  outcome, " ~ ", paste0(predictors, collapse = " + ")
) %>% as.formula()

model00 <- lm( happiness_score ~ gdp_per_capita + social_support + health +
               freedom + generosity + government_trust + dystopia_residual +
               continent, data = ds_train)

summary(model00)


model1 <- lm( happiness_score ~ gdp_per_capita, data = ds_train)

summary(model1)
confint(model1, level = .95)

# ---- table-1 -----------------------------------------------------------------

ds0 %>% ggplot(aes(x=happiness_score))+geom_histogram() + facet_wrap(~continent)

# ---- -------
set.seed(1234)  # for reproducibility
(cv_model1 <- train(
  form = happiness_score ~ freedom,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

(cv_model2 <- train(
  form = happiness_score ~ freedom + government_trust,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

(cv_model3 <- train(
  form = happiness_score ~ freedom + government_trust + generosity,
  data = ds_train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
))

summary(resamples(list(
  model1 = cv_model1,
  model2 = cv_model2,
  model3 = cv_model3
)))
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
