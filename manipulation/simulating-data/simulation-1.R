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
library(MASS)
library(caret) # cross-validation
library(vip) # variable importance
library(lares) # correlations
# ---- load-sources ------------------------------------------------------------

source("./scripts/common-functions.R")
source("./manipulation/simulating-data/steiger-r-functions.R")


# ---- declare-globals ---------------------------------------------------------
# custom function for HTML tables
neat <- function(x, output_format = "html"){
  # knitr.table.format = output_format
  if(output_format == "pandoc"){
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


prints_folder <- paste0("./analysis/.../prints/", strftime(Sys.Date()))
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
# ---- load-data ---------------------------------------------------------------

CompleteSymmetricMatrix <- function(x) {L <- length(x)
p <- (sqrt(8*L + 1) -1)/2
y <- matrix(0,p,p)
count <- 0
for(i in 1:p ) {for (j in 1:i){count <- count+1
y[i,j] <- x[count]
y[j,i] <- x[count]
}
}
y
}

set.seed(45)
mu <- c(8,6,7,7,8,8,7,8,9,6) # vector of means
(p <- length(mu)) # number of variables
# Covariance matrix
Sigma <- CompleteSymmetricMatrix(
  c(12,
    4,13,
    2,6,12,
    3,5,6,11,
    4,7,5,8,12,
    2,2,6,5,6,13,
    2,6,8,3,6,6,11,
    4,3,5,5,3,1,2,12,
    2,3,2,3,4,5,6,2,9,
    2,5,4,3,2,8,4,7,3,12

  )
)
x1 <- MASS::mvrnorm(n = 100, mu = mu, Sigma = Sigma,tol = .2)
ds1 <- tibble::as_tibble(x1) %>% janitor::clean_names()
ds1 %>%  explore::describe_all()
library("PerformanceAnalytics")
chart.Correlation(ds1, histogram=TRUE, pch=19)
# GGally::ggpairs(ds1)

# ---- inspect-data ------
library(lares)
# Strongest correlations in the dataset
corr_cross(ds1, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
# Strongest correlations with a given variables
corr_var(ds1, # name of dataset
         v1, # name of variable to focus on
         top = 10 # display top 5 correlations
)
# linear model
m1 <- lm(v1 ~ ., data = ds1)
m1 %>% summary()
broom::glance(m1)
broom::tidy(m1)
(cv_model_all <- caret::train(
    form = v1 ~ .,
    data = ds1,
    method = "lm",
    trControl = trainControl(method = "cv", number = 10)
))
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
