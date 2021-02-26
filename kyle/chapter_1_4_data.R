# The following script retrieves the data used throughout the book.


# AMES HOUSING
# access data
ames <- AmesHousing::make_ames()

# initial dimension
dim(ames)
## [1] 2930   81

# response variable
head(ames$Sale_Price)


# Employee Attrition
# Note this is different from the book as the data has moved

data("attrition", package = "modeldata")

# initial dimension
dim(attrition)
## [1] 1470   31

# response variable
head(attrition$Attrition)
