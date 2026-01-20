# Run PCA on the categories with more than 2 questions
# variables from preprocessing.R 

# Load required library
library(stats)
#install.packages("PCAmixdata")
library(PCAmixdata)
library(lavaan)
#install.packages("semTools")
library(semTools)


names(mydata)

# Example of how to perform PCA on selected columns
perform_pca <- function(data, columns_for_pca, scale = TRUE) {
  # Select only the columns we want to analyze
  pca_data <- data[, columns_for_pca]
  
  # Perform PCA
  pca_result <- prcomp(pca_data, scale. = scale)
  
  return(pca_result)
}

# social determinants
selected_columns <- c("Q10", "Q13", "Q36","Q37","Q39","Q40")
pca_output <- perform_pca(mydata_filter, selected_columns)
# PCA is for numerical and continuous variables, not for categorical. 
# Use PCAMIX (principal component analysis of mixed data)
data_subset <- mydata_filter[, selected_columns]
# Convert nominal variables to factors
data_subset$Q10 <- as.factor(data_subset$Q10)
data_subset$Q13 <- as.factor(data_subset$Q13)
data_subset$Q39 <- as.factor(data_subset$Q39)

pcamix_result <- PCAmix(
  X.quali = as.data.frame(data_subset[, c("Q10", "Q13", "Q39")]),  # nominal
  X.quanti = as.data.frame(data_subset[, c("Q36", "Q37", "Q40")]), # ordinal
  ndim = 2,
  rename.level = TRUE,
  graph = FALSE
)
summary(pcamix_result)
mydata_filter_PC <- mydata_filter
mydata_filter_PC$social_pc1 <- pcamix_result$scores[,1]

# self-efficacy
selected_columns <- c("Q1", "Q7", "Q8","Q29")
data_subset <- mydata_filter[, selected_columns]
# Convert nominal variables to factors
data_subset$Q1 <- as.factor(data_subset$Q1)
data_subset$Q7 <- as.factor(data_subset$Q7)
data_subset$Q8 <- as.factor(data_subset$Q8)

pcamix_result <- PCAmix(
  X.quali = as.data.frame(data_subset[, c("Q1", "Q7", "Q8")]),  # nominal
  X.quanti = as.data.frame(data_subset[, c("Q29")]), # ordinal
  ndim = 2,
  rename.level = TRUE,
  graph = FALSE
)
summary(pcamix_result)
mydata_filter_PC$selfEff_pc1 <- pcamix_result$scores[,1]

# Perceived vulnerability
selected_columns <- c("Q6", "Q27", "Q28")
data_subset <- mydata_filter[, selected_columns]
# Convert nominal variables to factors
data_subset$Q6 <- as.factor(data_subset$Q6)
data_subset$Q28 <- as.factor(data_subset$Q28)

pcamix_result <- PCAmix(
  X.quali = as.data.frame(data_subset[, c("Q6", "Q28")]),  # nominal
  X.quanti = as.data.frame(data_subset[, c("Q27")]), # ordinal
  ndim = 2,
  rename.level = TRUE,
  graph = FALSE
)
summary(pcamix_result)
mydata_filter_PC$perVul_pc1 <- pcamix_result$scores[,1]

# knowledge
selected_columns <- c("Q3", "Q4", "Q5","Q9","Q12","Q30","Q31")
data_subset <- mydata_filter[, selected_columns]
# Convert nominal variables to factors
data_subset$Q3 <- as.factor(data_subset$Q3)
data_subset$Q4 <- as.factor(data_subset$Q4)
data_subset$Q5 <- as.factor(data_subset$Q5)
data_subset$Q9 <- as.factor(data_subset$Q9)
data_subset$Q12 <- as.factor(data_subset$Q12)
data_subset$Q30 <- as.factor(data_subset$Q30)
data_subset$Q31 <- as.factor(data_subset$Q31)

pcamix_result <- PCAmix(
  X.quali = as.data.frame(data_subset[, c("Q3", "Q4", "Q5","Q9","Q12","Q30","Q31")]),  # nominal
  #X.quanti = as.data.frame(data_subset[, c("Q29")]), # ordinal
  ndim = 2,
  rename.level = TRUE,
  graph = FALSE
)
summary(pcamix_result)
mydata_filter_PC$know_pc1 <- pcamix_result$scores[,1]

names(mydata_filter_PC)

# logistic regression
control <- logistf.control(
  maxit = 2000,        # Increase from default
  maxstep = 0.5,       # Smaller steps
  maxhs = 5,           # Maximum step halving
  lconv = 1e-5,        # Convergence criterion
  xconv = 1e-5        # Parameter convergence
)
firth_model <- logistf(
  cooking_bottle ~ selfEff_pc1+scale(Q2)+perVul_pc1+social_pc1+know_pc1,  # Model formula
  data = mydata_filter_PC,
  pl = T,
  plcontrol = control)
summary(firth_model)

