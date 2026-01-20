setwd("~/Desktop/Jiali/TAMU/environment/starr/Survey/")
library(ggplot2)

mydata <- read.csv("Survey data with water testing.csv", header = T, row.names = 1)
# remove the missing value
mydata_filter <- na.omit(mydata)

# summary stats
# Convert the frequency counts to a dataframe
mydata_filter <- lapply(mydata_filter, as.character)
freq_count <- do.call(rbind, lapply(names(mydata_filter), function(col) {
  data.frame(
    Column = col,
    Value = as.numeric(names(freq_counts[[col]])),
    Frequency = as.numeric(freq_counts[[col]])
  )
}))

## example code for logistic regression ####

# define dependent and independent variable
# self efficacy 1,7,8,29
# perceived vulnerability 6,27,28
# Perceived benefits 2
# Social determinants 10,13,36,37,39,40
# Knowledge 3,4,5,9,12,30,31
# Exposure history-14,17,24,25,26
# Behavior intention 11,21

# model 1 - original water usage (filter, bottle water)
mydata_filter$Q17_logic <- ifelse(mydata_filter$Q17 == 1, 1,0)
glm_model1 <- glm(
  Q17_logic ~ Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q12+Q27+Q28+Q29+Q30+Q31+Q13+Q36+Q40,  # Model formula
  data = mydata_filter,
  family = "binomial")  # Logistic regression

# View model summary
model_summary <- summary(glm_model1)

# Extract coefficients and standard errors
coef <- coef(glm_model1)
se <- sqrt(diag(model_summary$cov.scaled))

# Calculate 95% confidence intervals
ci_lower <- coef - 1.96 * se
ci_upper <- coef + 1.96 * se

# Combine results and exponentiate for odds ratios
results <- exp(cbind(
  "Odds Ratio" = coef,
  "95% CI Lower" = ci_lower,
  "95% CI Upper" = ci_upper
))


# model 2 - bottle water (filter, bottle water)
mydata_filter$bottle_logic <- ifelse(mydata_filter$Q26 == 3, 1,0)
mydata_filter$cooking_bottle <- ifelse(grepl("1", mydata_filter$Q25), 1, 0)
glm_model2 <- glm(
  bottle_logic ~ Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q12+Q27+Q28+Q29+Q30+Q31,  # Model formula
  data = mydata_filter,
  family = binomial(link = "logit"))  # Logistic regression

# View model summary
model_summary <- summary(glm_model2)

# Extract coefficients and standard errors
coef <- coef(glm_model2)
se <- sqrt(diag(model_summary$cov.scaled))

# Calculate 95% confidence intervals
ci_lower <- coef - 1.96 * se
ci_upper <- coef + 1.96 * se

# Combine results and exponentiate for odds ratios
results <- exp(cbind(
  "Odds Ratio" = coef,
  "95% CI Lower" = ci_lower,
  "95% CI Upper" = ci_upper
))

# Firth's logistic regression
install.packages("logistf")
library(logistf)
firth_model2 <- logistf(
  bottle_logic ~ Q1+Q7+Q8+Q29+Q2+Q6+Q27+Q28+Q3+Q4+Q5+Q9+Q12+Q30+Q31,  # Model formula
  data = mydata_filter)

summary(firth_model4)

# combine predictors into categories
mydata_filter$self_eff <- scale(mydata_filter$Q1 + mydata_filter$Q7 + mydata_filter$Q8+mydata_filter$Q29)
mydata_filter$perc_vul <- scale(mydata_filter$Q6+mydata_filter$Q27+mydata_filter$Q28)
# social determinants
mydata_filter$social <- scale(mydata_filter$Q10 + mydata_filter$Q13 + mydata_filter$Q36+mydata_filter$Q37+mydata_filter$Q39+mydata_filter$Q40)
# score knowledge question
mydata_filter$Q3_logic <- ifelse(mydata_filter$Q3 == 1, 1,0)
mydata_filter$Q4_logic <- ifelse(mydata_filter$Q4 == 1, 1,0)
mydata_filter$Q5_logic <- ifelse(mydata_filter$Q5 == 2, 1,0)
mydata_filter$Q9_logic <- ifelse(mydata_filter$Q9 == 2, 1,0)
mydata_filter$Q12_logic <- ifelse(mydata_filter$Q12 == 1, 1,0)
mydata_filter$Q30_logic <- ifelse(mydata_filter$Q30 == 13, 1,0)
mydata_filter$Q31_logic <- ifelse(mydata_filter$Q31 == 1234, 1,0)
mydata_filter$know <- (mydata_filter$Q3_logic+mydata_filter$Q4_logic+mydata_filter$Q5_logic+mydata_filter$Q9_logic+
                              mydata_filter$Q30_logic+mydata_filter$Q31_logic)
know_df <- data.frame(table(mydata_filter$know))
  
ggplot(data = know_df, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")+
  labs(x="Number of questions correct", y = "Number of people")+
  theme_classic(base_size = 14)
ggsave("img/knowledge correct number.pdf", height = 3, width = 3)

mydata_filter$know <- scale(mydata_filter$know)

control <- logistf.control(
  maxit = 5000,        # Increase from default
  maxstep = 0.5,       # Smaller steps
  maxhs = 5,           # Maximum step halving
  lconv = 1e-5,        # Convergence criterion
  xconv = 1e-5        # Parameter convergence
)
firth_model3 <- logistf(
  bottle_logic ~ self_eff+scale(Q2)+perc_vul+social+know,  # Model formula
  data = mydata_filter,
  plcontrol = control)
summary(firth_model3)

# behave change, no filter -> use filter
mydata_filter$change <- ifelse(mydata_filter$Q17==2 & mydata_filter$Q11 == 1 & mydata_filter$Q21==1, 1,0) 
firth_model4 <- logistf(
  change ~ Q1+Q7+Q8+Q29+Q2+Q6+Q27+Q28+Q3+Q4+Q5+Q9+Q12+Q30+Q31,  # Model formula
  data = mydata_filter,
  plcontrol = control)
summary(firth_model4)

# fisher's test
mydata_filter$Q29_group <- ifelse(mydata_filter$Q29<5, 1,0)
mydata_filter$Q6_group <- ifelse(mydata_filter$Q6==4|mydata_filter$Q6==1, 0,1)
mydata_filter$Q28_group <- ifelse(mydata_filter$Q28==4, 1,0)
mydata_filter$Q36_group <- ifelse(mydata_filter$Q36==3|mydata_filter$Q36==6|mydata_filter$Q36==7, 1,0)
mydata_filter$Q37_group <- ifelse(mydata_filter$Q37==2|mydata_filter$Q37==3|mydata_filter$Q37==4|mydata_filter$Q37==5|mydata_filter$Q37==6, 0,1)
table_multi <- table(mydata_filter$Q29, mydata_filter$bottle_logic)

fisher_result <- fisher.test(table_multi, 
                             simulate.p.value = TRUE,  # For tables larger than 2x2
                             B = 10000)               # Number of simulations

interpret_fisher(fisher_result, table_multi)
# the below function needs to be run before the interprept_fisher function

# Run Fisher's test and create comprehensive interpretation
library(effectsize)

# Detailed interpretation function
interpret_fisher <- function(result, table_data) {
  # Sample size
  n <- sum(table_data)
  
  # Effect size (Cramer's V) - use single number
  cramer <- sqrt(chisq.test(table_data)$statistic / (n * (min(dim(table_data)) - 1)))
  
  # Interpret effect size
  effect_interpretation <- if(cramer < 0.1) {
    "negligible"
  } else if(cramer < 0.3) {
    "small"
  } else if(cramer < 0.5) {
    "moderate"
  } else {
    "large"
  }
  
  # Create interpretation text
  cat("Fisher's Exact Test Results:\n\n")
  cat("1. Statistical Significance:\n")
  cat("   p-value =", format(result$p.value, digits = 3), "\n")
  cat("   ", if(result$p.value < 0.05) "Statistically significant" else "Not statistically significant", "\n\n")
  
  cat("2. Effect Size:\n")
  cat("   Cramer's V =", format(cramer, digits = 3), "\n")
  cat("   This indicates a", effect_interpretation, "effect size\n\n")
  
  cat("3. Table Details:\n")
  cat("   Number of observations:", n, "\n")
  cat("   Table dimensions:", nrow(table_data), "x", ncol(table_data), "\n\n")
  
  cat("4. Observed Frequencies:\n")
  print(table_data)
  cat("\n")
  
  # Expected frequencies
  expected <- chisq.test(table_data, simulate.p.value = TRUE)$expected
  cat("5. Expected Frequencies:\n")
  print(round(expected, 2))
  
  # Row percentages
  cat("\n6. Row Percentages:\n")
  print(round(prop.table(table_data, 1) * 100, 2))
  
  # Column percentages
  cat("\n7. Column Percentages:\n")
  print(round(prop.table(table_data, 2) * 100, 2))
}

perform_fisher_test <- function(data, dependent_var, independent_vars, correction_method = "BH") {
  # Initialize vectors to store results
  variable_names <- character()
  p_values <- numeric()
  cramers_v <- numeric()
  
  # Loop over the selected independent variables
  for (var in independent_vars) {
    # Create a contingency table
    cont_table <- table(data[[dependent_var]], data[[var]])
    
    # Perform Fisher's exact test
    fisher_result <- fisher.test(cont_table)
    
    # Calculate Cramer's V
    chi_result <- chisq.test(cont_table)
    cramer_v <- sqrt(chi_result$statistic / (sum(cont_table) * (min(dim(cont_table)) - 1)))
    
    # Store results
    variable_names <- c(variable_names, var)
    p_values <- c(p_values, fisher_result$p.value)
    cramers_v <- c(cramers_v, as.numeric(cramer_v))
  }
  
  # Apply p-value correction
  corrected_p_values <- p.adjust(p_values, method = correction_method)
  
  # Create significance indicators
  sig_raw <- ifelse(p_values < 0.05, "*", "")
  sig_corrected <- ifelse(corrected_p_values < 0.05, "*", "")
  
  # Create results dataframe
  results_df <- data.frame(
    Variable = variable_names,
    P_Value = round(p_values, 4),
    P_Corrected = round(corrected_p_values, 4),
    Cramers_V = round(cramers_v, 3),
    Sig_Raw = sig_raw,
    Sig_Corrected = sig_corrected,
    stringsAsFactors = FALSE
  )
  
  # Sort by corrected p-value
  results_df <- results_df[order(results_df$P_Corrected), ]
  
  return(results_df)
}

dependent_var <- "cooking_bottle"
independent_vars <- c("Q28_group","Q36_group","Q37_group","Q1","Q2","Q3","Q4","Q5","Q7","Q8","Q9","Q10",
                      "Q12","Q13","Q27","Q30","Q31","Q39","Q40","Q29","Q6") #"Q14","Q29_group", Q6_group", 

# Perform Fisher's exact test for each independent variable
results <- perform_fisher_test(mydata_filter, dependent_var, independent_vars)

# Print the results
print(results)
 
table(mydata_filter$Q29, mydata_filter$cooking_bottle)

