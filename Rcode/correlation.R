setwd("~/Desktop/Jiali/TAMU/environment/starr/Survey/")

library(ggplot2)
library(reshape2)
library(dplyr)

# remodel the table into 2x2 format
table_mod <- read.csv("2x2table-survey responces.csv", header = T, row.names = 1)

# remove the questions I don't want to include Q15,16,18,19,20,22,23,34
table_mod_filter <- table_mod[,-c(15,16,18,19,20,22,23,34)]

# Function to calculate phi coefficient with missing value handling
phi_coefficient <- function(x, y, use = "complete.obs") {
  # Remove missing values based on specified method
  if (use == "complete.obs") {
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]
  }
  
  # Check if enough data remains after NA removal
  if (length(x) < 2 || length(y) < 2) {
    return(NA)
  }
  
  if (length(unique(x)) != 2 || length(unique(y)) != 2) {
    return(NA)
  }
  
  # Create contingency table
  cont_table <- table(x, y)
  
  # Check if it's a 2x2 table
  if (any(dim(cont_table) != 2)) {
    return(NA)
  }
  
  # Calculate phi
  chi_stat <- chisq.test(cont_table, correct=FALSE)$statistic
  n <- sum(cont_table)
  phi <- sqrt(chi_stat/n)
  
  # Determine sign
  if (cont_table[1,1]*cont_table[2,2] < cont_table[1,2]*cont_table[2,1]) {
    phi <- -phi
  }
  
  return(phi)
}

# Function for pairwise phi correlation matrix with missing handling
pairwise_phi <- function(df, use = "complete.obs") {
  n_cols <- ncol(df)
  col_names <- names(df)
  phi_matrix <- matrix(NA, n_cols, n_cols)
  colnames(phi_matrix) <- col_names
  rownames(phi_matrix) <- col_names
  
  # Add missing value counts
  na_counts <- colSums(is.na(df))
  
  for(i in 1:n_cols) {
    for(j in 1:n_cols) {
      phi_matrix[i,j] <- phi_coefficient(df[[i]], df[[j]], use=use)
    }
  }
  
  return(list(
    correlations = phi_matrix,
    na_counts = na_counts,
    complete_cases = sum(complete.cases(df))
  ))
}


phi_result <- pairwise_phi(table_mod_filter)
print(phi_result$correlations)  # Show correlation matrix
print(phi_result$na_counts)     # Show missing values per variable
print(phi_result$complete_cases)  # Show number of complete cases

cor_matrix <- phi_result$correlations


# Prepare data for ggplot
melted_cor <- melt(cor_matrix)
names(melted_cor) <- c("Var1", "Var2", "Correlation")

# Keep only upper triangle and remove NA correlations
melted_cor <- melted_cor %>%
  mutate(
    Var1 = factor(Var1, levels = colnames(cor_matrix)),
    Var2 = factor(Var2, levels = colnames(cor_matrix))
  ) %>%
  filter(as.numeric(Var1) > as.numeric(Var2)) %>%  # Keep lower triangle
  filter(!is.na(Correlation))  # Remove NAs

# Create the triangular heatmap
ggplot(melted_cor, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Correlation)),
    color = ifelse(abs(melted_cor$Correlation) > 0.5, "white", "black"),
    size = 1.8
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  coord_fixed() +
  labs(
    title = "Phi Correlation Heatmap",
    fill = "Correlation"
  )

ggsave("img/correlation plot.pdf", height = 6, width = 8)
