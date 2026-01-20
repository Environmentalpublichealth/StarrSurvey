setwd("~/Desktop/Jiali/TAMU/environment/starr/Survey/Rcode/")
# Bayesian SEM
if (!require(blavaan)) install.packages("blavaan")
library(blavaan)
library(lavaan)

# Calculate loading factors 
# Define the measurement model
model <- '
  # Define latent variable
  Know =~ Q3 + Q4 + Q5 + Q9 + Q12 + Q30 + Q31
'

model <- '
  # Define latent variable
  SelfEff =~ Q1 + Q7 + Q8 + Q29
'
model <- '
  # Define latent variable
  Vulner =~ Q6 + Q27 + Q28
'
model <- '
  Social =~ Q10 + Q13 + Q36_group + Q37_group + Q39 + Q40
'
model <- '
  Water =~ As + Nitrate + TDS + Coliform
'
model <- '
  Exposure =~ Q14 + Q17
  Benefit =~ Q2
'

# Step 3: Run Bayesian CFA
freq_fit <- cfa(model, data = mydata_filter, std.lv = TRUE)
bsem_fit <- bcfa(model, 
                 data = mydata_filter,
                 std.lv = TRUE,           # Standardize latent variables
                 burnin = 1000,           # Burn-in iterations
                 sample = 2000,           # Number of posterior samples
                 n.chains = 3,              # Number of MCMC chains
                 mcmcfile = TRUE)         # Save MCMC output

# Fit the Bayesian SEM model
# Using default priors for demonstration
bsem_fit <- bsem(model,
                 data = mydata_filter,
                 n.chains = 3, 
                 ordered = c("bottle_logic","Q1","Q7","Q8","Q29","Q6","Q27","Q28","Q3","Q2",
                             "Q4","Q5","Q9","Q12","Q30","Q31","Q10","Q13","Q36_group","Q14","Q17",
                             "Q37_group","Q39","Q40","Coliform"),
                 burnin = 1000,    # Number of burnin iterations
                 sample = 2000,    # Number of samples per chain
                 target = "stan",  # Use Stan as the MCMC sampler
                 seed = 123,
                 dp=dpriors(lambda = "normal(0.8,0.2"))       

# Extract and examine factor loadings
loadings <- standardizedPosterior(bsem_fit, type = "std.all")
loading_summary <- summary(bsem_fit)

# Create a function to extract factor loading statistics
get_factor_loadings <- function(bsem_object) {
  # Get parameter estimates for loadings
  params <- parTable(bsem_object)
  loading_params <- params[params$op == "=~", ]
  
  # Extract posterior distributions
  posterior_samples <- standardizedPosterior(bsem_object, type = "std.all")
  
  # Debug: Print column names to see what's available
  print("Available columns in posterior samples:")
  print(colnames(posterior_samples))
  
  # Get loading columns (they usually start with ".p")
  loading_cols <- grep("=~", colnames(posterior_samples), value = TRUE)
  print("Found loading columns:")
  print(loading_cols)
  
  # Extract only the factor loading columns from posterior samples
  loading_posteriors <- posterior_samples[, loading_cols, drop = FALSE]
  
  # Calculate statistics for each loading
  loading_stats <- data.frame(
    Indicator = loading_params$rhs,
    Mean = colMeans(loading_posteriors),
    SD = apply(loading_posteriors, 2, sd),
    Q2.5 = apply(loading_posteriors, 2, quantile, probs = 0.025),
    Q97.5 = apply(loading_posteriors, 2, quantile, probs = 0.975)
  )
  
  return(loading_stats)
}

# Get detailed loading statistics
loading_results <- get_factor_loadings(bsem_fit)


# Specify the model
# Model specification
model <- '
  # Measurement model for latent variables
  
  # First latent variable and its indicators
  SelfEff =~ Q1 + Q7 + Q8 + Q29  
  
  # Third latent variable and its indicators
  Vulner =~ Q6 + Q27 + Q28
  
  # Fourth latent variable and its indicators
  Know =~ Q3 + Q4 + Q5 + Q9 + Q12 + Q30 + Q31
  
  # Fifth latent variable and its indicators
  Social =~ Q10 + Q13 + Q36 + Q37 + Q39 + Q40
  # water quality
  Water =~ As + Nitrate + TDS + Coliform
  
  Exposure =~ Q14 + Q17
  
  Benefit =~ Q2
  
  # Structural model
  # Binary outcome predicted by latent variables
  #bottle_logic ~ SelfEff  + Know + Social+ Vulner+Water+Exposure+Benefit
  
'

simple_model <- '
  # Measurement model for latent variables
  # Replace v1.1, v1.2, etc. with your actual indicator variable names
  
  # First latent variable and its indicators
  SelfEff =~ Q29  #Q1 + Q8 + 
  
  # Third latent variable and its indicators
  #Vulner =~ Q6 
  
  # Fourth latent variable and its indicators
  #Know =~ Q3  + Q5 +Q12 #+ Q4 + Q30
  
  # Fifth latent variable and its indicators
  Social =~ Q10  +Q37_group #+ Q39 #+ Q13
  
  #Water =~ As+Nitrate+TDS
  
  #Exposure =~ Q4
  
  #Benefit =~ Q2
  
  # Structural model
  # Binary outcome predicted by latent variables
   #bottle_logic ~ b1*Social + b4*SelfEff  #+ b6*Vulner #+ b5*Know
   #SelfEff ~ a1*Social
   #Know ~ a2*Social
   #Vulner ~ a3*Water
   
  # Binary outcome predicted by latent variables
   change ~ b1*Social + b6*SelfEff
   SelfEff ~ a1*Social
   #Vulner ~ a2*Water
  
  #cooking_bottle ~ b1*Water + b4*Vulner + b5*Benefit #+ b6*SelfEff
  #Vulner ~ a1*Water
  #Benefit ~ a2*Water
  #SelfEff ~ a3*Water
  
  #cooking_bottle ~ b1*Exposure + b3*Know+ b4*Vulner + b5*Benefit + b6*SelfEff
  #Vulner ~ a1*Exposure
  #Benefit ~ a2*Exposure
  #SelfEff ~ a3*Exposure
  #Know ~ a4*Exposure
  
  # Define indirect effects using labeled parameters
  #indirect1 := a1*b4   # X1->X4->Y
  #indirect2 := a2*b5   # X1->X5->Y
  #indirect3 := a3*b6   # X2->X6->Y
  indirect3 := a1*b6   # X1->X6->Y
  #indirect4 := a2*b6   # X2->X6->Y
  
'  
# Define priors based on Rowles et al. (2020) Figure 4 and Table SI.2a
rowles_priors <- dpriors(
  
  # MEASUREMENT MODEL (Loadings)
  # Based on Table SI.2a [1]
  lambda = "normal(0.84, 0.1)" 
  
  # STRUCTURAL MODEL (Regressions)
  # Based on Figure 4 [4]
  
  # 1. PHR -> PWQ (Rowles found -0.655)
  #beta = "normal(-0.655, 0.1)", 
  
  # 2. PWQ -> DTW (Rowles found 0.146)
  # Note: Use a wider SD (0.2) because Rowles found this non-significant
  #beta = "normal(0.146, 0.2)" 
)
# Fit the model with appropriate estimator for categorical outcomes
#For Bayesian estimation:
fit <- bsem(simple_model,
            data = mydata_filter,
            ordered = c("change","Q1","Q7","Q8","Q29","Q6","Q27","Q28","Q3","Q2",
                        "Q4","Q5","Q9","Q12","Q30","Q31","Q10","Q13","Q36_group",
                        "Q37_group","Q39","Q40","Coliform","Q14","Q17"),
            
            std.lv = TRUE,
            burnin = 2000,
            sample = 4000, # Standardize latent variables
            seed = 123,
            dp = rowles_priors #dpriors(lambda = "normal(0.8,0.1)"
            )


# Get standardized posterior distributions
post <- standardizedPosterior(fit)

# Calculate summary statistics for each parameter
# This will give you means and credible intervals
parameter_summary <- apply(post, 2, function(x) {
  c(mean = mean(x),
    sd = sd(x),
    ci_lower = quantile(x, 0.025),
    ci_upper = quantile(x, 0.975))
})

# Convert to more readable format
results <- data.frame(t(parameter_summary))
print(results)

summary(fit, fit.measures = TRUE)
fitMeasures(fit)

