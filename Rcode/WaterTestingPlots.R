setwd("~/Desktop/Jiali/TAMU/environment/starr/Survey/Rcode/")
library(readxl)

TOWN24 <- read_excel("../Twon 2024 List (1).xlsx", sheet = 1)
# remove NAs
TOWN24 <- TOWN24[rowSums(!is.na(TOWN24)) > 0, ]
table(TOWN24$County)
