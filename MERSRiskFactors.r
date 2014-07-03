### Preliminary Loading, Data Intake, Etc. ###
# Load Libraries
library(Amelia)

# Import Raw Data
MERS_Raw <- read.csv("cleaned_MERS_Jun4.csv")

### Build Working Copy, Remove All Unneeded Variables ###
MERS <- MERS_Raw
MERS$X <- NULL
MERS$FT <- NULL
MERS$KSA_case <- NULL
MERS$code <- NULL

# Missing data due to lack of reporting or not present for all reporting countries
# Unlikely to yield good information
MERS$province <- NULL 
MERS$district <- NULL
MERS$hospital <- NULL
MERS$exposure <- NULL

# Will Use Date of Onset, Report and Date of Hospitalization
# Dates of Death and Discharge only make sense in a survival context
MERS$death <- NULL
MERS$discharged <- NULL

# Severity Outcomes, Will Use Combined Variables
MERS$severity <- NULL
MERS$outcome <- NULL
MERS$clinical <- NULL

# Keep only the new cluster variable
MERS$old_cluster <- NULL
MERS$Cauchemez.cluster <- NULL

# Paring down contact information and other admin variables
MERS$contact_with <- NULL
MERS$suspected <- NULL
MERS$notes <- NULL
MERS$citation <- NULL
MERS$citation2 <- NULL
MERS$citation3 <- NULL
MERS$citation4 <- NULL
MERS$sequence <- NULL
MERS$accession <- NULL
MERS$patient <- NULL
MERS$speculation <- NULL

## Variable Recodes ##
# Recode gender to be interpretable
MERS$female <- NA
MERS$female[MERS$gender=="M"] <- 0
MERS$female[MERS$gender=="F"] <- 1
MERS$gender <- NULL

# Convert Date Variables to Dates
MERS$onset <- as.Date(MERS$onset)
MERS$hospitalized <- as.Date(MERS$hospitalized)
MERS$sampled <- as.Date(MERS$sampled,"%Y-%m-%d")
MERS$reported <- as.Date(MERS$reported,"%Y-%m-%d")

# Create Two Lag Variables
# Set to 0 if there is a negative value (preexisting hospitalization)
MERS$hosp_delay <- MERS$hospitalized - MERS$onset
MERS$hosp_delay[MERS$hosp_helay<0] <- 0
MERS$report_delay <- MERS$reported - MERS$onset

# Recode Comorbidity as 0/1
MERS$comorb <- NA
MERS$comorb[MERS$comorbidity=="FALSE"] <- 0
MERS$comorb[MERS$comorbidity=="TRUE"] <- 1
MERS$comorb[MERS$comorbidity=="TRUE?"] <- 1
MERS$comorbidity <- MERS$comorb
MERS$comorb <- NULL

# Recode Animal and Camel Contact Variables as 0/1
MERS$ani <- NA
MERS$ani[MERS$animal_contact=="FALSE"] <- 0
MERS$ani[MERS$animal_contact=="TRUE"] <- 1
MERS$animal_contact <- MERS$ani
MERS$ani <- NULL

MERS$cam <- NA
MERS$cam[MERS$camel_contact=="False"] <- 0
MERS$cam[MERS$camel_contact=="True"] <- 1
MERS$camel_contact <- MERS$cam
MERS$cam <- NULL

# Recode HCW as 0/1
MERS$H <- NA
MERS$H[MERS$HCW=="False"] <- 0
MERS$H[MERS$HCW=="True"] <- 1
MERS$HCW <- MERS$H
MERS$H <- NULL

# Recode Secondary Case as 0/1
MERS$sec <- NA
MERS$sec[MERS$secondary=="FALSE"] <- 0
MERS$sec[MERS$secondary=="TRUE"] <- 1
MERS$secondary <- MERS$sec
MERS$sec <- NULL

# Recode condensed_health to a 0/1 with meaningful interpretation
MERS$Death <- NA
MERS$Death[MERS$condensed_health=="fatal"] <- 1
MERS$Death[MERS$condensed_health=="Alive"] <- 0
MERS$condensed_health <- NULL










