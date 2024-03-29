### Preliminary Loading, Data Intake, Etc. ###
set.seed(20380907)
# Load Libraries
library(Amelia)
library(snow)
library(sandwich)
library(lmtest)

# Import Raw Data
MERS_Raw <- read.csv("cleaned_MERS_Aug4.csv")
MERS_Raw$ROK <- 0
MERS_Raw$ROK[MERS_Raw$country=="South Korea"] <- 1

### Build Working Copy, Remove All Unneeded Variables ###
# Subset to non-ROK Cases
MERS <- MERS_Raw[which(MERS_Raw$ROK==0),]
MERS$ROK <- NULL
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

# Delete cluster variables, middling-poor data quality
MERS$cluster <- NULL
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
MERS$contact <- NULL
MERS$prior_travel <- NULL
MERS$inferred <- NULL
MERS$citation5 <- NULL

## Variable Recodes ##
# Recode gender to be interpretable
MERS$female <- NA
MERS$female[MERS$gender=="M"] <- 0
MERS$female[MERS$gender=="F"] <- 1
MERS$gender <- NULL

# Convert Date Variables to Days Since 1/1/2012
MERS$jan1 <- as.Date("2012-01-01")
MERS$onset <- as.Date(MERS$onset)
MERS$onset <- MERS$onset - MERS$jan1
MERS$onset <- as.numeric(MERS$onset)
MERS$hospitalized <- as.Date(MERS$hospitalized)
MERS$hospitalized <- MERS$hospitalized - MERS$jan1
MERS$hospitalized <- as.numeric(MERS$hospitalized)
MERS$sampled <- as.Date(MERS$sampled,"%Y-%m-%d")
MERS$sampled <- MERS$sampled - MERS$jan1
MERS$sampled <- as.numeric(MERS$sampled)
MERS$reported <- as.Date(MERS$reported,"%Y-%m-%d")
MERS$reported <- MERS$reported - MERS$jan1
MERS$reported <- as.numeric(MERS$reported)
MERS$jan1 <-NULL

# Create Hospitalization Delay Variable
# Set to 0 if there is a negative value (preexisting hospitalization)
MERS$hosp_delay <- MERS$hospitalized - MERS$onset
MERS$hosp_delay[MERS$hosp_delay<0] <- 0

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
MERS$cam[MERS$camel_contact=="FALSE"] <- 0
MERS$cam[MERS$camel_contact=="TRUE"] <- 1
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

# Recode Severity to Numeric and collapse to 0/1
MERS$sev <- NA
MERS$sev[MERS$severity2=="asymptomatic"] <- 0
MERS$sev[MERS$severity2=="mild"] <- 0
MERS$sev[MERS$severity2=="moderate"] <- 0
MERS$sev[MERS$severity2=="severe"] <- 1
MERS$sev[MERS$severity2=="fatal"] <- 1
MERS$severity2 <- NULL

### Multiple Imputation ###
# Disable some date variables, causing singularity issues
# Information is built into other variables
MERS$hospitalized <- NULL
MERS$sampled <- NULL
MERS$reported <- NULL

MERS$country <- NULL
MERS$city <- NULL

# Multiply impute with very small ridge prior to help with numerical stability
# As per Honaker, King and Blackwell, allowing all integer-valued ordinal data to be modeled as continuous unless statistical
# model requires a bound, as with dichotomous outcomes
mi.mers <- amelia(MERS, m=100, ords=c("Death","sev"), idvars="number",parallel="snow",ncpus=4, p2s=2,empri = .01*nrow(MERS))

### Outcome = Death ###
# Univariate Binomial Models
# Starting values obtained from logistic models
# Age
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ age, family= poisson, data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
age_combined <- mi.meld(q = b.out, se = se.out)
print(age_combined)

# Onset
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ onset, family= poisson, data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
onset_combined <- mi.meld(q = b.out, se = se.out)
print(onset_combined)

# Comorbidity
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ comorbidity, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
comorb_combined <- mi.meld(q = b.out, se = se.out)
print(comorb_combined)

# Animal Contact
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ animal_contact, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
ani_combined <- mi.meld(q = b.out, se = se.out)
print(ani_combined)

# Camel Contact
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ camel_contact, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
cam_combined <- mi.meld(q = b.out, se = se.out)
print(cam_combined)

# HCW
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ HCW, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
hcw_combined <- mi.meld(q = b.out, se = se.out)
print(hcw_combined)

# Secondary
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ secondary, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
sec_combined <- mi.meld(q = b.out, se = se.out)
print(sec_combined)

# Saudi
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ saudi, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
saudi_combined <- mi.meld(q = b.out, se = se.out)
print(saudi_combined)

# Female
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ female, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
fem_combined <- mi.meld(q = b.out, se = se.out)
print(fem_combined)

# Hospital Delay
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ hosp_delay, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
hosp_combined <- mi.meld(q = b.out, se = se.out)
print(hosp_combined)

# Multivariate Model
# Including all marginally associated (p < 0.20) variables
# Age, Comorbidity, Onset, Animal Contact, HCW, Secondary Case, Female
# Not yet thrilled with these estimates

b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(Death ~ age + onset + comorbidity + female + HCW + hosp_delay, 
                  family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
multi_combined <- mi.meld(q = b.out, se = se.out)
print(multi_combined)

### Outcome = Severe ###
# Univariate Binomial Models
# Age
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ age, family= poisson, data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
sage_combined <- mi.meld(q = b.out, se = se.out)
print(sage_combined)

# Onset
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ onset, family= poisson, data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
sonset_combined <- mi.meld(q = b.out, se = se.out)
print(sonset_combined)

# Comorbidity
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ comorbidity, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
scomorb_combined <- mi.meld(q = b.out, se = se.out)
print(scomorb_combined)

# Animal Contact
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ animal_contact, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
sani_combined <- mi.meld(q = b.out, se = se.out)
print(sani_combined)

# Camel Contact
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ camel_contact, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
scam_combined <- mi.meld(q = b.out, se = se.out)
print(scam_combined)

# HCW
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ HCW, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
shcw_combined <- mi.meld(q = b.out, se = se.out)
print(shcw_combined)

# Secondary
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ secondary, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
ssec_combined <- mi.meld(q = b.out, se = se.out)
print(ssec_combined)

# Saudi
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ saudi, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
ssaudi_combined <- mi.meld(q = b.out, se = se.out)
print(ssaudi_combined)

# Female
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ female, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
sfem_combined <- mi.meld(q = b.out, se = se.out)
print(sfem_combined)

# Hospital Delay
b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ hosp_delay, family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
shosp_combined <- mi.meld(q = b.out, se = se.out)
print(shosp_combined)

# Multivariate Model
# Including all marginally associated (p < 0.20) variables
# Age, Comorbidity, Onset, Animal Contact, HCW, Secondary Case, Female, Saudi
# Not yet thrilled with these estimates

b.out <- NULL
se.out <- NULL
for(i in 1:mi.mers$m){
  uni.out <- glm(sev ~ age + onset + comorbidity + HCW + secondary + female + saudi, 
                 family= poisson,data=mi.mers$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
smulti_combined <- mi.meld(q = b.out, se = se.out)
print(smulti_combined)

# Density Plotting
Death1 <- subset(MERS, Death == 1)
Death0 <- subset(MERS, Death == 0)

Death1den <- density(Death1$age, na.rm=TRUE)
Death0den <- density(Death0$age, na.rm=TRUE)

par(mar=c(5.1,7.1,4.1,2.1))
plot(Death1den,col="black", lwd = 3,xlab="Age, years",ylab="", main="", 
     cex.lab=1.3,axes=FALSE,bty="n",xlim=c(0,100),ylim=c(0,0.03))
axis(2, cex.axis=1.3,lwd=2,las=2,col="black")
mtext("Density", side=2, line=5, cex=1.3,las=3)
axis(1, cex.axis=1.3,las=1,,lwd=2,col="black",line=-0.9)
abline(h=0, lty=1, col="black",lwd=2)
lines(Death0den, col="black", lwd = 3,lty=2)
legend("topright", c("Fatal Cases","Nonfatal Cases"), lwd=3, col="black",
       lty=c(1,2), pch=c(NA,NA), bty='y',inset=0.1, cex=1.3,box.lwd=2)

