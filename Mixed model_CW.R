rm(list=ls())
library(lme4)
load("~/Downloads/All Modules/Sem_2/Mixed Models with Medical Applications/CW/cancer.Rdata")
makesample(0797)
save(CWsample, file = "/Users/shrichandbhuria/Downloads/All Modules/Sem_2/Mixed Models with Medical Applications/CW/CWsample.RData")
head(CWsample)
names(CWsample)
# Find unique values of the "Type" variable
unique(CWsample$Type)
unique(CWsample$Treatment)
summary(CWsample$AgeDiagnosis)

summary(CWsample$Type)
summary(CWsample$Treatment)
CWsample$Chemotherapy <- (CWsample$Treatment == "Chemotherapy") 
CWsample$Baldder <- (CWsample$Type == "Baldder") 

##############Step 1: Data Summary------------------------------------------------------------------------------
######1. Tabulate the Number of Patients in Each Site:
# Tabulate the number of patients in each site
site_counts <- table(CWsample$SiteID)
# Convert the table to a data frame for better readability
site_counts_df <- as.data.frame(site_counts)
names(site_counts_df) <- c("SiteID", "Number_of_Patients")
# Transpose the table
site_counts_df_transposed <- t(site_counts_df)
# Convert the transposed table into a data frame
site_counts_df_transposed <- as.data.frame(site_counts_df_transposed, stringsAsFactors = FALSE)
# Display the transposed table
site_counts_df_transposed
# Remove the "V1", "V2", etc. column names
colnames(site_counts_df_transposed) <- NULL
# Display the transposed table
site_counts_df_transposed





CWsample$SiteID <- as.factor(CWsample$SiteID)


#LINEAR MODEL
model_0 <- lm(AgeDeath ~ 1, data = CWsample)
summary(model_0)
betahat <- model_0$coefficients
betahat
summary(CWsample$AgeDeath)



# FIGURE 1
par(mfrow = c(1,2)) 
# Histogram plot for Age at Death
hist(CWsample$AgeDeath, 
     xlab = "Age at Death (years)", prob = TRUE, 
     main = "Histogram plot of Age at Death", 
     col = "#EBF0FF",
     breaks = 35)  # Specify the number of bins
# Add density line
lines(density(CWsample$AgeDeath), lwd = 2, col = "blue")
# QQ plot for Age at Death
qqnorm(CWsample$AgeDeath, main="QQ plot of Age at Death")
qqline(CWsample$AgeDeath, lwd = 2, col = "green")  # Add a reference line


# Histogram plot for Age at Diagnosis
hist(CWsample$AgeDiagnosis, 
     xlab = "Age at Diagnosis (years)", prob = TRUE, 
     main = "Histogram plot of Age at Diagnosis", 
     col = "#EBF0FF",
     breaks = 25)  # Specify the number of bins
# Add density line
lines(density(CWsample$AgeDiagnosis), lwd = 2, col = "blue")
# QQ plot for Age at Diagnosis
qqnorm(CWsample$AgeDiagnosis, main="QQ plot of Age at Diagnosis")
qqline(CWsample$AgeDiagnosis, lwd = 2, col = "green")  # Add a reference line


# Calculate summary statistics for AgeDeath
summary_stats_age_death <- summary(CWsample$AgeDeath)
summary_stats_age_death

# Calculate summary statistics for AgeDiagnosis
summary_stats_age_diagnosis <- summary(CWsample$AgeDiagnosis)
summary_stats_age_diagnosis

#########step 2-------------------------------------------------------------------------------
# Fit a single-level regression model
model_A <- lm(AgeDeath ~ AgeDiagnosis, data = CWsample)

# Print the coefficient estimates
summary(model_A)$coefficients

#FIGURE 2
par(mfrow = c(1, 2))    
plot(CWsample$AgeDiagnosis, CWsample$AgeDeath,  
     main = "Scatter plot of Age at Death \n vs. Age at Diagnosis",  
     ylab = "Age at death (years)", pch = 1, cex = 0.5, lwd = .5,
     xlab = "Age at Diagnosis (years)")
abline(model_A, lwd = 1.5, col = "purple")
qqnorm(model_A$resid, main="Normal QQ plot of Age at Death \n vs. Age at Diagnosis", cex = 0.45)
qqline(model_A$resid, col="orange", lwd=2)


model_A



# Calculate the Pearson correlation coefficient
correlation <- cor(CWsample$AgeDiagnosis, CWsample$AgeDeath)

# Print the correlation coefficient
correlation


# Create a list to store AgeDeath values for each site
site_data <- lapply(unique(CWsample$SiteID), function(site) {
  subset(CWsample, SiteID == site)$AgeDeath
})
# Define colors for the legend
colours <- rev(c("black", "blue", "red3", "magenta", "seagreen", "slateblue", "deepskyblue3", "green", "pink", "khaki", "purple", "orange"))

# Plot data points for each site (transposed)
par(mfrow=c(1,1))
par(mar = c(4, 8, 7, 4))  # Transposed margin values
plot(1, type="n", xlim=c(30, 100), ylim=c(0.5, length(site_data) + 0.5), 
     xlab = expression(bold("Age at Death (years)")), ylab = expression(bold("SiteID")), main = "Plot of Age at Death (years) split by hospital SiteID", yaxt="n")
# Add data points for each site
for (i in 1:length(site_data)) {
  points(site_data[[i]], rep(i, length(site_data[[i]])), pch=16, col=colours[i], cex = 0.7)
}
# Add labels and adjust size
axis(2, at = 1:length(site_data), labels = unique(CWsample$SiteID), cex.axis =.6, lwd.tick = 2, font.axis = 2)
# Add label for estimated value and adjust size
axis(3, at = 52.6, labels = expression(paste(hat(beta)[0], " = 52.6")), cex.axis = 0.8)
# Add legend and adjust size
legend("topright", legend = unique(CWsample$SiteID), col = colours, pch = 16, title = expression(bold("Site ID")), cex=0.9)
# Add vertical line at 52.6
abline(v = 52.6, lty = 2, lwd = 2)

# Add vertical marks for mean age at death for each Site ID
for (i in 1:length(site_data)) {
  mean_age <- mean(site_data[[i]])
  segments(mean_age, i - 0.4, mean_age, i + 0.52, col = "darkred", lwd = 2)
}
#########step 3-------------------------------------------------------------------------------


# Fit a regression model with AgeDiagnosis and Stage3or4 as covariates
model_B <- lm(AgeDeath ~ AgeDiagnosis + HighQualityOfLife, data = CWsample)
summary(model_B)

# Plot the scatter plot
plot(CWsample$AgeDiagnosis, CWsample$AgeDeath, 
     xlab = "Age at Diagnosis (years)", ylab = "Age at Death (years)",
     main = "Regression: Age at Death vs Age at Diagnosis with Race",
     col = ifelse(CWsample$Stage3or4 == "Yes", "red", "blue"))

# Add regression lines for each level of Stage3or4
for(j in 1:2){ 
  abline(a = coef(model_B)[1] + coef(model_B)[3], b = coef(model_B)[2], col = colours[j])
}
# Add the lines for correctly diagnosed patients: 
for(j in 1:2){ 
  abline(a = coef(model_B)[1], b = coef(model_B)[2], col = colours[j], lty = 2)
}
# Add legend
legend("topright", legend = c("HighQualityOfLife ", "Not HighQualityOfLife"), col = colours, lty = c(1, 2), bg = "white")
model_B



#FIGURE 6
par(mfrow = c(1,3))
boxplot(CWsample$AgeDeath ~ CWsample$Sex, cex.main=1.25, cex.lab=1.4, col=c("red", "blue"), main = "Box plots of 'Age at Death' \n by Sex Status", xlab="Sex Status" , ylab="Age at Death (years)")
boxplot(CWsample$AgeDeath ~ CWsample$HighQualityOfLife, cex.main=1.25, cex.lab=1.4, col=c("purple", "seagreen"), main = "Box plots of 'Age at Death' \n by High Quality Of Life Status", xlab="High Quality Of Life Status", ylab="Age at Death (years)")
boxplot(CWsample$AgeDeath ~ CWsample$Stage3or4, cex.main=1.25, cex.lab=1.4, col=c("green", "orange"), main = "Box plots of 'Age at Death' \n by Stage3or4 Status", xlab="Stage3or4 Status ", ylab="Age at Death (years)")

#POTENTIAL COVARIATES
modelAr <- lm(AgeDeath ~ Sex, data = CWsample)
summary(modelAr)
modelAh <- lm(AgeDeath ~ HighQualityOfLife, data = CWsample)
summary(modelAh)
modelAb <- lm(AgeDeath ~ Stage3or4, data = CWsample)
summary(modelAb)

#########step 4-------------------------------------------------------------------------------
#RANDOM INTERCEPT MODEL-----------------------------------------------------------------------

# Fit a mixed-effects model with random intercepts for SiteID
CWsample$SiteID <- as.factor(CWsample$SiteID)

model_C <- lmer(AgeDeath ~ AgeDiagnosis + (1 | SiteID), data = CWsample, REML = FALSE)

# Print the model summary
summary(model_C)
model_C
# Extract variance components
var_random <- VarCorr(model_C)$SiteID[1, 1]  # Variance of random intercept for SiteID
var_residual <- sigma(model_C)^2  # Residual variance

# Calculate ICC
ICC <- var_random / (var_random + var_residual)
ICC



# Extract fixed effects coefficients
b0 <- model_C@beta[1] # Intercept coefficient
b1 <- model_C@beta[2]# Coefficient for AgeDiagnosis

# Extract random intercepts for SiteID
randomeffects <- ranef(model_C)
u <- randomeffects$SiteID$"(Intercept)"
u
par(mfrow=c(1,1))
par(mar = c(4, 4, 4, 1))  # Bottom: 4, Left: 4, Top: 3, Right: 6

# Plot the scatter plot
plot(CWsample$AgeDeath ~ CWsample$AgeDiagnosis,
     col = colours[CWsample$SiteID], 
     pch = 16,
     cex = .8,
     xlab = expression(bold("Age at Diagnosis (years)")), 
     ylab = expression(bold("Age at Death (years)")),
     main = "Scatter Plot of age at diagnosis against age of death as categorised \n by hospital sites")

# Add lines representing random intercepts for each SiteID
for (j in 1:length(u)) {
  abline(a = b0 + u[j], b = b1, col = colours[j], lty = 2, lwd = 1.5)
}

# Add legend
legend("bottomright", legend = levels(as.factor(CWsample$SiteID)), cex=0.85, col = colours, pch = 16, title = expression(bold("SiteID")))




# Calculate log likelihoods for both models
A_log <- logLik(model_A)
B_log <- logLik(model_C)
# Calculate the likelihood ratio test statistic
chi <- 2 * (B_log - A_log)
# Calculate the p-value
p <- 1 - pchisq(chi, df = 1)
#Equivalently
anova(model_C, model_A)








#FIGURE 8
par(mfrow = c(1, 1))
par(mfrow = c(1, 2)) 
randomeffects <- ranef(model_C)
u11 <- randomeffects$SiteID$"(Intercept)"

#Posterior variances:
str(attr(randomeffects$SiteID, "postVar"))
v1 <- attr(randomeffects$SiteID, "postVar")[1, 1, ]
group1 <- rownames(randomeffects$SiteID)

# Create data frame:
level21 <- data.frame(group1, u11, v1)
# Name the columns:
colnames(level21) <- c("Group1", "Residual", "PostVar")
# Order by size of the residuals:
level21 <- level21[order(level21$Residual), ]
# Include a column containing the ranks:
level21 <- cbind(level21, 1:nrow(level21))
qqnorm(resid(model_C), main="Normal QQ plot of level 1 \n residuals in Model C", cex = 0.75)
qqline(resid(model_C), col=c("red"), lwd=2)
qqnorm(level21$Residual, main="Normal QQ plot of level 2 \n residuals in Model C", cex = 0.75)
qqline(level21$Residual, col="purple", lwd=2)






#########step 5-------------------------------------------------------------------------------

#RANDOM SLOPE MODEL
model_D <- lmer(AgeDeath ~ AgeDiagnosis + HighQualityOfLife + (1 + AgeDiagnosis| SiteID), data = CWsample, REML = FALSE)
summary(model_D)



# Extract coefficients and random effects
beta0 <- model_D@beta[1]
beta1 <- model_D@beta[2]
beta2 <- model_D@beta[3]
randomeffects1 <- ranef(model_D)
u0 <- randomeffects1$SiteID[, "(Intercept)"]
u1 <- randomeffects1$SiteID[, "AgeDiagnosis"]


par(mfrow=c(1,1))
par(mar = c(4, 4, 4, 1))  # Bottom: 4, Left: 4, Top: 3, Right: 6

# Scatter plot
plot(CWsample$AgeDeath ~ CWsample$AgeDiagnosis, 
     ylab = "Age at death (years)", 
     xlab = "Age at diagnosis (years)", 
     main = "Scatter Plot of Age at Diagnosis against Age of Death for \n High Quality Of Life,Categorized by Hospital Sites", 
     pch = 16, 
     cex = .8, 
     col = colours[CWsample$SiteID])

# Add legend for SiteID
legend("bottomright", legend = levels(as.factor(CWsample$SiteID)), cex = 0.75, col = colours, pch = 16, title = "SiteID")

# Add legend for Race
legend(30, 95, bty = "n", cex = 1, legend = c("HighQualityOfLife", "Non-HighQualityOfLife"), x.intersp = 0.2, seg.len = 1.5, lty = c(1:2), xpd = TRUE)

# Add regression lines
for (j in 1:12) { 
  abline(a = beta0 + beta2 * 1 + u0[j], b = beta1 + u1[j], col = colours[j], lty = 2)
}
for (j in 1:12) { 
  abline(a = beta0 + u0[j], b = beta1 + u1[j], col = colours[j]) 
}

E_log = logLik(model_D) 
B_log = logLik(model_B)
chi = 2*(E_log - B_log)
p1 = 1 - pchisq(chi, 1)
p2 = 1 - pchisq(chi, 2)
p = (p1 + p2)/2


















#FIGURE 10
randomeffects1$SiteID
str(attr(randomeffects1$SiteID, "postVar"))
attr(randomeffects$SiteID, "postVar")
varu0 <- attr(randomeffects1$SiteID, "postVar")[1,1 , ]
varu1 <- attr(randomeffects1$SiteID, "postVar")[2,2 , ]

group <- rownames(randomeffects1$SiteID)
level2 <- data.frame(group, u0, u1, varu0, varu1)
colnames(level2) <- c("Group", "Intercept Residuals", "'AgeDiagnosis' Residuals", "IntPostVar", "AFKPostVar")
level2



par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
for(i in 1:2){ 
  
  # Order by the rankings of the effects in column i+1:
  
  level2 <- level2[order(level2[ , 1 + i]), ]
  
  # Calculate the credible intervals for the
  # effects in column i+1:
  
  Lower <- level2[ , i + 1] - qnorm(0.975)*sqrt(level2[ , i + 3])
  Upper <- level2[ , i + 1] + qnorm(0.975)*sqrt(level2[ , i + 3]) 
  
  # Plot the effects against the rankings:
  
  plot(1:12, level2[ , i + 1], pch = 20,  
       xlab = "Rank", ylab = "Residual", 
       main = names(level2)[i + 1],
       ylim = c(min(Lower), 1.5*max(Upper)))
  
  # Plot the credible intervals:
  
  segments(1:12, Lower, 1:12, Upper)
  
  abline(h = 0, col = "green", lwd=2)
  
  groupname = paste(level2$Group)
  
  text(x = 1:12 , y = Upper, labels = groupname, srt = 90,  adj = 0, cex = 1)}




#FIGURE 11
par(mfrow = c(1, 1))
# Assessing Normality of Level 2 Residuals
par(mfrow = c(1, 2))
qqnorm(u1, main = "Normal Q-Q Plot for Level 2 \n Residuals in Model D")
qqline(u1, col = "green", lwd = 2)

summary(model_D)$varcor


















































model0 <- lm(AgeDiagnosis ~ 1, data = CWsample)
summary(model0)

CWsample$SiteID <- as.numeric(CWsample$SiteID)
model1 <- lm(SiteID ~ 1, data = CWsample)
summary(model1)

sigmasq <- 7.37**2
sigmasq_u <- 3.50**2
sigmasq_e <- sigmasq - sigmasq_u
sqrt(sigmasq_e)

n <- power.t.test(n = NULL, delta = 2.5, sd = 7.37, power = 0.8)$n

ICC <- sigmasq_u/(sigmasq)
DE <- 1 - ICC

#FIGURE 1

library(dplyr)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
state_tbl <- CWsample %>% group_by(SiteID) %>% 
  summarise(Average_AgeDiagnosis = (round(mean(AgeDiagnosis), digits=2)),
            .groups = 'drop')
kable(t(state_tbl), caption = "Table of average Age at Diagnosis  in each sites") %>%
  kable_styling(full_width = F) %>%
  row_spec(1, bold = TRUE)






#FIGURE 2
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
calc_p <- function(delta,sd,power)
{
  (sqrt(2*sd**2)*(-qnorm(1 - 0.05/2, mean = 0, sd = 1) - qnorm(power/1000, mean = 0, sd = 1))/delta)**2
}

X <- calc_p(delta=2.5, sd=7.37, power=600:999)
power=600:999/1000
par(mfrow = c(1, 2))
plot(X, power, xlab="Number of patients required", ylab="Value of power", main="Scatter plot of required sample size \n compared to specified power", pch=16, col="seagreen")

calc_a <- function(delta,sd,alpha)
{
  (sqrt(2*sd**2)*(-qnorm(1 - alpha/1000, mean = 0, sd = 1) - qnorm(0.8, mean = 0, sd = 1))/delta)**2
}

Y <- calc_a(delta=2.5, sd=7.37, alpha=1:100)
alpha=1:100/1000
plot(Y, alpha, xlab="Number of patients required", ylab="Value of type I error rate", main="Scatter plot of required sample size \n compare to specified type I error rate", pch=16, col="darkblue")

#FIGURE 3

par(mfrow = c(1, 1))
calc_sd <- function(delta,sd,alpha)
{
  (sqrt(2*sd**2)*(-qnorm(1 - alpha, mean = 0, sd = 1) - qnorm(0.8, mean = 0, sd = 1))/delta)**2
}

W <- calc_sd(delta=2.5, sd=1:150/10, alpha=0.025)
sd=1:150/10
plot(W, sd, xlab="Number of patients required", ylab="Value of standard deviation", main="Scatter plot of required sample size compared to standard deviation", pch=16, col="darkred")

































