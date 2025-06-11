# Load required packages
library(rms)

# Set seed for reproducibility
set.seed(42)

# Simulate data
df_CC <- simulated_rmsMD_data(type = "complete_case")
df_MI <- simulated_rmsMD_data(type = "missing_for_MI")
Imp <- aregImpute(~ age+bmi+sex+smoking+majorcomplication+lengthstay, data = df_MI, n.impute = 5)

dd <- datadist(df_CC)
options(datadist = 'dd')


# complete case ols

fit_olsCC <- ols(lengthstay ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
modelsummary_rms(fit_olsCC)

# complete case lrm
# without setting x and y :
fit_lrmCC <- lrm(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
modelsummary_rms(fit_lrmCC)

# for LR test
fit_lrmCC <- lrm(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC, x=TRUE, y = TRUE)
modelsummary_rms(fit_lrmCC)


######## need to check same with cph --> add time to event to the simulation


#


# Fit an OLS model
ols_fit <- ols(hba1c ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking, data = sim_data, x = TRUE, y = TRUE)
print(ols_fit)
anova(ols_fit)

# Fit a Logistic Regression Model
lrm_fit <- lrm(diabetes ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking, data = sim_data, x = TRUE, y = TRUE)
print(lrm_fit)
anova(lrm_fit)

# Fit a Cox Proportional Hazards Model
cph_fit <- cph(Surv(time, status) ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking, data = sim_data, x = TRUE, y = TRUE)
print(cph_fit)
anova(cph_fit)

# Plot predictions from the logistic model
ggplot(Predict(lrm_fit))

########### ########### ########### ########### ########### ###########
# ########### playing about with LR versus wald etc ###########
########### ########### ########### ########### ########### ###########

# Fit a Logistic Regression Model
lrm_fit <- lrm(diabetes ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking, data = sim_data, x = T, y = T)
print(lrm_fit)
anova(lrm_fit)

anova(lrm_fit, test = "LR")

# nb need x = TRUE, y = TRUE, even if datadist is set

# Fit an OLS model
ols_fit <- ols(hba1c ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking, data = sim_data, x = TRUE, y = TRUE)
print(ols_fit)
anova(ols_fit)
# anova(ols_fit, test = "LR")

# breaks with OLS

#####################################
#### with multiple imputation ########
#####################################

# add in missing data
sim_data_missing <- sim_data  # Copy

for (v in colnames(sim_data_missing)) {
  idx <- sample(1:300, 30)
  sim_data_missing[idx, v] <- NA
}

# aregimpute (uses all variables)
impute_formula <- as.formula(
  paste("~", paste(colnames(sim_data_missing), collapse = " + "))
)

imp <- aregImpute(impute_formula, data = sim_data_missing, n.impute = 5)

fit_MI_ols <- fit.mult.impute(hba1c ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking,
                             ols, imp, data= sim_data_missing)

fit_MI_ols
anova(fit_MI_ols, test = "Chisq")

fit_MI_lrm <- fit.mult.impute(diabetes ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking,
                              lrm, imp, data= sim_data_missing, lrt = TRUE)

anova(fit_MI_lrm)
processMI(fit_MI_lrm, "anova")


fit_MI_cph <- fit.mult.impute(Surv(time, status) ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking,
                              cph, imp, data = sim_data_missing)

anova(fit_MI_cph, india = FALSE, indnl = FALSE)
processMI(fit_MI_cph, "anova", india = FALSE, indnl = FALSE)




# note

class(fit_MI_ols)

class(fit_MI_lrm)

class(fit_MI_cph)



fit_lrt_TRUE <- fit.mult.impute(diabetes ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking,
                              lrm, imp, data= sim_data_missing, lrt = TRUE)
str(fit_lrt_TRUE)


fit_lrt_FALSE <- fit.mult.impute(diabetes ~ rcs(age, 3) + rcs(bmi, 4) + sex + smoking,
                                lrm, imp, data= sim_data_missing, lrt = FALSE)

str(fit_lrt_FALSE)


