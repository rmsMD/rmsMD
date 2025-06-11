# Load required packages
library(rms)

# Set seed for reproducibility
set.seed(42)

# Simulate data
df_CC <- simulated_rmsMD_data(type = "complete_case")

dd <- datadist(df_CC)
options(datadist = 'dd')

# nb all of these have rcs terms, need to make sure basic function without rcs terms works well


# complete case ols

fit_olsCC <- ols(lengthstay ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
modelsummary_rms(fit_olsCC)

# complete case lrm
# without setting x and y :
fit_lrmCC <- lrm(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
modelsummary_rms(fit_lrmCC)

# complete case lrm for LR test
fit_lrmCC <- lrm(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC, x=TRUE, y = TRUE)
modelsummary_rms(fit_lrmCC)

# complete case cph
fit_lrmCC <- cph(Surv(time,event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC)
modelsummary_rms(fit_lrmCC)

# CC cph for LR
fit_lrmCC <- cph(Surv(time,event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking, df_CC, x = TRUE, y= TRUE)
modelsummary_rms(fit_lrmCC)


#######################################
############# multi imp #############
############# ############# #############

df_MI <- simulated_rmsMD_data(type = "missing_for_MI")
imp <- aregImpute(~ age+bmi+sex+smoking+majorcomplication+lengthstay, data = df_MI, n.impute = 5)

dd <- datadist(df_MI)
options(datadist = 'dd')

# ols
fit_MI_ols <- fit.mult.impute(lengthstay ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              ols, imp, data= df_MI)

modelsummary_rms(fit_MI_ols)

# lrm with wald default
fit_MI_lrm <- fit.mult.impute(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              lrm, imp, data= df_MI)

modelsummary_rms(fit_MI_lrm)

# cph with wald default
fit_MI_cph <- fit.mult.impute(Surv(time,event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              cph, imp, data= df_MI)

modelsummary_rms(fit_MI_cph)

##############################################################################
############# multi imp using lrt = T to get LR test rather than wald#############
##############################################################################

# trying it with non- MI models, or without setting lrt to check error messages

modelsummary_rms(fit_MI_ols, MI_lrt = TRUE)
modelsummary_rms(fit_lrmCC, MI_lrt = TRUE)
modelsummary_rms(fit_MI_lrm, MI_lrt = TRUE)

# lrm with wald default
fit_MI_lrm_lrt <- fit.mult.impute(majorcomplication ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              lrm, imp, data= df_MI, lrt = TRUE)

modelsummary_rms(fit_MI_lrm_lrt, MI_lrt = TRUE)

# cph with wald default
fit_MI_cph_lrt <- fit.mult.impute(Surv(time,event) ~ rcs(age,4) + rcs(bmi,3) + sex + smoking,
                              cph, imp, data= df_MI, lrt = TRUE)

modelsummary_rms(fit_MI_cph_lrt, MI_lrt = TRUE)
