
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 6)


# Cox regression ----------------------------------------------------------

mod <- coxph(formula(paste0(
  "Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == 'Yes') ~ ddr_sglt2 +",
  paste(modvars, collapse = " + "))), data = dataass)


# Checking for non-prop hazards -------------------------------------------

print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

# check spec for sglt2, ok
survminer::ggcoxzph(testpat[1]) 
plot(testpat[1])

# check for vars with most sig in most imps
survminer::ggcoxzph(testpat[4])
plot(testpat[4])
plot(testpat[13])
plot(testpat[14])

# Checking for outliers ---------------------------------------------------

survminer::ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)


# Checking for linearity ---------------------------------------------------

#ggcoxfunctional(Surv(time_out_hf_hosp, out_death_hfhosp == "yes") ~ age + bp.sys + heartRate, data = dataass)
# No continous variables

# Logistic regression -----------------------------------------------------
modlm <- glm(formula(paste0("ddr_sglt2 == 'Yes' ~ ", paste(modvars, collapse = " + "))),
  family = binomial(link = "logit"), data = dataass
)


# Linearity for continous variables ---------------------------------------

# No continous variables

#probabilities <- predict(modlm, type = "response")
#predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

#contdata <- dataass %>%
#  select(age, bp.sys, heartRate)

# Bind the logit and tidying the data for plot
#contdata <- contdata %>%
#  mutate(logit = log(probabilities / (1 - probabilities))) %>%
#  gather(key = "predictors", value = "predictor.value", -logit)

#ggplot(contdata, aes(logit, predictor.value)) +
#  geom_point(size = 0.5, alpha = 0.5) +
#  geom_smooth(method = "loess") +
#  theme_bw() +
#  facet_wrap(~predictors, scales = "free_y")


# Outliers ---------------------------------------------------------------

plot(modlm, which = 4, id.n = 3)


# Multicollinearity -------------------------------------------------------

car::vif(modlm)
