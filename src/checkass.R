
ProjectTemplate::reload.project()

# variables that should be presented in tables and included in models
source("./src/vars_for_tabs_mods.R")

dataass <- mice::complete(imp, 7)


# Cox regression ----------------------------------------------------------


mod <- coxph(formula(paste0(
  "Surv(timeTodeath, death == 'yes') ~ clinic +", paste(modvars, collapse = " + ")
)), data = dataass)


# Checking for non-prop hazards -------------------------------------------

print(testpat <- cox.zph(mod))
(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

# check spec for clinic, ok
ggcoxzph(testpat[1]) 

# check for vars with most sig in most imps
ggcoxzph(testpat[3])
ggcoxzph(testpat[4])
ggcoxzph(testpat[16])
ggcoxzph(testpat[36])

# Checking for outliers ---------------------------------------------------

ggcoxdiagnostics(mod,
  type = "dfbeta",
  linear.predictions = FALSE, ggtheme = theme_bw()
)

ggcoxdiagnostics(mod, type = , linear.predictions = TRUE)


# Checking for linearity ---------------------------------------------------

#ggcoxfunctional(Surv(time_out_hf_hosp, out_death_hfhosp == "yes") ~ age + bp.sys + heartRate, data = dataass)
# No continous variables

# Logistic regression -----------------------------------------------------
modlm <- glm(formula(paste0("clinic == 'medicine' ~ ", paste(modvars, collapse = " + "))),
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
