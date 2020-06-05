

# Propensity scores -------------------------------------------------------

ps <- as_tibble(matrix(NA, nrow = nrow(pdata), ncol = 11), .name_repair = "universal")

for (i in 1:10) {
  impdata_ps <- mice::complete(imp, i)
  if (i == 1) ps[, 1] <- impdata_ps$LopNr
  pslog <- glm(formula(paste0("ddr_sglt2num ~ ", 
                              paste(modvarsns, 
                                    collapse = " + "))),
               data = impdata_ps,
               family = binomial
  )
  ps[, i + 1] <- pslog$fitted
}

pdata <- left_join(pdata, 
                   ps %>%
                     mutate(ps = rowSums(.[2:11]) / 10) %>%
                     select(...1, ps), 
                   by = c("LopNr" = "...1"))

cal <- c(0.01 / sd(pdata$ps))
set.seed(2334325)

match <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 2
)
match1 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)
match3 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 3
)

matchingn <- paste0("1:1: N = ", match1$wnobs, ", ",
                   "1:2: N = ", match$wnobs, ", ",
                   "1:3: N = ", match3$wnobs
)

pdata$par <- rep(NA, nrow(pdata))

pdata$par[c(unique(match$index.treated), match$index.control)] <- c(1:match$wnobs, rep(1:match$wnobs, each = 2))
matchp <- pdata[c(unique(match$index.treated), match$index.control), ]


# Sensitivity ps analysis exckl NT-proBNP ----------------------------------

ps_sens <- as_tibble(matrix(NA, nrow = nrow(pdata), ncol = 11), .name_repair = "universal")

for (i in 1:10) {
  impdata_ps <- mice::complete(imp_sens, i)
  if (i == 1) ps_sens[, 1] <- impdata_ps$LopNr
  pslog <- glm(formula(paste0("ddr_sglt2num ~ ", 
                              paste(modvarsns_sens, 
                                    collapse = " + "))),
               data = impdata_ps,
               family = binomial
  )
  ps_sens[, i + 1] <- pslog$fitted
}

pdata <- left_join(pdata, 
                   ps_sens %>%
                     mutate(ps_sens = rowSums(.[2:11]) / 10) %>%
                     select(...1, ps_sens), 
                   by = c("LopNr" = "...1"))

cal <- c(0.01 / sd(pdata$ps_sens))
set.seed(2334325)

match <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps_sens,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 2
)

match$wnobs

pdata$par_sens <- rep(NA, nrow(pdata))

pdata$par_sens[c(unique(match$index.treated), match$index.control)] <- c(1:match$wnobs, rep(1:match$wnobs, each = 2))
matchp_sens <- pdata[c(unique(match$index.treated), match$index.control), ]