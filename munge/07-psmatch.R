

# Propensity scores -------------------------------------------------------

ps <- as_tibble(matrix(NA, nrow = nrow(pdata), ncol = 11), .name_repair = "universal")

for (i in 1:10) {
  impdata_ps <- mice::complete(imp, i)
  if (i == 1) ps[, 1] <- impdata_ps$LopNr
  pslog <- glm(formula(paste0(
    "ddr_sglt2num ~ ",
    paste(modvars,
      collapse = " + "
    )
  )),
  data = impdata_ps,
  family = binomial
  )
  ps[, i + 1] <- pslog$fitted
}

pdata <- left_join(pdata,
  ps %>%
    mutate(ps = rowSums(.[2:11]) / 10) %>%
    select(...1, ps),
  by = c("LopNr" = "...1")
)

cal <- c(0.01 / sd(pdata$ps))

set.seed(2334325)
match1 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)
set.seed(2334325)
match2 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 2
)
set.seed(2334325)
match3 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 3
)
set.seed(2334325)
match4 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 4
)
set.seed(2334325)
match5 <- Match(
  Tr = pdata$ddr_sglt2num,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 5
)
matchingn <- paste0(
  "1:1: N = ", match1$wnobs, ", ",
  "1:2: N = ", match2$wnobs, ", ",
  "1:3: N = ", match3$wnobs, ", ",
  "1:4: N = ", match4$wnobs, ", ",
  "1:5: N = ", match5$wnobs
)

pdata$par <- rep(NA, nrow(pdata))

pdata$par[c(unique(match3$index.treated), match3$index.control)] <- c(1:match3$wnobs, rep(1:match3$wnobs, each = 3))
matchp <- pdata[c(unique(match3$index.treated), match3$index.control), ]
