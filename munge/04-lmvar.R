
# Antidiabetic treatments from DDR ----------------------------------------

lmtmp <- left_join(pdata %>% select(LopNr, shf_indexdtm),
  lm,
  by = "LopNr"
)

lmtreats <- function(atc, treatname) {
  lmtmp2 <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc),
      diff = as.numeric(EDATUM - shf_indexdtm)
    ) %>%
    filter(
      atcneed,
      diff >= -30.5 * 5, diff <= 14
    )

  treatname <- paste0("ddr_", treatname)

  lmtmp2 <- lmtmp2 %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatname := "Yes") %>%
    select(LopNr, !!sym(treatname))

  pdata <<- left_join(pdata,
    lmtmp2,
    by = "LopNr"
  ) %>%
    mutate(!!treatname := replace_na(!!sym(treatname), "No"))

  metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }
}


lmtreats("^(A10BK0[1-6]|A10BD15|A10BD16|A10BD19|A10BD20|A10BD21|A10BD23|A10BD24|A10BD25|A10BX09|A10BX11|A10BX12)", "sglt2")

lmtreats("^(A10BK01|A10BD15|A10BD21|A10BD25)", "sglt2_Dapagliflozin")
lmtreats("^(A10BK02|A10BD16)", "sglt2_Canagliflozin")
lmtreats("^(A10BK03|A10BD19|A10BD20)", "sglt2_Empagliflozin")
lmtreats("^(A10BK04|A10BD23|A10BD24)", "sglt2_Ertugliflozin")
lmtreats("^A10BK05", "sglt2_Ipragliflozin")
lmtreats("^A10BK06", "sglt2_Sotagliflozin")

lmtreats("^A10A", "insulin")

lmtreats("^(A10BA|A10BD02|A10BD03|A10BD05|A10BD07|A10BD08|A10BD10|A10BD11|A10BD13|A10BD15|A10BD16|A10BD20|A10BD23|A10BD25)", "metformin")
# lmtreats("^(A10BA01|A10BA03)", "checknotmetformin") biguanides = metformin

lmtreats("^(A10BB|A10BC|A10BD02|A10BD04|A10BD06)", "sulfonylureas")

lmtreats("^A10BF", "agi")

lmtreats("^(A10BG|A10BD03|A10BD04|A10BD05|A10BD06|A10BD09)", "thiazolidinediones")

lmtreats("^(A10BJ|A10BX04|A10BX07|A10BX10|A10BX13|A10BX14)", "glp1a")

lmtreats("^(A10BH|A10BD07|A10BD08|A10BD09|A10BD10|A10BD11|A10BD13|A10BD19|A10BD21|A10BD24|A10BD25)", "dpp4i")

pdata <- pdata %>%
  mutate_if(is_character, factor) %>%
  mutate(ddr_sglt2num = as.numeric(ddr_sglt2) - 1)

colnames(metalm) <- c("Variable", "ATC")
metalm <- metalm %>%
  as_tibble() %>%
  mutate(
    ATC = gsub("^", "", ATC, fixed = TRUE),
    ATC = gsub("(", "", ATC, fixed = TRUE),
    ATC = gsub(")", "", ATC, fixed = TRUE),
    ATC = gsub("?!", " excl.", ATC, fixed = TRUE),
    Registry = "Dispensed Drug Registry",
    Period = "-5mo-14days",
  )


# SGLT2 at end of study ---------------------------------------------------

lmtmp2 <- lmtmp %>%
  mutate(
    atcneed = stringr::str_detect(ATC, "^(A10BK0[1-6]|A10BD15|A10BD16|A10BD19|A10BD20|A10BD21|A10BD23|A10BD24|A10BD25|A10BX09|A10BX11|A10BX12)"),
    diff = as.numeric(EDATUM - ymd("2018-12-31"))
  ) %>%
  filter(
    atcneed,
    diff >= -30.5 * 5, diff <= 0
  ) %>%
  group_by(LopNr) %>%
  arrange(EDATUM) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(ddr_sglt2_end = case_when(
    stringr::str_detect(ATC, "^(A10BK01|A10BD15|A10BD21|A10BD25)") ~ "Dapagliflozin",
    stringr::str_detect(ATC, "^(A10BK02|A10BD16)") ~ "Canagliflozin",
    stringr::str_detect(ATC, "^(A10BK03|A10BD19|A10BD20)") ~ "Empagliflozin",
    stringr::str_detect(ATC, "^(A10BK04|A10BD23|A10BD24)") ~ "Ertugliflozin",
    stringr::str_detect(ATC, "^A10BK05") ~ "Ipragliflozin",
    stringr::str_detect(ATC, "^A10BK06") ~ "Sotagliflozin",
    TRUE ~ "Other"
  )) %>%
  select(LopNr, ddr_sglt2_end)


pdata <- left_join(pdata,
  lmtmp2,
  by = "LopNr"
) %>%
  mutate(
    ddr_sglt2_end = replace_na(ddr_sglt2_end, "No"),
    available_at_end_study = case_when(
      shf_indexdtm + sos_outtime_death >= ymd("2018-12-31") ~ "Yes",
      TRUE ~ "No"
    )
  )

# Overtime graph ----------------------------------------------------------

overtime <- lmtmp %>%
  mutate(atcneed = stringr::str_detect(ATC, "^(A10BK0[1-6]|A10BD15|A10BD16|A10BD19|A10BD20|A10BD21|A10BD23|A10BD24|A10BD25|A10BX09|A10BX11|A10BX12)")) %>%
  filter(atcneed) %>%
  group_by(LopNr, AR) %>%
  slice(1) %>%
  ungroup() %>%
  count(AR)
