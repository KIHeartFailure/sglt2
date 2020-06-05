

# Define sglt2 crossover from DDR -------------------------------------------

matchplm <- left_join(matchp,
  lm,
  by = "LopNr"
) %>%
  filter(
    ANTAL > 0,
    EDATUM <= ymd("2018-12-31"),
    EDATUM >= shf_indexdtm
  ) %>%
  select(LopNr, ddr_sglt2, EDATUM)

# Censor for sglt2 No

matchplmNo <- matchplm %>%
  filter(ddr_sglt2 == "No") %>%
  group_by(LopNr) %>%
  arrange(EDATUM) %>%
  slice(1) %>%
  ungroup()

# Censor sglt2 yes, if > 6 mo between dispensed drugs

matchplmYes1 <- matchplm %>%
  filter(ddr_sglt2 == "Yes") %>%
  group_by(LopNr) %>%
  arrange(EDATUM) %>%
  mutate(diffmed = lead(EDATUM) - EDATUM) %>%
  ungroup() %>%
  arrange(LopNr, EDATUM) %>%
  filter(diffmed >= 30.5 * 6) %>%
  group_by(LopNr) %>%
  arrange(EDATUM) %>%
  slice(1) %>%
  ungroup()

# Censor sglt2 yes, check last disp (so will censor at last + 3 mo)

matchplmYes2 <- matchplm %>%
  filter(ddr_sglt2 == "Yes") %>%
  group_by(LopNr) %>%
  arrange(EDATUM) %>%
  slice(n()) %>%
  ungroup()

matchplmYes <- bind_rows(matchplmYes1, matchplmYes2) %>%
  group_by(LopNr) %>%
  arrange(EDATUM) %>%
  slice(1) %>%
  ungroup()

matchplmBoth <- bind_rows(matchplmNo, matchplmYes)

matchplm2 <- left_join(matchp, matchplmBoth %>% select(-ddr_sglt2), by = "LopNr")

matchp <- matchplm2 %>%
  mutate(
    sglt2censdate = case_when(
      ddr_sglt2 == "No" ~ EDATUM,
      ddr_sglt2 == "Yes" &
        EDATUM <= shf_indexdtm + sos_outtime_death - 31 * 6 ~
      EDATUM + 31 * 3,
      ddr_sglt2 == "Yes" & is.na(EDATUM) ~ shf_indexdtm + 31 * 3,
    ),
    sglt2censtime = as.numeric(sglt2censdate - shf_indexdtm),


    sos_outtime_death_sglt2 = pmin(sos_outtime_death, sglt2censtime, na.rm = TRUE),
    sos_out_death_sglt2 = case_when(
      sos_outtime_death_sglt2 < sos_outtime_death ~ "No",
      TRUE ~ as.character(sos_out_death)
    ),
    sos_out_deathcv_sglt2 = case_when(
      sos_outtime_death_sglt2 < sos_outtime_death ~ "No",
      TRUE ~ as.character(sos_out_deathcv)
    ),


    sos_outtime_hosphf_sglt2 = pmin(sos_outtime_hosphf, sglt2censtime, na.rm = TRUE),
    sos_out_deathcvhosphf_sglt2 = case_when(
      sos_outtime_hosphf_sglt2 < sos_outtime_hosphf ~ "No",
      TRUE ~ as.character(sos_out_deathcvhosphf)
    ),
    sos_out_hosphf_sglt2 = case_when(
      sos_outtime_hosphf_sglt2 < sos_outtime_hosphf ~ "No",
      TRUE ~ as.character(sos_out_hosphf)
    ),


    sos_outtime_hospcv_sglt2 = pmin(sos_outtime_hospcv, sglt2censtime, na.rm = TRUE),
    sos_out_hospcv_sglt2 = case_when(
      sos_outtime_hospcv_sglt2 < sos_outtime_hospcv ~ "No",
      TRUE ~ as.character(sos_out_hospcv)
    ),

    sos_outtime_hospmistroketia_sglt2 = pmin(sos_outtime_hospmistroketia, sglt2censtime, na.rm = TRUE),
    sos_out_deathcvhospmistroketia_sglt2 = case_when(
      sos_outtime_hospmistroketia_sglt2 < sos_outtime_hospmistroketia ~ "No",
      TRUE ~ as.character(sos_out_deathcvhospmistroketia)
    )
  ) %>%
  select(-EDATUM, -diffmed, -sglt2censtime, sglt2censdate)
