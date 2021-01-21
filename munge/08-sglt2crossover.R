

# Define sglt2 crossover from DDR -------------------------------------------

matchplm <- left_join(matchp,
  lm,
  by = "LopNr"
) %>%
  mutate(tmp_sglt2 = stringr::str_detect(
    ATC,
    "^(A10BK0[1-6]|A10BD15|A10BD16|A10BD19|A10BD20|A10BD21|A10BD23|A10BD24|A10BD25|A10BX09|A10BX11|A10BX12)"
  )) %>%
  filter(
    EDATUM <= ymd("2018-12-31"),
    EDATUM >= shf_indexdtm,
    tmp_sglt2
  )

# assume that if you are on you are on until LAST dispension + 3 mo OR 2018-08-01
# (last follow-up - 5 mo) OR death - 5 mo
# independent of how long time between dispensions
# although sort of crude, is probably the option that will reflect reality
# the best for the majority of patients.

crossoverfunc <- function(time, event) {
  time <- sym(time)
  event <- sym(event)

  matchplm2 <- matchplm %>%
    filter(EDATUM <= shf_indexdtm + !!time) %>%
    group_by(LopNr) %>%
    arrange(EDATUM) %>%
    slice(c(1, n())) %>%
    mutate(firstlast = ifelse(row_number() == 1, "firstdtm", "lastdtm")) %>%
    ungroup()

  matchplm3 <- left_join(matchp,
    matchplm2 %>% select(LopNr, firstlast, EDATUM),
    by = "LopNr"
  ) %>%
    mutate(
      enddtm = shf_indexdtm + !!time,
      crossoverdtm = case_when(
        ddr_sglt2 == "Yes" & firstlast == "lastdtm" & EDATUM <= enddtm - 5 * 30 ~ EDATUM + 3 * 30,
        ddr_sglt2 == "No" & firstlast == "firstdtm" ~ EDATUM,
        ddr_sglt2 == "No" & firstlast == "lastdtm" & EDATUM <= enddtm - 5 * 30 ~ EDATUM + 3 * 30
      ),
      crossover_ddr_sglt2 = case_when(
        ddr_sglt2 == "Yes" ~ "No",
        ddr_sglt2 == "No" & firstlast == "firstdtm" ~ "Yes",
        ddr_sglt2 == "No" & firstlast == "lastdtm" ~ "No"
      ),
      crossover = 1
    ) %>%
    filter(!is.na(crossoverdtm)) %>%
    select(-ddr_sglt2, -enddtm, -EDATUM, -firstlast) %>%
    rename(ddr_sglt2 = crossover_ddr_sglt2)

  matchpcrossover <- bind_rows(
    matchp,
    matchplm3
  ) %>%
    mutate(dtmuse = coalesce(crossoverdtm, shf_indexdtm)) %>%
    group_by(LopNr) %>%
    arrange(dtmuse) %>%
    mutate(
      start = case_when(
        row_number() == 1 ~ 0,
        TRUE ~ as.numeric(dtmuse - shf_indexdtm)
      ),
      stop = case_when(
        row_number() == n() ~ !!time,
        TRUE ~ lead(start)
      ),
      !!event := case_when(
        row_number() == n() ~ as.character(!!event),
        TRUE ~ "No"
      )
    ) %>%
    ungroup() %>%
    arrange(LopNr, dtmuse) %>%
    select(LopNr, shf_indexdtm, ddr_sglt2, start, stop, !!event, par)
  return(matchpcrossover)
}

matchpcross_deathcvhosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_deathcvhosphf"
)
matchpcross_deathhosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_deathhosphf"
)
matchpcross_death <- crossoverfunc(
  time = "sos_outtime_death",
  event = "sos_out_death"
)
matchpcross_deathcv <- crossoverfunc(
  time = "sos_outtime_death",
  event = "sos_out_deathcv"
)
matchpcross_hosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_hosphf"
)
matchpcross_hospcv <- crossoverfunc(
  time = "sos_outtime_hospcv",
  event = "sos_out_hospcv"
)
matchpcross_deathcvhospmistroke <- crossoverfunc(
  time = "sos_outtime_hospmistroke",
  event = "sos_out_deathcvhospmistroke"
)
matchpcross_hospbleed <- crossoverfunc(
  time = "sos_outtime_hospbleed",
  event = "sos_out_hospbleed"
)