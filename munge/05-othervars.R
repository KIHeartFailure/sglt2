

# Additional variables from mainly SHF ------------------------------------

pdata <- pdata %>%
  mutate(
    
    shf_quarter = quarter(shf_indexdtm), 
    shf_indexyearquarter = paste0(shf_indexyear, ":", shf_quarter),
      
    shf_age_cat = case_when(
      shf_age < 75 ~ "<75",
      shf_age >= 75 ~ ">=75"
    ),

    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 1,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 3
    ),
    labels = c("pEF", "mrEF", "rEF"),
    levels = 1:3
    ),

    shf_nyha_cat = case_when(
      shf_nyha %in% c("I", "II") ~ "I-II",
      shf_nyha %in% c("III", "IV") ~ "III-IV"
    ),

    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Former", "Never") ~ 1,
      shf_smoking == "Current" ~ 2
    ),
    labels = c("Former/Never", "Current"),
    levels = 1:2
    ),

    shf_map_cat = case_when(
      shf_map <= 90 ~ "<=90",
      shf_map > 90 ~ ">90"
    ),

    shf_potassium_cat = factor(
      case_when(
        is.na(shf_potassium) ~ NA_real_,
        shf_potassium < 3.5 ~ 2,
        shf_potassium <= 5 ~ 1,
        shf_potassium > 5 ~ 3
      ),
      labels = c("normakalemia", "hypokalemia", "hyperkalemia"),
      levels = 1:3
    ),

    shf_heartrate_cat = case_when(
      shf_heartrate <= 70 ~ "<=70",
      shf_heartrate > 70 ~ ">70"
    ),

    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No/Pacemaker", "CRT/ICD"),
    levels = 1:2
    ),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_ckd = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi >= 60 ~ 2,
      shf_gfrckdepi >= 30 ~ 1,
      shf_gfrckdepi < 30 ~ 3,
    ),
    labels = c("30-60", ">=60", "<30"),
    levels = 1:3
    ),

    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    # Anemia
    shf_anemia = case_when(
      is.na(shf_hb) ~ NA_character_,
      shf_sex == "Female" & shf_hb < 120 | shf_sex == "Male" & shf_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    # Outcomes
    sos_out_deathcvhosphf = case_when(
      sos_out_deathcv == "Yes" |
        sos_out_hosphf == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    sos_out_deathhosphf = case_when(
      sos_out_death == "Yes" |
        sos_out_hosphf == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    sos_out_deathcvhospmistroke = case_when(
      sos_out_deathcv == "Yes" |
        sos_out_hospmi == "Yes" |
        sos_out_hospstroke == "Yes"
      ~ "Yes",
      TRUE ~ "No"
    ),
    sos_outtime_hospmistroke = pmin(sos_outtime_hospmi, sos_outtime_hospstroke),

    # comp event outcomes
    sos_out_deathcvhosphf_comp = case_when(
      sos_out_deathcvhosphf == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_hosphf_comp = case_when(
      sos_out_hosphf == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_deathcv_comp = case_when(
      sos_out_deathcv == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_hospcv_comp = case_when(
      sos_out_hospcv == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_deathcvhospmistroke_comp = case_when(
      sos_out_deathcvhospmistroke == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    ),
    sos_out_hospbleed_comp = case_when(
      sos_out_hospbleed == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    )
  )

# outcomes at 30, 60, 90, 365 days
cutout <- function(event, time, timepoint) {
  time2 <- paste0(time, "_", timepoint)
  event2 <- paste0(event, "_", timepoint)

  pdata <<- pdata %>%
    mutate(
      !!time2 := pmin(!!sym(time), timepoint),
      !!event2 := ifelse(!!sym(time2) == !!sym(time), as.character(!!sym(event)), "No")
    )
}

cutout("sos_out_death", "sos_outtime_death", timepoint = 30)
cutout("sos_out_death", "sos_outtime_death", timepoint = 60)
cutout("sos_out_death", "sos_outtime_death", timepoint = 90)
cutout("sos_out_death", "sos_outtime_death", timepoint = 365)

cutout("sos_out_deathcv", "sos_outtime_death", timepoint = 30)
cutout("sos_out_deathcv", "sos_outtime_death", timepoint = 60)
cutout("sos_out_deathcv", "sos_outtime_death", timepoint = 90)
cutout("sos_out_deathcv", "sos_outtime_death", timepoint = 365)

cutout("sos_out_deathcvhosphf", "sos_outtime_hosphf", timepoint = 30)
cutout("sos_out_deathcvhosphf", "sos_outtime_hosphf", timepoint = 60)
cutout("sos_out_deathcvhosphf", "sos_outtime_hosphf", timepoint = 90)
cutout("sos_out_deathcvhosphf", "sos_outtime_hosphf", timepoint = 365)

cutout("sos_out_deathhosphf", "sos_outtime_hosphf", timepoint = 30)
cutout("sos_out_deathhosphf", "sos_outtime_hosphf", timepoint = 60)
cutout("sos_out_deathhosphf", "sos_outtime_hosphf", timepoint = 90)
cutout("sos_out_deathhosphf", "sos_outtime_hosphf", timepoint = 365)

cutout("sos_out_hosphf", "sos_outtime_hosphf", timepoint = 30)
cutout("sos_out_hosphf", "sos_outtime_hosphf", timepoint = 60)
cutout("sos_out_hosphf", "sos_outtime_hosphf", timepoint = 90)
cutout("sos_out_hosphf", "sos_outtime_hosphf", timepoint = 365)

cutout("sos_out_hospcv", "sos_outtime_hospcv", timepoint = 30)
cutout("sos_out_hospcv", "sos_outtime_hospcv", timepoint = 60)
cutout("sos_out_hospcv", "sos_outtime_hospcv", timepoint = 90)
cutout("sos_out_hospcv", "sos_outtime_hospcv", timepoint = 365)

cutout("sos_out_deathcvhospmistroke", "sos_outtime_hospmistroke", timepoint = 30)
cutout("sos_out_deathcvhospmistroke", "sos_outtime_hospmistroke", timepoint = 60)
cutout("sos_out_deathcvhospmistroke", "sos_outtime_hospmistroke", timepoint = 90)
cutout("sos_out_deathcvhospmistroke", "sos_outtime_hospmistroke", timepoint = 365)

cutout("sos_out_hospbleed", "sos_outtime_hospbleed", timepoint = 30)
cutout("sos_out_hospbleed", "sos_outtime_hospbleed", timepoint = 60)
cutout("sos_out_hospbleed", "sos_outtime_hospbleed", timepoint = 90)
cutout("sos_out_hospbleed", "sos_outtime_hospbleed", timepoint = 365)

# income

inc <- pdata %>%
  group_by(shf_indexyear) %>%
  summarise(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.5),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(incsum)) %>%
  spread(name, value)

pdata <- left_join(
  pdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < `50%` ~ 1,
      scb_dispincome >= `50%` ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-`50%`)

# ntprobnp

ntprobnp <- pdata %>%
  group_by(shf_ef_cat, shf_indexyear) %>%
  summarise(ntsum = list(enframe(quantile(shf_ntpropbnp,
    probs = c(0.5),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(ntsum)) %>%
  spread(name, value)

pdata <- left_join(
  pdata,
  ntprobnp,
  by = c("shf_ef_cat", "shf_indexyear")
) %>%
  mutate(
    shf_ntpropbnp_cat = case_when(
      shf_ntpropbnp < `50%` ~ 1,
      shf_ntpropbnp >= `50%` ~ 2
    ),
    shf_ntpropbnp_cat = factor(shf_ntpropbnp_cat,
      levels = 1:2,
      labels = c("Below medium by EF", "Above medium by EF")
    )
  ) %>%
  select(-`50%`)

pdata <- pdata %>%
  mutate_if(is_character, factor)
