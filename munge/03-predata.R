
# Creating variables needed for project PRIOR to imputation ---------------

pdata <- pdata %>%
  mutate(
    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 1,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 3
    ),
    labels = c("pEF", "mrEF", "rEF")
    ),

    shf_indexyear_cat = case_when(
      shf_indexyear <= 2005 ~ "2000-2005",
      shf_indexyear <= 2010 ~ "2006-2010",
      shf_indexyear <= 2015 ~ "2011-2015",
      shf_indexyear <= 2018 ~ "2016-2018"
    ),
    
    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_character_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No/Pacemaker", "CRT/ICD"),
    levels = 1:2),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_ckd = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi >= 60 ~ 1,
      shf_gfrckdepi >= 30 ~ 2,
      shf_gfrckdepi < 30 ~ 3,
    ),
    labels = c(">=60", "30-60", "<30"),
    levels = 1:3
    ),

    shf_sos_com_af = case_when(sos_com_af == "Yes" |
      shf_af == "Yes" |
      shf_ekg == "Atrial fibrillation" ~ "Yes", 
      TRUE ~ "No"),

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
    sos_out_deathcvhosphf = case_when(sos_out_deathcv == "Yes" | 
                                        sos_out_hosphf == "Yes" ~ "Yes", 
                                      TRUE ~"No"),
    sos_out_deathcvhospmistroketia = case_when(sos_out_deathcv == "Yes" | 
                                              sos_out_hospmi == "Yes" |
                                              sos_out_hospstroketia == "Yes" 
                                              ~ "Yes", 
                                      TRUE ~"No"), 
    sos_outtime_hospmistroketia = pmin(sos_outtime_hospmi, sos_outtime_hospstroketia),
    
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
    sos_out_deathcvhospmistroketia_comp = case_when(
      sos_out_deathcvhospmistroketia == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    )
  ) %>%
  mutate_if(is_character, factor)
