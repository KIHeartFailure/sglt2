tabvars <- c(
  # demo
  "shf_sex", 
  "shf_age", 
  
  # organizational
  "shf_location", 
  "shf_followuphfunit", "shf_followuplocation",
  
  # clinical factors and lab measurments
  "shf_ef_cat",
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bpsys", 
  "shf_bpdia", 
  "shf_map", 
  "shf_heartrate", 
  "shf_gfrckdepi", 
  "shf_potassium", 
  "shf_hb", 
  "shf_ntpropbnp",
  
  # treatments
  "shf_rasarni", 
  "shf_mra", 
  "shf_digoxin",                  
  "shf_diuretic",
  "shf_nitrate", 
  "shf_asaantiplatelet", 
  "shf_anticoagulantia", 
  "shf_statin", 
  "shf_bbl", 
  "shf_device_cat",
  
  # add other antidiabeteic treats
  "ddr_insulin",
  "ddr_biguanides",
  "ddr_sulfonylureas",
  "ddr_agi",
  "ddr_thiazolidinediones",
  "ddr_glp1a",
  "ddr_dpp4i",
  
   # comorbs
  "shf_smoking", 
  "shf_sos_com_hypertension", 
  "shf_sos_com_ihd",
  "shf_bmi_cat",
  "shf_revasc",
  "sos_com_peripheralartery", 
  "sos_com_stroke", 
  "shf_sos_com_af",
  "shf_ckd",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_pci", 
  "sos_com_cabg",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_copd", 
  
  # socec
  "scb_famtype", 
  "scb_child", 
  "scb_education",
  "scb_dispincome_cat", 
)

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "shf_bpsys", 
  "shf_bpdia",
  "shf_bmi",
  "shf_ckd",
  "shf_anemia",
  "shf_revasc",
  
  "ddr_insulin",
  "ddr_biguanides",
  "ddr_sulfonylureas",
  "ddr_agi",
  "ddr_thiazolidinediones",
  "ddr_glp1a",
  "ddr_dpp4i"
)
 
modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

kontvars <- c("shf_age", "shf_map", "shf_heartrate", "shf_hb", 
              "shf_potassium", "shf_gfrckdepi", "shf_ntpropbnp")

modvarsns <- modvars
modvarsns[modvarsns %in% kontvars] <- 
  paste0("ns(", modvarsns[modvarsns %in% kontvars], " , 3)")

# sens analysis excl NT-proBNP
modvarsns_sens <- modvarsns[modvarsns != "ns(shf_ntpropbnp , 3)"]