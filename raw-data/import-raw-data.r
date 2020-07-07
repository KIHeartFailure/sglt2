
ProjectTemplate::reload.project()

# Import LM from SoS -----------------------------------------------------

sospath <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/raw-data/SOS/lev3_15875_2019 Lina Benson/"

lm <- read_sas(paste0(sospath, "t_r_lmed__15875_2019.sas7bdat"))
lm <- zap_formats(lm)
lm <- zap_label(lm)

# Select ATC codes --------------------------------------------------------

lm <- lm %>%
  mutate(atcneed = stringr::str_detect(ATC, "^A10")) %>%
  filter(
    Fall == 1,
    ANTAL >= 0,
    #AR >= 2013, 
    #AR <= 2018, 
    atcneed
  )

kollatc <- lm %>% count(ATC)

write.xlsx(kollatc, paste0("./checks/checkATC_", Sys.Date(), ".xlsx"), rowNames = FALSE)

# Store as RData in /data folder ------------------------------------------

save(file = "./data/lm.RData", list = c("lm"))

# Patient registry from SHFDB3 v 3.1.2, prepared in 08-prep_sosdata.R -----

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/patreg.RData")

patreg <- patreg %>% 
  filter(sos_source == "sv")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/patreg.RData", list = c("patreg"))