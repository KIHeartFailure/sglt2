
ProjectTemplate::reload.project()

# Import LM from SoS -----------------------------------------------------

sospath <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/raw-data/SOS/lev3_15875_2019 Lina Benson/"

lm <- read_sas(paste0(sospath, "t_r_lmed__15875_2019.sas7bdat"))

#lm <- zap_formats(lm)
#lm <- zap_label(lm)


# Select ATC codes --------------------------------------------------------

lm <- lm %>%
  mutate(atcneed = stringr::str_detect(ATC, "A10BK01|A10BK02|A10BK03|A10BK04|A10BK05|A10A|A10BA|A10BB|A10BC|A10BF|A10BG|A10BH|A10BJ|A10BD")) %>%
  filter(
    Fall == 1,
    AR >= 2013, 
    AR <= 2018, 
    atcneed
  )

# lm %>% filter(ATC %in% c("A10BK01", "A10BK02", "A10BK03",
#                   "A10BK04", "A10BK05")) %>% count(AR)

# Store as RData in /data folder ------------------------------------------

save(file = "./data/lm.RData", list = c("lm"))
