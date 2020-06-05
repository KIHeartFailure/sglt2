
# Antidiabetic treatments from DDR ----------------------------------------

lmtmp <- left_join(pdata, lm, by = "LopNr")

lmtreats <- function(atc, treatname){
  lmtmp2 <- lmtmp %>%
    mutate(atcneed = stringr::str_detect(ATC, atc),
           diff = as.numeric(EDATUM - shf_indexdtm)) %>%
    filter(atcneed, 
           diff >= - 30.5 * 5, diff <= 14)
  
  treatname <- paste0("ddr_", treatname)
  
  lmtmp2 <- lmtmp2 %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatname := "Yes") %>%
    select(LopNr, !!sym(treatname))
  
  pdata <<- left_join(pdata, 
                      lmtmp2, 
                      by = "LopNr") %>%
    mutate(!!treatname := replace_na(!!sym(treatname), "No"))
  
  metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }
}


lmtreats("A10BK01|A10BK02|A10BK03|A10BK04|A10BK05|A10BD15|A10BD20|A10BD24", "sglt2")

lmtreats("A10A", "insulin")

lmtreats("A10BA", "biguanides")

lmtreats("A10BB|A10BC", "sulfonylureas")

lmtreats("A10BF", "agi")

lmtreats("A10BG", "thiazolidinediones")

lmtreats("A10BH", "glp1a")

lmtreats("A10BJ", "dpp4i")

pdata <- pdata %>%
  mutate_if(is_character, factor) %>%
  mutate(ddr_sglt2num = as.numeric(ddr_sglt2) - 1)

colnames(metalm) <- c("Treat", "ATC")