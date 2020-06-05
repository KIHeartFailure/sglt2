

# Inclusion/exclusion criteria --------------------------------------------------------

pdata <- rsdata312 %>%
  filter(casecontrol == "Case")

flow <- c("Number of posts (cases) in SHFDB3", nrow(pdata))

pdata <- pdata %>%
  filter(shf_diabetestype == "Type II")
flow <- rbind(flow, c("Diabetes type II", nrow(pdata)))

pdata <- pdata %>%
  filter(shf_indexdtm >= ymd("2016-01-01"))
  flow <- rbind(flow, c("Indexdate >= 1 Jan 2016, approx start of SGLT2i", nrow(pdata)))

pdata <- pdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(n()) %>%
  ungroup()

flow <- rbind(flow, c("Last post / patient", nrow(pdata)))

colnames(flow) <- c("Criteria", "N")
  
  

