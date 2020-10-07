# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

ProjectTemplate::cache("flow")
ProjectTemplate::cache("pdata")
ProjectTemplate::cache("imp")
ProjectTemplate::cache("matchp")

ProjectTemplate::cache("matchpcross_deathcvhosphf")
ProjectTemplate::cache("matchpcross_deathhosphf")
ProjectTemplate::cache("matchpcross_death")
ProjectTemplate::cache("matchpcross_deathcv")
ProjectTemplate::cache("matchpcross_hosphf")
ProjectTemplate::cache("matchpcross_hospcv")
ProjectTemplate::cache("matchpcross_deathcvhospmistroke")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("metaout")
ProjectTemplate::cache("matchingn")
ProjectTemplate::cache("overtime")
ProjectTemplate::cache("lm2018")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("stratavars")
ProjectTemplate::cache("modvarsstrata")