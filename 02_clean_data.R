# 02_clean_data.R
# Applies the cleaning rules of functions.R (section 2) to the data of step 1.
# Every change and every flag is saved in outputs/tables/02_cleaning_log.csv.
source("config.R")
source("functions.R")

dat <- readRDS(file.path(DIR_PROCESSED, "01_soil_data.rds"))
res <- clean_soil_data(dat)
dat <- res$dat
log <- res$log

write.csv(log, file.path(DIR_TABLES, "02_cleaning_log.csv"), row.names = FALSE)
saveRDS(dat, file.path(DIR_PROCESSED, "02_soil_data_clean.rds"))

# ---- Summary of the log: number of samples per rule ---------------------------------
log_summary <- aggregate(sample_number ~ rule + action, data = log, FUN = length)
names(log_summary)[3] <- "n_samples"
print(log_summary, row.names = FALSE)

# ---- Values available per location after cleaning ----------------------------------
available <- do.call(rbind, lapply(split(dat, dat$location), function(d) data.frame(
  location   = as.character(d$location[1]),
  n          = nrow(d),
  soc        = sum(!is.na(d$soc)),
  soc_source = d$soc_source[1],
  tc         = sum(!is.na(d$tc)),
  n_total    = sum(!is.na(d$n_total)),
  bd         = sum(!is.na(d$bd)),
  units      = length(unique(d$unit)),
  to_review  = sum(d$review != "")
)))
print(available, row.names = FALSE)
write.csv(available, file.path(DIR_TABLES, "02_available_by_location.csv"), row.names = FALSE)