# 01_read_data.R
# Reads the raw spectra and lab files, links them by lab ID (F number) and
# saves one table with all samples. No value is changed here.
source("config.R")
source("functions.R")
dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TABLES, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Spectra 2026 (GANSO and BEZOS) ------------------------------------------
nir <- read_winisi_nir(FILE_SPECTRA)
cat("Spectra 2026:", nrow(nir$spc), "scans x", ncol(nir$spc), "wavelengths\n")
nir <- nir[!nir$sample_number %in% TEST_SCAN_IDS, ]
avg <- average_repeated_scans(nir)
spc26 <- avg$dat
print(avg$repeated)
write.csv(avg$repeated, file.path(DIR_TABLES, "01_repeated_scans.csv"), row.names = FALSE)

# ---- 2. Spectra HSJ ---------------------------------------------------------------
spc_hsj <- read_hsj_spectra(FILE_HSJ_SPECTRA, HSJ_SPECTRA_HEADER_ROW)
cat("Spectra HSJ:", nrow(spc_hsj$spc), "samples\n")
stopifnot(identical(colnames(spc_hsj$spc), colnames(spc26$spc)),
          !anyDuplicated(spc_hsj$sample_number))

# ---- 3. GANSO: work order + lab results ---------------------------------------------
ot_g  <- read_work_order(FILE_WORK_ORDERS, WORK_ORDER_SHEETS[["GANSO"]], WORK_ORDER_START_ROW)
lab_g <- read_ganso(FILES_GANSO)
ganso <- link_ganso(ot_g, lab_g)
lim <- depth_limits(ganso$depth)
ganso_tab <- data.frame(
  sample_number = ganso$sample_number, dataset = "GANSO",
  location = paste("GANSO", ganso$region),
  profile = paste(ganso$region, ganso$order), unit = ganso$farm,
  top = lim$top, bottom = lim$bottom,
  tc = ganso$tc, ic = NA_real_, oc = NA_real_, n_raw = ganso$n_raw,
  bd = NA_real_, lon = NA_real_, lat = NA_real_,
  label = ganso$label_work_order
)

# ---- 4. BEZOS: work order + lab results ---------------------------------------------
ot_b  <- read_work_order(FILE_WORK_ORDERS, WORK_ORDER_SHEETS[["BEZOS"]], WORK_ORDER_START_ROW)
lab_b <- read_bezos(FILE_BEZOS, BEZOS_SHEETS)
bezos <- link_bezos(ot_b, lab_b)
lim <- depth_limits(bezos$depth)
bezos_tab <- data.frame(
  sample_number = bezos$sample_number, dataset = "BEZOS",
  location = paste("BEZOS", bezos$location_sheet),
  profile = paste(bezos$location_sheet, "plot", bezos$plot),
  unit = paste(bezos$location_sheet, "plot", bezos$plot),
  top = lim$top, bottom = lim$bottom,
  tc = bezos$tc, ic = bezos$ic, oc = bezos$oc, n_raw = NA_character_,
  bd = NA_real_, lon = NA_real_, lat = NA_real_,
  label = paste(bezos$location_sheet, "ID", bezos$id, bezos$treatment)
)

# ---- 5. HSJ: lab base, linked to the spectra with the block rule ---------------------
lab_h <- read_hsj_lab(FILE_HSJ_LAB, HSJ_LAB_SHEET)
sample_no <- hsj_sample_of_f(spc_hsj$sample_number)
stopifnot(!anyNA(sample_no), !anyDuplicated(sample_no))
h <- lab_h[match(sample_no, lab_h$sample_no), ]
stopifnot(!anyNA(h$sample_no))
hsj_tab <- data.frame(
  sample_number = spc_hsj$sample_number, dataset = "HSJ", location = "HSJ Llanos",
  profile = h$profile, unit = paste("HSJ point", h$point),
  top = h$top, bottom = h$bottom,
  tc = h$tc, ic = NA_real_, oc = NA_real_, n_raw = h$n_raw,
  bd = h$bd, lon = h$lon, lat = h$lat,
  label = paste0("Sample no. ", h$sample_no, " (", h$method, ")")
)
# HSJ lab samples without spectrum (expected: 7)
no_spc_hsj <- lab_h[!lab_h$sample_no %in% sample_no, c("sample_no", "profile", "depth", "tc")]
print(no_spc_hsj)
write.csv(no_spc_hsj, file.path(DIR_TABLES, "01_hsj_lab_without_spectrum.csv"), row.names = FALSE)

# ---- 6. One table: lab data + spectra ------------------------------------------------
lab_all <- rbind(hsj_tab, ganso_tab, bezos_tab)
spc_all <- rbind(spc_hsj$spc, spc26$spc)
ids_spc <- c(spc_hsj$sample_number, spc26$sample_number)

i <- match(lab_all$sample_number, ids_spc)
cat("Lab samples without spectrum:", sum(is.na(i)), "\n")
cat("Spectra without lab data:", length(setdiff(ids_spc, lab_all$sample_number)), "\n")
stopifnot(!anyNA(i), !anyDuplicated(lab_all$sample_number))

dat <- lab_all
dat$location <- factor(dat$location, levels = LOCATIONS)
dat$spc <- spc_all[i, ]
saveRDS(dat, file.path(DIR_PROCESSED, "01_soil_data.rds"))

# ---- 7. Summary ------------------------------------------------------------------
summary_tab <- do.call(rbind, lapply(split(dat, dat$location), function(d) data.frame(
  location = as.character(d$location[1]), n = nrow(d),
  profiles = length(unique(d$profile)), units = length(unique(d$unit)),
  tc_mean = round(mean(d$tc, na.rm = TRUE), 2),
  tc_min = min(d$tc, na.rm = TRUE), tc_max = max(d$tc, na.rm = TRUE),
  with_bd = sum(!is.na(d$bd))
)))
print(summary_tab, row.names = FALSE)
write.csv(summary_tab, file.path(DIR_TABLES, "01_summary_by_location.csv"), row.names = FALSE)