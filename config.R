# config.R
# Shared settings for all workflows. Every script starts with source("config.R");
# scripts of a workflow then load their own config file.
# To change something, edit only this file (or the workflow config).

# ---- Paths --------------------------------------------------------------------
DIR_RAW       <- "data/raw"        # original files, never edited (names in file_names.csv)
DIR_PROCESSED <- "data/processed"  # files created by the common steps 01-03
DIR_TABLES    <- "outputs/tables"  # tables of the common steps
DIR_FIGURES   <- "outputs/figures"

# ---- Raw files: spectra ---------------------------------------------------------
# GANSO and BEZOS: WinISI .nir file exported from Mosaic (full precision).
FILE_SPECTRA       <- file.path(DIR_RAW, "spectra_2026.nir")
TEST_SCAN_IDS      <- c("F25-0000", "F25-XXXX")   # instrument tests, not soil samples
REPEATED_SCAN_RULE <- "mean"                      # F261472 and F261473 were scanned twice

# HSJ: spectra exported from Mosaic to Excel (full precision); rows 1-13 are file information
FILE_HSJ_SPECTRA       <- file.path(DIR_RAW, "HSJ_spectra.xlsx")
HSJ_SPECTRA_HEADER_ROW <- 14

# ---- Raw files: work orders (GANSO and BEZOS) -------------------------------------
# Link between the lab ID (F number) and the sample label.
# Columns used: 1 = running number, 2 = F number, 3 = external label.
FILE_WORK_ORDERS     <- file.path(DIR_RAW, "work_orders_2026.xlsx")
WORK_ORDER_SHEETS    <- c(GANSO = "OT_021", BEZOS = "OT_041")
WORK_ORDER_START_ROW <- 37   # first sample row (rows 35 and 36 are the English and Spanish headers)

# ---- Raw files: GANSO lab results ---------------------------------------------------
FILES_GANSO <- c(
  "Orinoquia"       = file.path(DIR_RAW, "GANSO_lab_Orinoquia.xlsx"),
  "Costa Caribe"    = file.path(DIR_RAW, "GANSO_lab_Costa_Caribe.xlsx"),
  "Magdalena Medio" = file.path(DIR_RAW, "GANSO_lab_Magdalena_Medio.xlsx")
)
# Rows of OT_021 that belong to each region (checked against the labels)
GANSO_WORK_ORDER_ROWS <- list(
  "Orinoquia"       = 1:576,
  "Costa Caribe"    = 577:1252,
  "Magdalena Medio" = 1253:1616
)
# How each region is linked to the work order:
# - Orinoquia: by running number, because several labels are wrong
#   (profile 139 labelled "137"; profiles 131 and 132 without depth)
# - Costa Caribe and Magdalena Medio: by label (not case sensitive)
GANSO_LINK <- c("Orinoquia" = "running_number",
                "Costa Caribe" = "label", "Magdalena Medio" = "label")

# ---- Raw files: BEZOS lab results ---------------------------------------------------
# OT_041 only has the treatment, not a unique ID, so the link is the row order:
# the sheets were listed in this order (checked: identical treatment sequence
# in all 1100 rows).
FILE_BEZOS   <- file.path(DIR_RAW, "BEZOS_lab.xlsx")
BEZOS_SHEETS <- c("Llanos", "Palmira")

# ---- Raw files: HSJ lab results -----------------------------------------------------
FILE_HSJ_LAB  <- file.path(DIR_RAW, "HSJ_lab.xlsx")
HSJ_LAB_SHEET <- "Stock.SOC _2"
# datos_nir.xlsx and soil_sample_metadata_HSJ.xlsx are NOT used: both link
# samples to the wrong spectra (see PeerJ notes).
#
# Link between the F number (F25-xxxx) and 'Sample no.' of the lab base. The
# pits (Sample no. 505-636) were scanned first. Checked with a PLS model.
HSJ_LINK <- data.frame(
  first_f      = c(1015, 1147, 1519),
  last_f       = c(1146, 1518, 1650),
  first_sample = c(505,  133,  1)
)
HSJ_LONGITUDE_SIGN_FIX <- 509:512   # Sample no. with positive longitude (CS_SAB_A_001_CAL_001)

# ---- Locations ----------------------------------------------------------------
LOCATIONS <- c("HSJ Llanos", "GANSO Orinoquia", "GANSO Costa Caribe",
               "GANSO Magdalena Medio", "BEZOS Llanos", "BEZOS Palmira")

# Bulk density: HSJ is read; GANSO and BEZOS files requested (workflow D)
HAS_BULK_DENSITY <- c(HSJ = TRUE, GANSO = FALSE, BEZOS = FALSE)

# ---- Target variables ---------------------------------------------------------
# Reference labs:
# - HSJ and GANSO: CIAT lab, dry combustion (Thermo FlashSmart NC Soil) WITHOUT
#   acid pretreatment, so the lab value is TOTAL carbon.
# - BEZOS: external lab. Llanos: total carbon only. Palmira: total and
#   inorganic carbon measured; organic carbon = total minus inorganic (all samples).
# Source of SOC in each location:
#   "organic" = lab organic carbon
#   "total"   = total carbon taken as SOC (no carbonates)
#   NA        = no SOC until carbonates are ruled out
SOC_SOURCE <- c(
  "HSJ Llanos"            = "total",    # acid soils (pH about 4.5)
  "GANSO Orinoquia"       = "total",    # acid soils
  "GANSO Costa Caribe"    = "total",    # pH below 7.5 and no carbonates observed (GANSO team,
                                        # 2026-10-15); no carbonate signal in 161 of 169 profiles
  "GANSO Magdalena Medio" = "total",    # same basis; no signal in 82 of 91 profiles
  "BEZOS Llanos"          = "total",    # acid soils, no carbonates (BEZOS team)
  "BEZOS Palmira"         = "organic"   # alkaline soils: inorganic C up to 3.3% below 30 cm
)
TARGETS <- c("soc", "tc", "n_total")
VAR_LABELS <- c(
  soc     = "Organic carbon (%)",
  tc      = "Total carbon (%)",
  n_total = "Total nitrogen (%)"
)

# ---- Cleaning decisions (step 2) ----------------------------------------------
CENSORED_LABELS    <- c("< LCM")                # below the quantification limit: set to NA
ZERO_IS_MISSING    <- c("soc", "tc", "n_total") # exactly 0 is a non-detect, not a measurement
CN_REVIEW_RANGE    <- c(5, 30)                  # C:N outside this range is flagged, not removed
CARBON_BALANCE_TOL <- 0.02                      # |total - inorganic - organic| above this is an error
UNKNOWN_FARM_PATTERN   <- "^GANSO_ORI"          # placeholder farm IDs (GANSO profiles 144-153)
FARM_FIXES             <- c()                   # "profile" = "farm", after confirmation
SCANNED_BD_BAG_PATTERN <- "_DA$"                # bulk density bag scanned instead of the SOC bag

# Possible carbonates where total C is used as SOC: flagged for review, not changed.
# Signals: total C higher than in the 0-10 cm layer of the same profile, or
# C:N above CARBONATE_CN_MAX in the subsoil (carbonate adds C but not N).
# Both signals can also come from buried organic horizons or charcoal.
CARBONATE_SUBSOIL_TOP <- 30   # cm
CARBONATE_CN_MAX      <- 20

# ---- Independent units ----------------------------------------------------------
# All samples of one unit always go to the same set and the same fold.
# HSJ:   sampling point (a pit and a probe profile at the same point go together)
# GANSO: farm (several profiles per farm, close together, same management)
# BEZOS: plot (one profile per plot)
GROUP_UNIT <- c(HSJ = "point", GANSO = "farm", BEZOS = "plot")

# ---- Pretreatment (step 3): research version ----------------------------------------
SPLICE_POINT  <- 1099.5  # small jump (about 0.001) where the two detectors meet
SG_POLY       <- 2
RESAMPLE_STEP <- 5       # nm, final resolution
PRETREAT_CANDIDATES <- data.frame(
  name   = c("SG1 11 + SNV", "SG1 21 + SNV", "SNV-DT + SG1 11", "SG2 21 + SNV"),
  snv_dt = c(FALSE, FALSE, TRUE, FALSE),   # SNV + detrend on absorbance, before the derivative
  deriv  = c(1, 1, 1, 2),
  window = c(11, 21, 11, 21),              # points (x 0.5 nm)
  snv    = c(TRUE, TRUE, FALSE, TRUE)      # SNV after the derivative
)
PRETREAT_FIXED <- "SG1 11 + SNV"           # same as the forage workflow

# ---- Pretreatment (step 3): WinISI version (workflow C) ----------------------------
# Same settings as the 2026 forage equations of the lab: SNVD (SNV + detrend)
# and math treatment 1,4,4,1 (derivative, gap, smooth, second smooth).
WINISI_SCATTER <- "SNVD"
WINISI_MATH    <- c(deriv = 1, gap = 4, smooth = 4, smooth2 = 1)
WINISI_STEP_NM <- NA   # CONFIRM with the lab: WinISI data point spacing (2, 4 or 8 nm)
WINISI_RANGES  <- list("400-2500 nm" = c(400, 2500), "1100-2500 nm" = c(1100, 2500))

# ---- Spectral outliers (step 3) ----------------------------------------------------
N_PCS_OUTLIERS <- 5   # as in Ramirez-Lopez et al. (2026)
GH_LIMIT       <- 3   # GH = D^2 / number of PCs; flagged for review, not removed

# ---- Lab uncertainty ---------------------------------------------------------------
# Standard deviation of one dry combustion measurement: 0.15% C, placeholder
# from the SoilFER manual (Stevens et al., 2013: about 0.2% in European labs).
# PENDING: repeatability of the CIAT FlashSmart (validation in progress) and of
# the external lab (BEZOS).
LAB_SD <- c(soc = 0.15, tc = 0.15, n_total = NA)
# SOC by location: one measurement (0.15), except Palmira, where SOC is total
# minus inorganic C, two measurements: sqrt(0.15^2 + 0.15^2) = 0.21
LAB_SD_SOC <- c(
  "HSJ Llanos"            = 0.15,
  "GANSO Orinoquia"       = 0.15,
  "GANSO Costa Caribe"    = 0.15,
  "GANSO Magdalena Medio" = 0.15,
  "BEZOS Llanos"          = 0.15,
  "BEZOS Palmira"         = 0.21
)

# ---- Figures -----------------------------------------------------------------------
FIG_DPI       <- 300
FIG_WIDTH     <- 7.5
FIG_HEIGHT    <- 6
FIG_PDF       <- TRUE
FIG_BASE_SIZE <- 12
COL_LOCATION <- c(
  "HSJ Llanos"            = "#B15928",
  "GANSO Orinoquia"       = "#1F78B4",
  "GANSO Costa Caribe"    = "#33A02C",
  "GANSO Magdalena Medio" = "#6A3D9A",
  "BEZOS Llanos"          = "#FF7F00",
  "BEZOS Palmira"         = "#C8102E"
)

# ---- Reproducibility ----------------------------------------------------------
SEED <- 801124
