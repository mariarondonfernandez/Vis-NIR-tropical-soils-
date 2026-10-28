# functions.R
# All the functions of the soil project, in one file. Every script loads it
# with source("functions.R"). The scripts only call these functions.

library(openxlsx)

# ==============================================================================
# 1. Reading the raw files (step 1)
# ==============================================================================
# Functions that read the raw files and link spectra with lab results.
# They do not change any value: cleaning is done in step 2.

# Column names without line breaks or repeated spaces ("C\n(%)" -> "C (%)")
clean_names <- function(x) trimws(gsub("\\s+", " ", x))

# Top and bottom depth (cm) from labels like "0-10cm" or "10-30 cm"
depth_limits <- function(depth) {
  d <- gsub("cm|\\s", "", depth)
  lim <- do.call(rbind, strsplit(d, "-"))
  data.frame(top = as.numeric(lim[, 1]), bottom = as.numeric(lim[, 2]))
}

# Reads a WinISI .nir file exported from Mosaic (FOSS DS3).
# Structure found by inspecting the file (not from FOSS documentation):
# - header of 896 bytes: number of samples (bytes 3-4), number of points
#   (bytes 7-8), and start, step and end of the two detector segments
# - one record per sample of 480 + 4 * points bytes: sample ID at the
#   start, spectrum (4-byte floats) from byte 257 of the record
read_winisi_nir <- function(path) {
  b <- readBin(path, "raw", n = file.info(path)$size)
  int16 <- function(off) readBin(b[off + 1:2], "integer", size = 2, signed = FALSE, endian = "little")
  float <- function(off) readBin(b[off + 1:4], "numeric", size = 4, endian = "little")

  n_samples <- int16(2)
  n_points  <- int16(6)
  wav <- c(seq(float(204), float(260), by = float(232)),   # detector 1 (silicon)
           seq(float(208), float(264), by = float(236)))   # detector 2 (PbS)
  stopifnot(length(wav) == n_points)

  header <- 896
  record <- 480 + 4 * n_points
  stopifnot(header + n_samples * record <= length(b))

  ids <- character(n_samples)
  spc <- matrix(NA_real_, n_samples, n_points, dimnames = list(NULL, as.character(wav)))
  for (i in seq_len(n_samples)) {
    start <- header + (i - 1) * record
    id <- b[start + 1:16]
    end <- which(id == as.raw(0))
    ids[i] <- rawToChar(id[seq_len(if (length(end) > 0) end[1] - 1 else 16)])
    spc[i, ] <- readBin(b[start + 256 + seq_len(4 * n_points)], "numeric",
                        size = 4, n = n_points, endian = "little")
  }
  stopifnot(all(is.finite(spc)), all(spc > 0 & spc < 3))

  dat <- data.frame(sample_number = ids)
  dat$spc <- spc
  dat
}

# Mean spectrum of the samples scanned more than once (as in the SoilFER
# manual). Returns the averaged data and a table of the repeated samples.
average_repeated_scans <- function(dat) {
  ids <- unique(dat$sample_number)
  n <- as.vector(table(dat$sample_number)[ids])
  spc <- rowsum(dat$spc, dat$sample_number, reorder = FALSE) / n
  rep_ids <- ids[n > 1]
  max_diff <- vapply(rep_ids, function(id) {
    s <- dat$spc[dat$sample_number == id, , drop = FALSE]
    max(apply(s, 2, function(v) diff(range(v))))
  }, numeric(1))

  out <- data.frame(sample_number = ids)
  out$spc <- unname(spc)
  colnames(out$spc) <- colnames(dat$spc)
  list(
    dat = out,
    repeated = data.frame(sample_number = rep_ids, n_scans = n[n > 1],
                          max_abs_difference = unname(max_diff))
  )
}

# HSJ spectra: Excel exported from Mosaic; 'header_row' has "Position",
# "Sample Number" and the wavelengths
read_hsj_spectra <- function(path, header_row) {
  x <- read.xlsx(path, startRow = header_row, check.names = FALSE, sep.names = " ")
  wav <- suppressWarnings(as.numeric(colnames(x)))
  is_wav <- !is.na(wav)
  spc <- as.matrix(x[, is_wav])
  storage.mode(spc) <- "double"
  colnames(spc) <- as.character(wav[is_wav])

  dat <- data.frame(sample_number = trimws(as.character(x[["Sample Number"]])))
  dat$spc <- spc
  dat[!is.na(dat$sample_number) & dat$sample_number != "", ]
}

# Sample list of a lab work order: running number, F number and external label
read_work_order <- function(path, sheet, start_row) {
  x <- read.xlsx(path, sheet = sheet, startRow = start_row, colNames = FALSE,
                 cols = 1:3, skipEmptyRows = FALSE)
  names(x) <- c("running_number", "sample_number", "label")
  x$running_number <- suppressWarnings(as.integer(x$running_number))
  x <- x[!is.na(x$running_number) & grepl("^F2", x$sample_number), ]
  x$label <- trimws(as.character(x$label))
  x
}

# GANSO lab results: all sheets of the three regional files.
# Column names differ between sheets ("C (%)" or "C"; "ORDEN" or "ID").
# The running number is "N. laboratorio" in S-191, and the number written
# before the label in S-197 and S-198 ("209 69_GO_029_0-10cm_SOC").
read_ganso <- function(files) {
  out <- list()
  for (region in names(files)) {
    for (sh in getSheetNames(files[[region]])) {
      x <- read.xlsx(files[[region]], sheet = sh, check.names = FALSE, sep.names = " ")
      names(x) <- clean_names(names(x))
      col_c     <- intersect(c("C (%)", "C"), names(x))[1]
      col_n     <- intersect(c("N (%)", "N"), names(x))[1]
      col_order <- intersect(c("ORDEN", "ID"), names(x))[1]
      x <- x[!is.na(x[[col_order]]), ]

      ident <- trimws(as.character(x[["Identificador Laboratorio"]]))
      label <- if ("Etiqueta" %in% names(x)) trimws(as.character(x[["Etiqueta"]])) else ident
      no_label <- is.na(label) | label == ""
      label[no_label] <- ident[no_label]

      has_prefix <- grepl("^\\d+\\s+", ident)
      running <- as.integer(x[["N. laboratorio"]])
      running[has_prefix] <- as.integer(sub("^(\\d+)\\s+.*$", "\\1", ident[has_prefix]))

      out[[paste(region, sh)]] <- data.frame(
        region = region, sheet = sh, running_number = running, label = label,
        order = as.integer(x[[col_order]]),
        farm = trimws(as.character(x[["ID_FINCA"]])),
        depth = gsub("\\s", "", x[["Depth"]]),
        tc = suppressWarnings(as.numeric(x[[col_c]])),
        n_raw = trimws(as.character(x[[col_n]]))   # can be "< LCM": converted in step 2
      )
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# Links the GANSO work order (F numbers) with the lab results, region by
# region, with the rule in GANSO_LINK. Stops if any link is missing or repeated.
link_ganso <- function(ot, lab) {
  out <- list()
  for (region in names(GANSO_WORK_ORDER_ROWS)) {
    o <- ot[ot$running_number %in% GANSO_WORK_ORDER_ROWS[[region]], ]
    l <- lab[lab$region == region, ]
    i <- if (GANSO_LINK[[region]] == "running_number") {
      match(o$running_number, l$running_number)
    } else {
      match(tolower(o$label), tolower(l$label))
    }
    if (nrow(o) != nrow(l) || anyNA(i) || anyDuplicated(i)) {
      stop("GANSO ", region, ": ", nrow(o), " work-order rows, ", nrow(l), " lab rows, ",
           sum(is.na(i)), " without match, ", sum(duplicated(i[!is.na(i)])), " repeated matches")
    }
    out[[region]] <- data.frame(sample_number = o$sample_number, label_work_order = o$label,
                                l[i, ], row.names = NULL)
  }
  do.call(rbind, out)
}

# BEZOS lab results ("-" means not measured and becomes NA)
read_bezos <- function(path, sheets) {
  num <- function(v) suppressWarnings(as.numeric(v))
  out <- lapply(sheets, function(sh) {
    x <- read.xlsx(path, sheet = sh, check.names = FALSE, sep.names = " ")
    names(x) <- clean_names(names(x))
    x <- x[!is.na(x[["ID"]]), ]
    data.frame(
      location_sheet = sh, id = as.integer(x[["ID"]]), plot = as.integer(x[["PLOT"]]),
      block = as.integer(x[["BLOCK"]]), treatment = trimws(x[["TREATMENT"]]),
      species = trimws(x[["ESPECIE"]]), depth = gsub("\\s", "", x[["Depth"]]),
      tc = num(x[["Total carbon"]]), ic = num(x[["Inorganic carbon"]]),
      oc = num(x[["Organic carbon"]])
    )
  })
  do.call(rbind, out)
}

# Links the BEZOS work order with the lab results by row order. The order is
# only accepted if the treatment sequence is identical in every row.
link_bezos <- function(ot, lab) {
  if (nrow(ot) != nrow(lab)) stop("BEZOS: ", nrow(ot), " work-order rows but ", nrow(lab), " lab rows")
  same <- ot$label == lab$treatment
  if (!all(same)) stop("BEZOS: treatment differs in ", sum(!same), " rows; the link by row order is not valid")
  data.frame(sample_number = ot$sample_number, label_work_order = ot$label, lab, row.names = NULL)
}

# HSJ lab base (Base_Mike). Depths are stored as negative numbers.
read_hsj_lab <- function(path, sheet) {
  x <- read.xlsx(path, sheet = sheet, check.names = FALSE, sep.names = " ")
  names(x) <- clean_names(names(x))
  x <- x[!is.na(x[["Sample no."]]), ]
  data.frame(
    sample_no = as.integer(x[["Sample no."]]), point = as.integer(x[["Point_HSJ"]]),
    profile = trimws(x[["COD"]]), method = trimws(x[["Sampling.method"]]),
    depth = gsub("\\s", "", x[["Depth"]]),
    top = abs(as.numeric(x[["Upper_cm"]])), bottom = abs(as.numeric(x[["Lower_cm"]])),
    lon = as.numeric(x[["LONGITUD"]]), lat = as.numeric(x[["LATITUD"]]),
    bd = as.numeric(x[["BD (g/cm3)"]]),
    tc = as.numeric(x[["C Comb(%)"]]),
    n_raw = as.character(x[["N Com(%)"]])
  )
}

# 'Sample no.' of the HSJ lab base for each F number, with the block rule in
# HSJ_LINK (the pits were scanned first). NA if outside the blocks.
hsj_sample_of_f <- function(f_number, link = HSJ_LINK) {
  f <- as.integer(sub("^F25-", "", f_number))
  b <- pmax(findInterval(f, link$first_f), 1)
  out <- link$first_sample[b] + (f - link$first_f[b])
  out[f < link$first_f[1] | f > link$last_f[b]] <- NA
  out
}

# ==============================================================================
# 2. Cleaning rules (step 2)
# ==============================================================================
# Cleaning rules. Only impossible values or confirmed errors are changed; the
# rest is flagged for review in the column 'review'. Every change and every
# flag is written to a log, so we can always say which value was changed,
# why, and by which rule.

# Adds rows to the log for the samples in 'rows'
log_change <- function(log, dat, rows, variable, rule, action, new_value = NA) {
  if (length(rows) == 0) return(log)
  old <- if (variable %in% names(dat)) as.character(dat[[variable]][rows]) else NA_character_
  rbind(log, data.frame(
    sample_number = dat$sample_number[rows], location = as.character(dat$location[rows]),
    variable = variable, old_value = old, new_value = as.character(new_value),
    rule = rule, action = action
  ))
}

# Adds a review note to the samples in 'rows' (notes are separated by "; ")
add_review <- function(review, rows, text) {
  review[rows] <- ifelse(review[rows] == "", text, paste(review[rows], text, sep = "; "))
  review
}

clean_soil_data <- function(dat) {
  log <- data.frame()
  dat$review <- ""

  # 1. Nitrogen below the quantification limit ("< LCM"): unknown value
  rows <- which(dat$n_raw %in% CENSORED_LABELS)
  log <- log_change(log, dat, rows, "n_raw", "Below the quantification limit", "set to NA")
  dat$n_total <- suppressWarnings(as.numeric(dat$n_raw))
  bad <- which(!is.na(dat$n_raw) & !dat$n_raw %in% CENSORED_LABELS & is.na(dat$n_total))
  if (length(bad) > 0) stop("Nitrogen values that are not numbers: ",
                            paste(unique(dat$n_raw[bad]), collapse = ", "))

  # 2. Exactly zero is a non-detect, not a measurement
  for (v in intersect(c("tc", "n_total"), ZERO_IS_MISSING)) {
    rows <- which(dat[[v]] == 0)
    log <- log_change(log, dat, rows, v, "Value is exactly 0 (non-detect)", "set to NA")
    dat[[v]][rows] <- NA
  }

  # 3. Negative values are impossible
  for (v in c("tc", "ic", "oc", "n_total", "bd")) {
    rows <- which(dat[[v]] < 0)
    log <- log_change(log, dat, rows, v, "Negative value", "set to NA")
    dat[[v]][rows] <- NA
  }

  # 4. Carbon balance: total = inorganic + organic (only where all three exist)
  rows <- which(abs(dat$tc - dat$ic - dat$oc) > CARBON_BALANCE_TOL)
  log <- log_change(log, dat, rows, "oc",
                    paste0("|total - inorganic - organic| > ", CARBON_BALANCE_TOL), "flagged for review")
  dat$review <- add_review(dat$review, rows, "carbon balance")

  # 5. Organic carbon (SOC) according to the location (see SOC_SOURCE)
  src <- unname(SOC_SOURCE[as.character(dat$location)])
  dat$soc <- ifelse(src %in% "organic", dat$oc, ifelse(src %in% "total", dat$tc, NA))
  dat$soc_source <- ifelse(is.na(src), "not defined", src)
  rows <- which(is.na(src) & !is.na(dat$tc))
  log <- log_change(log, dat, rows, "soc",
                    "Carbonates not ruled out: total C not used as SOC", "SOC not defined")

  # 6. C:N outside the usual range: flagged, not changed
  cn <- dat$tc / dat$n_total
  rows <- which(cn < CN_REVIEW_RANGE[1] | cn > CN_REVIEW_RANGE[2])
  log <- log_change(log, dat, rows, "n_total",
                    paste0("C:N outside ", CN_REVIEW_RANGE[1], "-", CN_REVIEW_RANGE[2]), "flagged for review")
  dat$review <- add_review(dat$review, rows, "C:N")

  # 7. Possible carbonates where total C is used as SOC: flagged, not changed.
  #    Signals: total C higher than in the 0-10 cm layer of the same profile,
  #    or high C:N in the subsoil. Both can also come from buried organic
  #    horizons or charcoal, so they only point to samples worth an HCl test.
  uses_total <- dat$soc_source == "total"
  top_tc <- tapply(dat$tc[dat$top == 0], dat$profile[dat$top == 0], function(v) v[1])
  ref <- unname(top_tc[dat$profile])
  tc_up <- uses_total & dat$top > 0 & !is.na(dat$tc) & !is.na(ref) & dat$tc > ref
  cn_high <- uses_total & dat$top >= CARBONATE_SUBSOIL_TOP & !is.na(cn) & cn > CARBONATE_CN_MAX
  rows <- which(tc_up | cn_high)
  log <- log_change(log, dat, rows, "soc",
                    paste0("Possible carbonate (total C above the 0-10 cm layer, or C:N > ",
                           CARBONATE_CN_MAX, " below ", CARBONATE_SUBSOIL_TOP, " cm)"),
                    "flagged for review")
  dat$review <- add_review(dat$review, rows, "possible carbonate")

  # 8. Impossible depths (SoilFER checks): the sample cannot be placed in the profile
  rows <- which(is.na(dat$top) | is.na(dat$bottom) | dat$top < 0 | dat$bottom <= dat$top)
  log <- log_change(log, dat, rows, "top", "Missing, negative or inverted depth", "sample removed")
  if (length(rows) > 0) dat <- dat[-rows, ]

  # 9. Longitude with the wrong sign (HSJ samples listed in config)
  hsj_no <- suppressWarnings(as.integer(sub("^Sample no\\. (\\d+).*$", "\\1", dat$label)))
  rows <- which(dat$dataset == "HSJ" & hsj_no %in% HSJ_LONGITUDE_SIGN_FIX & dat$lon > 0)
  log <- log_change(log, dat, rows, "lon", "Positive longitude (Colombia is west)",
                    "sign corrected", new_value = -dat$lon[rows])
  dat$lon[rows] <- -dat$lon[rows]
  if (any(dat$lon > 0, na.rm = TRUE)) stop("Other positive longitudes found: check the coordinates")

  # 10. GANSO farm IDs: confirmed fixes, then placeholders (unit = profile)
  rows <- which(dat$profile %in% names(FARM_FIXES))
  log <- log_change(log, dat, rows, "unit", "Farm ID corrected", "farm changed",
                    new_value = FARM_FIXES[dat$profile[rows]])
  dat$unit[rows] <- FARM_FIXES[dat$profile[rows]]

  rows <- which(grepl(UNKNOWN_FARM_PATTERN, dat$unit))
  log <- log_change(log, dat, rows, "unit", "Farm ID is a placeholder",
                    "profile used as unit", new_value = dat$profile[rows])
  dat$unit[rows] <- dat$profile[rows]
  dat$review <- add_review(dat$review, rows, "farm unknown")

  # 11. GANSO: farm written in the label differs from ID_FINCA (ID_FINCA is used)
  pattern <- "^(\\d+\\s+)?\\d+_([A-Za-z]+_\\d+(_\\d+)?)_\\d+-\\d+cm.*$"
  has_farm <- dat$dataset == "GANSO" & grepl(pattern, dat$label)
  farm_label <- toupper(sub(pattern, "\\2", dat$label))
  rows <- which(has_farm & farm_label != toupper(dat$unit) & !grepl(UNKNOWN_FARM_PATTERN, dat$unit))
  log <- log_change(log, dat, rows, "unit", "Farm in the label differs from ID_FINCA", "flagged for review")
  dat$review <- add_review(dat$review, rows, "farm in label differs")

  # 12. Bulk density bag scanned instead of the SOC bag
  rows <- which(grepl(SCANNED_BD_BAG_PATTERN, dat$label))
  log <- log_change(log, dat, rows, "label", "Bulk density bag scanned", "flagged for review")
  dat$review <- add_review(dat$review, rows, "BD bag scanned")

  # 13. Same depth repeated in one profile
  key <- paste(dat$profile, dat$top, dat$bottom)
  rows <- which(duplicated(key) | duplicated(key, fromLast = TRUE))
  log <- log_change(log, dat, rows, "profile", "Same depth repeated in the profile", "flagged for review")
  dat$review <- add_review(dat$review, rows, "repeated depth")

  rownames(dat) <- NULL
  list(dat = dat, log = log)
}

# ==============================================================================
# 3. Spectral pretreatment and spectral outliers (step M02)
# ==============================================================================
# Every pretreatment step works on one spectrum at a time (row-wise), so it
# does not use information from other samples: it can be applied to all
# samples before splitting without data leakage.

# Pretreatment of the SoilFER manual, adapted to Vis-NIR:
# splice correction (optional) -> resample -> Savitzky-Golay derivative -> SNV.
# Returns the resampled absorbance and the pretreated spectra.
pretreat_manual <- function(spc, splice = TRUE, step_nm = 2, m = 1, p = 1, w = 11) {
  wav <- as.numeric(colnames(spc))
  x <- spc
  if (splice) x <- prospectr::spliceCorrection(x, wav, splice = SPLICE_POINT)

  new_wav <- seq(ceiling(min(wav)), floor(max(wav)), by = step_nm)
  resampled <- prospectr::resample(x, wav = wav, new.wav = new_wav)
  colnames(resampled) <- as.character(new_wav)

  pretreated <- prospectr::savitzkyGolay(resampled, m = m, p = p, w = w)
  pretreated <- prospectr::standardNormalVariate(pretreated)
  list(resampled = resampled, pretreated = pretreated)
}

# Spectral outliers: standardized Mahalanobis distance (GH) in the space of
# the first PCs. GH = D^2 / k, where D is the Mahalanobis distance and k the
# number of PCs. The spectra are centred but not scaled: after SNV, scaling
# each wavelength would give flat, noisy regions the same weight as the
# absorption bands (as in the SoilFER manual).
gh_distance <- function(spc, n_pcs = N_PCS_OUTLIERS) {
  pca <- prcomp(spc, center = TRUE, scale. = FALSE)
  scores <- pca$x[, 1:n_pcs]
  d2 <- rowSums(sweep(scores, 2, apply(scores, 2, sd), "/")^2)
  list(gh = d2 / n_pcs,
       var_explained = sum(summary(pca)$importance[2, 1:n_pcs]))
}

# Range of the wavelengths of a spectral matrix, as text ("400-2499.5 nm")
wav_range <- function(spc) paste0(paste(range(as.numeric(colnames(spc))), collapse = "-"), " nm")

# ==============================================================================
# 4. Figures
# ==============================================================================
# Spectra figure in the style of Ramirez-Lopez et al. (2026), Fig. 2: every
# spectrum as a thin transparent line in the colour of its location, and the
# 0, 25, 50, 75 and 100% quantiles of each dataset as black lines.
# Rows: pretreatment stage. Columns: dataset.

# Long tables for ggplot from a named list of spectral matrices (one per stage).
# 'location' should be a factor, so the legend follows the order of LOCATIONS.
# 'every' keeps one wavelength out of 'every', to make the plot lighter.
spectra_figure_data <- function(spc_list, dataset, location, every = 1,
                                probs = c(0, 0.25, 0.5, 0.75, 1)) {
  long <- list()
  q <- list()
  for (stage in names(spc_list)) {
    spc <- spc_list[[stage]]
    spc <- spc[, seq(1, ncol(spc), by = every), drop = FALSE]
    wav <- as.numeric(colnames(spc))
    long[[stage]] <- data.frame(
      stage = stage,
      dataset = rep(dataset, times = ncol(spc)),
      location = rep(location, times = ncol(spc)),
      sample = rep(seq_len(nrow(spc)), times = ncol(spc)),
      wavelength = rep(wav, each = nrow(spc)),
      value = as.vector(spc)
    )
    for (ds in unique(dataset)) {
      qs <- apply(spc[dataset == ds, , drop = FALSE], 2, quantile, probs = probs)
      q[[paste(stage, ds)]] <- data.frame(
        stage = stage, dataset = ds,
        quantile = rep(probs, times = ncol(qs)),
        wavelength = rep(wav, each = nrow(qs)),
        value = as.vector(qs)
      )
    }
  }
  long <- do.call(rbind, long)
  q <- do.call(rbind, q)
  for (v in c("stage", "dataset")) {
    lev <- if (v == "stage") names(spc_list) else unique(dataset)
    long[[v]] <- factor(long[[v]], levels = lev)
    q[[v]] <- factor(q[[v]], levels = lev)
  }
  list(long = long, q = q)
}

plot_spectra_panels <- function(fig_data) {
  ggplot2::ggplot(fig_data$long, ggplot2::aes(wavelength, value)) +
    ggplot2::geom_line(ggplot2::aes(group = sample, colour = location), alpha = 0.12, linewidth = 0.25) +
    ggplot2::geom_line(data = fig_data$q, ggplot2::aes(group = quantile), colour = "black", linewidth = 0.35) +
    ggplot2::facet_grid(stage ~ dataset, scales = "free_y") +
    ggplot2::scale_colour_manual(values = COL_LOCATION, name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, override.aes = list(alpha = 1, linewidth = 1))) +
    ggplot2::scale_x_continuous(breaks = seq(500, 2000, by = 500), expand = c(0.04, 0)) +
    ggplot2::labs(x = "Wavelength [nm]", y = NULL) +
    theme_paper() +
    ggplot2::theme(legend.position = "bottom", panel.spacing.x = ggplot2::unit(1.2, "lines"))
}

# Common style for all figures: grey panels as in Ramirez-Lopez et al. (2026),
# with axis numbers as large as the rest of the text
theme_paper <- function(base_size = FIG_BASE_SIZE) {
  ggplot2::theme_grey(base_size = base_size) +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = ggplot2::rel(1), colour = "grey20"),
      axis.title       = ggplot2::element_text(size = ggplot2::rel(1.1)),
      strip.text       = ggplot2::element_text(size = ggplot2::rel(1)),
      legend.text      = ggplot2::element_text(size = ggplot2::rel(1)),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# Saves a ggplot as PNG (and PDF if FIG_PDF is TRUE) with the sizes in config.R
save_figure <- function(plot, path_no_ext, width = FIG_WIDTH, height = FIG_HEIGHT) {
  ggplot2::ggsave(paste0(path_no_ext, ".png"), plot, width = width, height = height,
                  units = "in", dpi = FIG_DPI)
  if (FIG_PDF) ggplot2::ggsave(paste0(path_no_ext, ".pdf"), plot, width = width,
                               height = height, units = "in")
  invisible(path_no_ext)
}
