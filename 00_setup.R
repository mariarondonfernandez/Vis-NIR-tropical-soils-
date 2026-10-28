# 00_setup.R
# Checks that the project is set up correctly: project folder, raw files,
# folders and packages. It creates missing folders but never changes or
# deletes files. Run it once at the start, and again after moving the project.

# ---- 1. Project folder ------------------------------------------------------------
cat("Working directory:", getwd(), "\n")
rproj <- list.files(pattern = "\\.Rproj$")
if (length(rproj) != 1) {
  stop("The working directory must contain exactly one .Rproj file. ",
       "Open the project by double-clicking Vis_NIR_SOC.Rproj.")
}
cat("Project file:", rproj, "\n")

# Other .Rproj files in the parent folder could mean a second copy of the project
other <- setdiff(list.files("..", pattern = "\\.Rproj$", recursive = TRUE),
                 file.path(basename(getwd()), rproj))
if (length(other) > 0) {
  cat("WARNING: other R projects found near this one (check they are not copies):\n")
  print(file.path(normalizePath(".."), other))
}

# ---- 2. Folders ------------------------------------------------------------------
folders <- c("data/raw", "data/raw/not_used", "data/processed",
             "outputs/tables", "outputs/figures", "M_manual")
for (f in folders) {
  if (!dir.exists(f)) {
    dir.create(f, recursive = TRUE)
    cat("Created:", f, "\n")
  }
}

# ---- 3. Raw files ----------------------------------------------------------------
raw_files <- c("spectra_2026.nir", "HSJ_spectra.xlsx", "HSJ_lab.xlsx",
               "GANSO_lab_Orinoquia.xlsx", "GANSO_lab_Costa_Caribe.xlsx",
               "GANSO_lab_Magdalena_Medio.xlsx", "BEZOS_lab.xlsx",
               "work_orders_2026.xlsx")
found <- file.exists(file.path("data/raw", raw_files))
print(data.frame(file = raw_files, found = found,
                 size_MB = round(file.size(file.path("data/raw", raw_files)) / 1e6, 1)),
      row.names = FALSE)
if (!all(found)) stop("Missing raw files: ", paste(raw_files[!found], collapse = ", "))

extra <- setdiff(list.files("data/raw"), c(raw_files, "file_names.csv", "not_used"))
if (length(extra) > 0) cat("Other files in data/raw (not used by the scripts):", extra, "\n")

# ---- 4. Packages -----------------------------------------------------------------
packages <- c("openxlsx", "prospectr", "resemble", "ranger", "matrixStats",
              "aqp", "ggplot2")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing:", missing, "\n")
  install.packages(missing)
}
versions <- data.frame(package = packages,
                       version = vapply(packages, function(p) as.character(packageVersion(p)), ""))
print(versions, row.names = FALSE)
writeLines(capture.output(sessionInfo()), "outputs/session_info_setup.txt")

cat("\nProject OK.\n")