# 02b_carbonate_screening.R
# Screening for carbonates in GANSO Costa Caribe and Magdalena Medio, where
# total C cannot be taken as SOC yet. It only PRIORITISES samples for the HCl
# test and inorganic C analysis in the lab; it does not decide.
# Two kinds of evidence:
#   1. Lab data per profile: total C that increases with depth, high C:N in subsoil
#   2. Spectra: PLS model of inorganic C trained with BEZOS Palmira (measured
#      inorganic C), applied only to samples inside the Palmira spectral domain
source("config.R")
source("functions.R")
source("M_manual/config_M.R")
library(pls)

# ---- Settings of this screening --------------------------------------------------------
TARGET_LOCATIONS <- c("GANSO Costa Caribe", "GANSO Magdalena Medio")
IC_THRESHOLD     <- 0.3   # % inorganic C considered "carbonate present"
SUBSOIL_TOP      <- 30    # cm; subsoil = samples starting at or below this depth
CN_SUBSOIL_MAX   <- 20    # C:N above this in the subsoil is a carbonate signal
N_PCS_DOMAIN     <- 10
DOMAIN_CONF      <- 0.99

dat <- readRDS(file.path(DIR_M_PROCESSED, "M02_spectra.rds"))
X <- dat$spc_pretreated
pal <- which(dat$location == "BEZOS Palmira" & !is.na(dat$ic))
tgt <- which(dat$location %in% TARGET_LOCATIONS)
cat("Palmira samples with inorganic C:", length(pal), "| target samples:", length(tgt), "\n")

# ---- 1. PLS of inorganic C in Palmira, cross-validated by plot ---------------------------
units <- unique(dat$unit[pal])
set.seed(SEED)
fold_of_unit <- sample(rep(1:10, length.out = length(units)))
fold <- fold_of_unit[match(dat$unit[pal], units)]

d_pal <- data.frame(ic = dat$ic[pal])
d_pal$X <- X[pal, ]
fit <- plsr(ic ~ X, data = d_pal, ncomp = 20, validation = "CV",
            segments = split(seq_along(pal), fold))
nc <- max(1, selectNcomp(fit, method = "onesigma", plot = FALSE))
pred_cv <- as.numeric(fit$validation$pred[, 1, nc])
obs <- d_pal$ic
cat("\nPLS of inorganic C (Palmira, CV by plot):", nc, "components\n")
cat("  R2 =", round(1 - sum((pred_cv - obs)^2) / sum((obs - mean(obs))^2), 2),
    "| RMSE =", round(sqrt(mean((pred_cv - obs)^2)), 3), "% C\n")

# Detection of "inorganic C > threshold"
present <- obs > IC_THRESHOLD
flagged <- pred_cv > IC_THRESHOLD
cat("  Samples with IC >", IC_THRESHOLD, "%:", sum(present), "of", length(obs), "\n")
cat("  Sensitivity (carbonate found when present):", round(mean(flagged[present]), 2), "\n")
cat("  Specificity (no carbonate found when absent):", round(mean(!flagged[!present]), 2), "\n")

# ---- 2. Spectral domain of Palmira (Hotelling T2 and Q residuals) --------------------------
pca <- prcomp(X[pal, ], center = TRUE, scale. = FALSE)
domain_stats <- function(Z) {
  z  <- scale(Z, center = pca$center, scale = FALSE)
  sc <- z %*% pca$rotation[, 1:N_PCS_DOMAIN]
  t2 <- rowSums(sweep(sc^2, 2, pca$sdev[1:N_PCS_DOMAIN]^2, "/"))
  q  <- rowSums((z - sc %*% t(pca$rotation[, 1:N_PCS_DOMAIN]))^2)
  cbind(T2 = t2, Q = q)
}
lim <- apply(domain_stats(X[pal, ]), 2, quantile, probs = DOMAIN_CONF)
st  <- domain_stats(X[tgt, ])
inside <- st[, "T2"] <= lim["T2"] & st[, "Q"] <= lim["Q"]

# ---- 3. Prediction of inorganic C for the target samples ------------------------------
d_tgt <- data.frame(id = seq_along(tgt))
d_tgt$X <- X[tgt, ]
ic_pred <- as.numeric(predict(fit, ncomp = nc, newdata = d_tgt))

# ---- 4. Lab signals per profile -------------------------------------------------------
t <- dat[tgt, c("sample_number", "location", "profile", "unit", "top", "bottom", "tc", "n_total", "label")]
t$ic_pred <- round(ic_pred, 3)
t$inside_domain <- inside
t$cn <- round(t$tc / t$n_total, 1)
t$subsoil <- t$top >= SUBSOIL_TOP

top_tc <- tapply(t$tc[t$top == 0], t$profile[t$top == 0], function(v) v[1])
t$tc_higher_than_topsoil <- t$tc > top_tc[t$profile] & t$top > 0
t$high_cn_subsoil <- t$subsoil & !is.na(t$cn) & t$cn > CN_SUBSOIL_MAX
t$spectral_signal <- t$inside_domain & t$ic_pred > IC_THRESHOLD

# ---- 5. Priority per profile ----------------------------------------------------------
prof <- do.call(rbind, lapply(split(t, t$profile), function(p) data.frame(
  location = as.character(p$location[1]), profile = p$profile[1], unit = p$unit[1],
  n_samples = nrow(p),
  max_ic_pred = max(p$ic_pred),
  spectral_signal = any(p$spectral_signal),
  outside_domain = any(!p$inside_domain),
  tc_increases = any(p$tc_higher_than_topsoil, na.rm = TRUE),
  high_cn_subsoil = any(p$high_cn_subsoil, na.rm = TRUE)
)))
prof$priority <- ifelse(prof$spectral_signal | (prof$tc_increases & prof$high_cn_subsoil), "high",
                        ifelse(prof$outside_domain | prof$tc_increases | prof$high_cn_subsoil, "check", "low"))
prof$priority <- factor(prof$priority, levels = c("high", "check", "low"))
prof <- prof[order(prof$priority, -prof$max_ic_pred), ]

cat("\nTarget samples inside the Palmira spectral domain:", sum(inside), "of", length(inside), "\n")
cat("\nProfiles by priority:\n")
print(table(prof$location, prof$priority))
cat("\nHigh-priority profiles:\n")
print(head(prof[prof$priority == "high", ], 20), row.names = FALSE)

write.csv(t, file.path(DIR_TABLES, "02b_carbonate_screening_samples.csv"), row.names = FALSE)
write.csv(prof, file.path(DIR_TABLES, "02b_carbonate_screening_profiles.csv"), row.names = FALSE)
cat("\nTables saved in", DIR_TABLES, "\n")