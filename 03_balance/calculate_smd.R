# Standardised mean difference ------------------------------------------------
#
# smd()      pure, vectorised formula on estimates and SDs.
# add_smd()  appends `smd` to an aligned table (see balance_schema.R).
#
# One formula serves every row_type. Summary rows carry mean and SD; level and
# missing rows carry a proportion and sqrt(p(1-p)) as SD, so the pooled-SD
# formula below reduces to the usual proportion SMD. Dispatching on row_type
# would duplicate code for no gain.
#
# Sign convention: IPD minus SLD. Positive means the IPD is higher.
#
# Degenerate denominators (both SDs zero):
#   est_ipd == est_sld  ->  0    (identical constants, no imbalance)
#   otherwise           ->  +/-Inf (e.g. 100% vs 0%); reporting formats it.
# Any NA input gives NA.

smd <- function(est_ipd, sd_ipd, est_sld, sd_sd) {
  pooled <- sqrt((sd_ipd^2 + sd_sd^2) / 2)
  diff   <- est_ipd - est_sld
  out    <- diff / pooled
  out[!is.na(diff) & diff == 0] <- 0
  out
}

add_smd <- function(aligned) {
  stopifnot(all(ALIGNED_COLUMNS %in% names(aligned)))
  aligned$smd <- smd(aligned$ipd_est, aligned$ipd_sd, aligned$sld_est, aligned$sld_sd)
  aligned[BALANCE_COLUMNS]
}
