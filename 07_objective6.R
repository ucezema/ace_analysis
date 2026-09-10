# =============================================================================
# ACE-II Evaluation
# Script 07: OBJECTIVE 6 - Absenteeism and service delivery
#
# Run AFTER 00_setup.R and 01_preprocessing.R.
# Output: outputs/Objective6_ServiceDelivery.docx
#         outputs/Objective6_PatientVolumeTrend.pdf
# =============================================================================

stopifnot(exists("survey"), exists("ext"), exists("fac_mo"), exists("sv_phc"))

obj6_outputs <- list()

# Helper: majority value (mode) for a character vector
mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# =============================================================================
# 6.1 UNIVARIATE
# =============================================================================

# Patient volume distribution & trend
tbl_patv <- fac_mo |>
  dplyr::select(facility_category, period, monthly_patients) |>
  gtsummary::tbl_strata(
    strata = period,
    .tbl_fun = ~ .x |>
      gtsummary::tbl_summary(
        by = facility_category,
        type = list(monthly_patients ~ "continuous2"),
        statistic = list(monthly_patients ~ c("{mean} ({sd})",
                                                "{median} [{p25}, {p75}]")),
        digits = list(monthly_patients ~ 1),
        label = list(monthly_patients ~ "Monthly patient volume (ANC + Imm)"),
        missing = "no"
      ),
    .header = "**{strata}**"
  ) |>
  gtsummary::modify_caption("Table 6.1. Monthly patient volume (facility-month observations), by facility category and period")
obj6_outputs[["Table 6.1 Patient volume distribution"]] <- tbl_patv

# 13-month trend figure
trend_pat <- fac_mo |>
  dplyr::group_by(facility_category, month_date) |>
  dplyr::summarise(monthly_patients = mean(monthly_patients, na.rm = TRUE),
                   .groups = "drop")
p_trend <- ggplot2::ggplot(trend_pat,
                ggplot2::aes(month_date, monthly_patients,
                              colour = facility_category,
                              linetype = facility_category)) +
  ggplot2::geom_vline(xintercept = as.numeric(as.Date("2025-08-01")),
                       linetype = "dashed", colour = "grey40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  ggplot2::labs(title = "Monthly patient volume by facility category",
                 subtitle = "Dashed line = intervention start (Aug 2025)",
                 x = NULL, y = "Mean monthly patients per facility",
                 colour = "Facility", linetype = "Facility") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                  legend.position = "bottom")

fig_path <- file.path(OUTPUT_DIR, "Objective6_PatientVolumeTrend.pdf")
ggplot2::ggsave(fig_path, p_trend, width = 9, height = 5)
message("Wrote: ", fig_path)

# Q72 perceived service delivery (intervention sites only)
tbl_q72 <- survey |>
  dplyr::filter(facility_category == "Intervention") |>
  dplyr::select(q72_service) |>
  gtsummary::tbl_summary(
    label = list(q72_service ~ "Q72. Did reduced absenteeism improve service delivery?")
  ) |>
  gtsummary::modify_caption("Table 6.2. Perceived service delivery improvement (Q72), intervention sites only")
obj6_outputs[["Table 6.2 Q72 service delivery"]] <- tbl_q72

# Q60 staffing adequacy
tbl_q60 <- survey |>
  dplyr::select(facility_category, q60_staff_adequate) |>
  gtsummary::tbl_summary(
    by = facility_category,
    label = list(q60_staff_adequate ~ "Q60. Sufficient staff to ensure service coverage")
  ) |>
  gtsummary::modify_caption("Table 6.3. Staffing adequacy (Q60), by facility category - NB Q60 only filled for intervention sites due to ODK skip logic")
obj6_outputs[["Table 6.3 Q60 staffing adequacy"]] <- tbl_q60

# =============================================================================
# 6.2 BIVARIATE
# =============================================================================
biv6 <- list()

# (a) Monthly patient volume vs facility-level absenteeism rate (correlation)
res <- choose_corr_test(fac_mo$fac_absent_rate, fac_mo$monthly_patients)
biv6$pat_abs <- tibble::tibble(
  Outcome = "Monthly patient volume",
  Predictor = "Facility-month absenteeism rate (continuous)",
  Test = res$method, Statistic = res$estimate, p = res$p)

# (b) Patient volume vs facility category (t-test)
res <- choose_2group_test(fac_mo$monthly_patients, fac_mo$facility_category)
biv6$pat_fc <- tibble::tibble(
  Outcome = "Monthly patient volume",
  Predictor = "Facility category",
  Test = res$method, Statistic = res$statistic, p = res$p)

# (c) Q72 perceived improvement (ordinal) vs facility attendance rate (Spearman)
# Q72 only filled for intervention sites; merge facility-level attendance.
fac_avg_att <- fac_mo |>
  dplyr::group_by(phc) |>
  dplyr::summarise(fac_att = mean(fac_attend_rate, na.rm = TRUE),
                   .groups = "drop")
sv_q72 <- survey |>
  dplyr::filter(facility_category == "Intervention",
                !is.na(q72_score)) |>
  dplyr::left_join(fac_avg_att, by = "phc") |>
  dplyr::filter(!is.na(fac_att))

if (nrow(sv_q72) >= 5 && length(unique(sv_q72$q72_score)) >= 2) {
  ct <- suppressWarnings(cor.test(sv_q72$q72_score, sv_q72$fac_att,
                                    method = "spearman"))
  biv6$q72_att <- tibble::tibble(
    Outcome = "Q72 Perceived service delivery improvement (ordinal)",
    Predictor = "Facility-level attendance rate",
    Test = "Spearman rank correlation",
    Statistic = unname(ct$estimate), p = ct$p.value)
} else {
  biv6$q72_att <- tibble::tibble(
    Outcome = "Q72 Perceived service delivery improvement (ordinal)",
    Predictor = "Facility-level attendance rate",
    Test = "Not run",
    Statistic = NA_real_, p = NA_real_)
}

# (d) Patient volume vs staffing adequacy (binary, t-test)
# Q60 is at survey-respondent level; collapse to PHC-majority
phc_q60 <- survey |>
  dplyr::group_by(phc) |>
  dplyr::summarise(q60_majority = mode_chr(q60_staff_adequate),
                   .groups = "drop")
fac_with_q60 <- fac_mo |>
  dplyr::left_join(phc_q60, by = "phc") |>
  dplyr::mutate(q60_yn = factor(dplyr::if_else(q60_majority == "Yes", "Yes",
                                                 dplyr::if_else(q60_majority == "No", "No",
                                                                NA_character_)),
                                 levels = c("No","Yes")))

if (length(unique(na.omit(fac_with_q60$q60_yn))) == 2) {
  res <- choose_2group_test(fac_with_q60$monthly_patients, fac_with_q60$q60_yn)
  biv6$pat_q60 <- tibble::tibble(
    Outcome = "Monthly patient volume",
    Predictor = "Q60 Staffing adequacy (PHC majority)",
    Test = res$method, Statistic = res$statistic, p = res$p)
} else {
  biv6$pat_q60 <- tibble::tibble(
    Outcome = "Monthly patient volume",
    Predictor = "Q60 Staffing adequacy (PHC majority)",
    Test = "Not run",
    Statistic = NA_real_, p = NA_real_)
}

biv6_combined <- dplyr::bind_rows(biv6) |>
  dplyr::select(Outcome, Predictor, Test, Statistic, p) |>
  dplyr::mutate(p = fmt_p(p),
                Statistic = ifelse(is.na(Statistic), NA,
                                    formatC(Statistic, digits = 3, format = "f")))

ft_biv6 <- flextable::flextable(biv6_combined) |>
  flextable::set_caption("Table 6.4. Bivariate associations - absenteeism and service delivery") |>
  flextable::autofit()
obj6_outputs[["Table 6.4 Bivariate associations"]] <- ft_biv6

# =============================================================================
# 6.3 MULTIVARIATE
# =============================================================================

# --- 6.3.1 Mixed-effects linear regression: patient volume -----------------
# DV: monthly patient volume (log-transformed if heavily skewed)
# Random intercept: phc
fac_for_mix <- fac_mo |>
  dplyr::filter(!is.na(monthly_patients), !is.na(fac_absent_rate)) |>
  dplyr::mutate(month_num = as.numeric(month_date - as.Date("2025-02-01")) / 30.44)

# Quick skewness check
skew_p <- if (length(fac_for_mix$monthly_patients) >= 3 &&
               length(fac_for_mix$monthly_patients) <= 5000) {
  tryCatch(shapiro.test(fac_for_mix$monthly_patients)$p.value,
            error = function(e) NA_real_)
} else NA_real_
use_log <- !is.na(skew_p) && skew_p < 0.05
if (use_log) {
  fac_for_mix <- fac_for_mix |> dplyr::mutate(log_patients = log1p(monthly_patients))
  dv_str <- "log_patients"
  dv_label <- "log(1 + monthly patients)"
} else {
  dv_str <- "monthly_patients"
  dv_label <- "monthly patients"
}

mix_form <- as.formula(paste0(dv_str,
                              " ~ fac_absent_rate + facility_category * period + ",
                              "month_num + lga + (1 | phc)"))
mix_fit <- tryCatch(
  lmerTest::lmer(mix_form, data = fac_for_mix, REML = TRUE,
                  control = lme4::lmerControl(optimizer = "bobyqa")),
  error = function(e) NULL,
  warning = function(w) NULL
)

if (!is.null(mix_fit)) {
  fixed_df <- broom.mixed::tidy(mix_fit, effects = "fixed", conf.int = TRUE) |>
    dplyr::mutate(p.value = fmt_p(p.value),
                  dplyr::across(c(estimate, std.error, statistic, conf.low, conf.high),
                                ~ formatC(.x, digits = 3, format = "f"))) |>
    dplyr::select(Term = term, Estimate = estimate, SE = std.error,
                  `t / df` = statistic, `95% CI low` = conf.low,
                  `95% CI high` = conf.high, p = p.value)

  ft_mix6 <- flextable::flextable(fixed_df) |>
    flextable::set_caption(sprintf("Table 6.5. Mixed-effects linear regression: %s ~ facility-level absenteeism + intervention DiD. Random intercept: PHC.",
                                    dv_label)) |>
    flextable::autofit()
  obj6_outputs[["Table 6.5 Mixed-effects model patient volume"]] <- ft_mix6
} else {
  obj6_outputs[["Table 6.5 Mixed-effects model"]] <-
    flextable::flextable(tibble::tibble(
      Note = "Mixed-effects model failed to converge."
    )) |> flextable::autofit()
}

# --- 6.3.2 Ordinal logistic regression: Q72 perceived improvement ----------
# Q72 has limited variance: in this dataset, all 39 intervention respondents
# answered "Yes, significantly". The model cannot separate an ordinal outcome
# with one realised level - we report a note.
sv_q72_full <- survey |>
  dplyr::filter(facility_category == "Intervention", !is.na(q72_score)) |>
  dplyr::mutate(q72_ord = factor(q72_score, ordered = TRUE))

if (nlevels(sv_q72_full$q72_ord) >= 2 && nrow(sv_q72_full) >= 10) {
  # Merge facility-level attendance
  sv_q72_lr <- sv_q72_full |>
    dplyr::left_join(fac_avg_att, by = "phc")

  ord_fit <- tryCatch(
    MASS::polr(q72_ord ~ fac_att + cadre + fac_location +
                  pci_capabil + exposure_idx,
                data = sv_q72_lr, Hess = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(ord_fit)) {
    tbl_ord6 <- ord_fit |>
      gtsummary::tbl_regression(exponentiate = TRUE) |>
      gtsummary::modify_caption("Table 6.6. Ordinal logistic regression - predictors of Q72 perceived service delivery improvement")
    obj6_outputs[["Table 6.6 Ordinal logit Q72"]] <- tbl_ord6
  } else {
    obj6_outputs[["Table 6.6 Ordinal logit Q72"]] <-
      flextable::flextable(tibble::tibble(
        Note = "Ordinal logit model failed to converge - likely too few cases per category."
      )) |> flextable::autofit()
  }
} else {
  obj6_outputs[["Table 6.6 Ordinal logit Q72"]] <-
    flextable::flextable(tibble::tibble(
      Note = sprintf("Ordinal logit on Q72 not run: outcome has only %d realised category in n = %d intervention respondents (e.g. 'Yes, significantly' for all). The DAP-specified model cannot be fitted with this distribution.",
                      nlevels(sv_q72_full$q72_ord), nrow(sv_q72_full))
    )) |> flextable::autofit()
}

# =============================================================================
# 6.4 Write to docx
# =============================================================================
out_path <- file.path(OUTPUT_DIR, "Objective6_ServiceDelivery.docx")
save_to_docx(obj6_outputs, out_path,
             title = "Objective 6 - Absenteeism and service delivery")
