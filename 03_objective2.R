# =============================================================================
# ACE-II Evaluation
# Script 03: OBJECTIVE 2 - Effect of the intervention on absenteeism
#
# Run AFTER 00_setup.R and 01_preprocessing.R.
# Output: outputs/Objective2_Intervention_Effect.docx
#         outputs/Objective2_Figures.pdf  (line plots)
# =============================================================================

stopifnot(exists("survey"), exists("ext"), exists("fac_mo"))

obj2_outputs <- list()

# =============================================================================
# 2.1 UNIVARIATE -- pre-post comparison & monthly trend descriptives
# =============================================================================

# --- 2.1.1 Pre-post means per facility category (worker-month level) -------
prepost_tbl <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(period)) |>
  dplyr::group_by(facility_category, period) |>
  dplyr::summarise(
    n_obs       = dplyr::n(),
    abs_mean    = mean(absent_rate,    na.rm = TRUE),
    abs_sd      = sd(absent_rate,      na.rm = TRUE),
    abs_median  = median(absent_rate,  na.rm = TRUE),
    att_mean    = mean(attend_rate,    na.rm = TRUE),
    att_sd      = sd(attend_rate,      na.rm = TRUE),
    tard_mean   = mean(total_min_late, na.rm = TRUE),
    tard_sd     = sd(total_min_late,   na.rm = TRUE),
    tard_median = median(total_min_late,na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `Absenteeism rate, mean (SD)` = sprintf("%.2f (%.2f)", abs_mean, abs_sd),
    `Absenteeism rate, median`    = sprintf("%.2f", abs_median),
    `Attendance rate, mean (SD)`  = sprintf("%.2f (%.2f)", att_mean, att_sd),
    `Tardiness (min), mean (SD)`  = sprintf("%.2f (%.2f)", tard_mean, tard_sd),
    `Tardiness (min), median`     = sprintf("%.2f", tard_median)
  ) |>
  dplyr::select(`Facility category` = facility_category,
                Period = period, n = n_obs,
                dplyr::starts_with("Absenteeism"),
                dplyr::starts_with("Attendance"),
                dplyr::starts_with("Tardiness"))

ft_prepost <- flextable::flextable(prepost_tbl) |>
  flextable::set_caption("Table 2.1. Pre- vs intervention-period attendance metrics, by facility category (worker-month observations)") |>
  flextable::autofit()
obj2_outputs[["Table 2.1 Pre-post means by arm"]] <- ft_prepost

# --- 2.1.2 Perceived effectiveness Q68-Q73 (intervention sites only) -------
sv_intv <- survey |> dplyr::filter(facility_category == "Intervention")

tbl_perc <- sv_intv |>
  dplyr::select(q68_abs_reduced, q69_punct_improved, q70_leave_reduced,
                q71_unintended, q72_service, q73_commit_improved) |>
  gtsummary::tbl_summary(
    missing = "ifany",
    label = list(
      q68_abs_reduced     ~ "Q68. Extent absenteeism reduced",
      q69_punct_improved  ~ "Q69. Punctuality improved",
      q70_leave_reduced   ~ "Q70. Leaving early reduced",
      q71_unintended      ~ "Q71. Unintended negative consequences",
      q72_service         ~ "Q72. Service delivery improved",
      q73_commit_improved ~ "Q73. Staff commitment improved"
    )
  ) |>
  gtsummary::modify_caption("Table 2.2. Perceived effectiveness of the intervention (Q68-Q73), intervention sites only (n = 39)")
obj2_outputs[["Table 2.2 Perceived effectiveness"]] <- tbl_perc

# --- 2.1.3 Monthly trend (figure) ----------------------------------------------
trend_data <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(month_date)) |>
  dplyr::group_by(facility_category, month_date) |>
  dplyr::summarise(absent_rate = mean(absent_rate, na.rm = TRUE),
                   attend_rate = mean(attend_rate, na.rm = TRUE),
                   tard_min    = mean(total_min_late, na.rm = TRUE),
                   .groups = "drop")

p_abs <- ggplot2::ggplot(trend_data,
                         ggplot2::aes(month_date, absent_rate,
                                      colour = facility_category,
                                      linetype = facility_category)) +
  ggplot2::geom_vline(xintercept = as.numeric(as.Date("2025-08-01")),
                      linetype = "dashed", colour = "grey40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(title = "Monthly mean absenteeism rate, by facility category",
                subtitle = "Dashed line = intervention start (Aug 2025)",
                x = NULL, y = "Absenteeism rate (%)",
                colour = "Facility", linetype = "Facility") +
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.position = "bottom")

p_att <- ggplot2::ggplot(trend_data,
                         ggplot2::aes(month_date, attend_rate,
                                      colour = facility_category,
                                      linetype = facility_category)) +
  ggplot2::geom_vline(xintercept = as.numeric(as.Date("2025-08-01")),
                      linetype = "dashed", colour = "grey40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(title = "Monthly mean attendance rate, by facility category",
                x = NULL, y = "Attendance rate (%)",
                colour = "Facility", linetype = "Facility") +
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.position = "bottom")

p_tard <- ggplot2::ggplot(trend_data,
                          ggplot2::aes(month_date, tard_min,
                                       colour = facility_category,
                                       linetype = facility_category)) +
  ggplot2::geom_vline(xintercept = as.numeric(as.Date("2025-08-01")),
                      linetype = "dashed", colour = "grey40") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::labs(title = "Monthly mean tardiness (total minutes late), by facility category",
                x = NULL, y = "Total minutes late (per worker-month)",
                colour = "Facility", linetype = "Facility") +
  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                 legend.position = "bottom")

# Save the three figures to a single PDF for the appendix
fig_path <- file.path(OUTPUT_DIR, "Objective2_Figures.pdf")
pdf(fig_path, width = 9, height = 5)
print(p_abs)
print(p_att)
print(p_tard)
dev.off()
message("Wrote: ", fig_path)

# =============================================================================
# 2.2 BIVARIATE -- pre/post & between-arm tests
# =============================================================================
biv2 <- list()

# --- (a) Within-intervention paired test: facility-level pre vs post --------
# We need paired data: each facility gets ONE pre value (mean of pre months)
# and ONE post value (mean of intervention months). Pairing is by phc.
fac_paired <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(period),
                facility_category == "Intervention") |>
  dplyr::group_by(phc, period) |>
  dplyr::summarise(absent_rate = mean(absent_rate, na.rm = TRUE),
                   .groups = "drop") |>
  tidyr::pivot_wider(names_from = period, values_from = absent_rate) |>
  tidyr::drop_na()

if (nrow(fac_paired) >= 2) {
  diff_vec <- fac_paired$Intervention - fac_paired$`Pre-intervention`
  shap_p <- if (length(diff_vec) >= 3 && length(diff_vec) <= 5000)
    shapiro.test(diff_vec)$p.value else 0
  if (shap_p > 0.05) {
    tt <- t.test(fac_paired$Intervention, fac_paired$`Pre-intervention`,
                 paired = TRUE)
    biv2$paired <- tibble::tibble(
      Outcome = "Absenteeism rate (%)",
      Comparison = "Pre vs Intervention (within intervention sites)",
      Test = "Paired t-test",
      Statistic = unname(tt$statistic),
      p = tt$p.value,
      `n facilities` = nrow(fac_paired)
    )
  } else {
    wt <- suppressWarnings(wilcox.test(fac_paired$Intervention,
                                       fac_paired$`Pre-intervention`,
                                       paired = TRUE))
    biv2$paired <- tibble::tibble(
      Outcome = "Absenteeism rate (%)",
      Comparison = "Pre vs Intervention (within intervention sites)",
      Test = "Wilcoxon signed-rank",
      Statistic = unname(wt$statistic),
      p = wt$p.value,
      `n facilities` = nrow(fac_paired)
    )
  }
}

# --- (b) Change-score (Δ = post - pre) compared between arms ---------------
fac_delta <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(period)) |>
  dplyr::group_by(phc, facility_category, period) |>
  dplyr::summarise(absent_rate = mean(absent_rate, na.rm = TRUE),
                   .groups = "drop") |>
  tidyr::pivot_wider(names_from = period, values_from = absent_rate) |>
  dplyr::mutate(delta = Intervention - `Pre-intervention`) |>
  tidyr::drop_na(delta)

res <- choose_2group_test(fac_delta$delta, fac_delta$facility_category)
biv2$delta <- tibble::tibble(
  Outcome = "Change in absenteeism rate (post - pre)",
  Comparison = "Intervention vs Control (facility-level)",
  Test = res$method,
  Statistic = res$statistic,
  p = res$p,
  `n facilities` = nrow(fac_delta)
)

# --- (c) Q68 perceived reduction vs facility category (Mann-Whitney) -------
# In this dataset Q68 is only filled for intervention; cannot compare between
# arms. We document this rather than running an invalid test.
q68_by_arm <- table(survey$facility_category, survey$q68_abs_reduced, useNA = "no")
if (sum(q68_by_arm[, ] > 0) > 0 && nrow(q68_by_arm) == 2 &&
    rowSums(q68_by_arm)[1] > 0 && rowSums(q68_by_arm)[2] > 0) {
  q68_levs <- c("No reduction","Slight reduction","Moderate reduction",
                "Significant reduction")
  sv_test <- survey |>
    dplyr::mutate(q68_ord = factor(q68_abs_reduced, levels = q68_levs,
                                   ordered = TRUE),
                  q68_num = as.integer(q68_ord))
  wt <- suppressWarnings(wilcox.test(q68_num ~ facility_category, data = sv_test))
  biv2$q68 <- tibble::tibble(
    Outcome = "Q68 Perceived absenteeism reduction (ordinal)",
    Comparison = "Intervention vs Control",
    Test = "Mann-Whitney U",
    Statistic = unname(wt$statistic), p = wt$p.value,
    `n facilities` = sum(q68_by_arm))
} else {
  biv2$q68 <- tibble::tibble(
    Outcome = "Q68 Perceived absenteeism reduction (ordinal)",
    Comparison = "Intervention vs Control",
    Test = "Not run",
    Statistic = NA_real_, p = NA_real_,
    `n facilities` = NA_integer_)
}

# --- (d) Q69 punctuality improved (binary) vs facility category (chi-sq) ---
q69_tab <- table(survey$facility_category,
                 ifelse(survey$q69_punct_improved == "Yes", "Yes", "No"))
if (all(dim(q69_tab) >= 2) && all(rowSums(q69_tab) > 0)) {
  res69 <- choose_chisq(survey$facility_category,
                        ifelse(survey$q69_punct_improved == "Yes","Yes","No"))
  biv2$q69 <- tibble::tibble(
    Outcome = "Q69 Punctuality improved (binary)",
    Comparison = "Intervention vs Control",
    Test = res69$method,
    Statistic = res69$statistic, p = res69$p,
    `n facilities` = sum(q69_tab))
} else {
  biv2$q69 <- tibble::tibble(
    Outcome = "Q69 Punctuality improved (binary)",
    Comparison = "Intervention vs Control",
    Test = "Not run (Q69 only filled for intervention sites)",
    Statistic = NA_real_, p = NA_real_,
    `n facilities` = NA_integer_)
}

biv2_combined <- dplyr::bind_rows(biv2) |>
  dplyr::mutate(p = fmt_p(p),
                Statistic = ifelse(is.na(Statistic), NA,
                                   formatC(Statistic, digits = 3, format = "f")))

ft_biv2 <- flextable::flextable(biv2_combined) |>
  flextable::set_caption("Table 2.3. Bivariate tests of intervention effect (paired and unpaired)") |>
  flextable::autofit()
obj2_outputs[["Table 2.3 Bivariate tests"]] <- ft_biv2

# =============================================================================
# 2.3 MULTIVARIATE
# =============================================================================

# --- 2.3.1 DiD linear regression -----------------------------------------------
# DV: worker-month absent_rate
# Key term: facility_category x period (post)
ext_for_did <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(period),
                !is.na(absent_rate), !is.na(cadre)) |>
  dplyr::left_join(sv_phc |> dplyr::select(phc, fac_location), by = "phc")

did_lm <- lm(absent_rate ~ facility_category * period + cadre + fac_location,
             data = ext_for_did)

# Pull the interaction term as the DiD estimator
did_tbl <- did_lm |>
  gtsummary::tbl_regression(
    intercept = TRUE,
    label = list(
      facility_category ~ "Facility category",
      period            ~ "Period",
      cadre             ~ "Cadre",
      fac_location          ~ "Location",
      `facility_category:period` ~ "Facility category x Period (DiD)"
    )) |>
  gtsummary::add_glance_source_note(include = c(r.squared, adj.r.squared,
                                                nobs, p.value)) |>
  gtsummary::modify_caption("Table 2.4. Difference-in-differences linear regression - intervention effect on monthly absenteeism rate. The 'Facility category x Period' coefficient is the DiD estimator.")
obj2_outputs[["Table 2.4 DiD linear regression"]] <- did_tbl

# Parallel-trends visual + statistical check (pre-period only)
# (1) Visual already in p_abs above
# (2) Statistical: in the pre-period, facility_category x month should be NS
pre_only <- ext_for_did |>
  dplyr::filter(period == "Pre-intervention") |>
  dplyr::mutate(month_num = as.numeric(month_date - min(month_date, na.rm = TRUE)) / 30.44)
pt_lm <- lm(absent_rate ~ facility_category * month_num + cadre + fac_location,
            data = pre_only)
pt_int <- broom::tidy(pt_lm) |>
  dplyr::filter(stringr::str_detect(term, ":")) |>
  dplyr::mutate(p.value = fmt_p(p.value),
                across(c(estimate, std.error, statistic), ~ formatC(.x, digits=3, format="f")))
ft_pt <- flextable::flextable(pt_int) |>
  flextable::set_caption("Table 2.4a. Parallel-trends pre-period check. The interaction (facility_category x month_num) should be non-significant for the parallel-trends assumption to hold.") |>
  flextable::autofit()
obj2_outputs[["Table 2.4a Parallel-trends pre-period check"]] <- ft_pt

# --- 2.3.2 Propensity-score model -----------------------------------------
# DAP says: model probability of being in intervention based on baseline
# observable characteristics. We use facility-level pre-period covariates
# (pre absenteeism, % female, mean dependents, governance index from survey,
# etc.), match on PS, then re-estimate the DiD on the matched sample.
#
# Because this is a small study (9 facilities in extraction; 12 survey PHCs),
# matching is more illustrative than statistically powerful. We still report
# the SMD-balance table because it's standard for propensity work.

# Build facility-level baseline frame
fac_baseline <- ext |>
  dplyr::filter(period == "Pre-intervention") |>
  dplyr::group_by(phc, facility_category) |>
  dplyr::summarise(
    pre_abs_rate    = mean(absent_rate,   na.rm = TRUE),
    pre_tard_min    = mean(total_min_late, na.rm = TRUE),
    pre_patients    = mean(monthly_patients, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(sv_phc, by = "phc")

# Treatment indicator
fac_baseline <- fac_baseline |>
  dplyr::mutate(treat = as.integer(facility_category == "Intervention"))

ps_vars <- c("pre_abs_rate","pre_tard_min","governance_idx_phc",
             "sanction_idx_phc","pct_walk_phc","pct_female_phc")
ps_data <- fac_baseline |>
  dplyr::select(phc, treat, dplyr::all_of(ps_vars)) |>
  tidyr::drop_na()

if (nrow(ps_data) >= 4 && sum(ps_data$treat == 1) >= 2 &&
    sum(ps_data$treat == 0) >= 2) {
  
  ps_form <- as.formula(paste("treat ~", paste(ps_vars, collapse = " + ")))
  
  # Logistic propensity model
  ps_fit <- tryCatch(
    glm(ps_form, data = ps_data, family = binomial()),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  # Match 1:1 nearest-neighbour with replacement (small n means we need replacement)
  m_out <- tryCatch(
    MatchIt::matchit(ps_form, data = ps_data, method = "nearest",
                     replace = TRUE, ratio = 1),
    error = function(e) NULL
  )
  
  if (!is.null(m_out)) {
    sumtab <- summary(m_out, standardize = TRUE)
    # MatchIt 4.x: $sum.matched; older versions may differ. Guard:
    sm <- if (!is.null(sumtab$sum.matched)) sumtab$sum.matched else
      if (!is.null(sumtab$sum.all))     sumtab$sum.all     else NULL
    if (!is.null(sm)) {
      bal_df <- as.data.frame(sm) |>
        tibble::rownames_to_column("Variable") |>
        dplyr::mutate(dplyr::across(where(is.numeric),
                                    ~ formatC(.x, digits=3, format="f")))
      ft_bal <- flextable::flextable(bal_df) |>
        flextable::set_caption("Table 2.5. Standardised mean differences after 1:1 propensity-score matching (with replacement). |SMD| < 0.10 typically considered well-balanced.") |>
        flextable::autofit()
      obj2_outputs[["Table 2.5 PS matching balance"]] <- ft_bal
    }
    
    # Apply weights to the worker-month DiD model
    matched_data <- MatchIt::match.data(m_out)
    ext_matched <- ext_for_did |>
      dplyr::inner_join(matched_data |> dplyr::select(phc, weights),
                        by = "phc")
    
    did_lm_ps <- lm(absent_rate ~ facility_category * period + cadre + fac_location,
                    data = ext_matched, weights = weights)
    did_ps_tbl <- did_lm_ps |>
      gtsummary::tbl_regression(intercept = TRUE) |>
      gtsummary::add_glance_source_note(include = c(r.squared, nobs)) |>
      gtsummary::modify_caption("Table 2.6. Difference-in-differences linear regression on PS-matched sample (worker-month observations weighted by matching weights).")
    obj2_outputs[["Table 2.6 PS-matched DiD"]] <- did_ps_tbl
  } else {
    obj2_outputs[["Table 2.5 PS matching balance"]] <-
      flextable::flextable(tibble::tibble(
        Note = "Propensity-score matching failed (likely insufficient overlap)."
      )) |> flextable::autofit()
  }
} else {
  obj2_outputs[["Table 2.5 PS matching balance"]] <-
    flextable::flextable(tibble::tibble(
      Note = sprintf("Propensity-score matching skipped: only %d facilities with complete baseline data (need >=4 with >=2 per arm).",
                     nrow(ps_data))
    )) |> flextable::autofit()
}

# --- 2.3.3 Mixed-effects (multilevel) linear regression ---------------------
# Random intercepts for phc (workers nested in facilities) and worker.
# But there's no stable worker_id across months (worker_index is reset per
# extraction submission). So we use phc-level random intercept only.
#
# We also include a continuous month-since-start to capture time trend
# additively, while facility_category x period captures the discrete shift.
ext_for_mixed <- ext_for_did |>
  dplyr::mutate(month_num = as.numeric(month_date - as.Date("2025-02-01")) / 30.44)

mix_fit <- tryCatch(
  lmerTest::lmer(absent_rate ~ facility_category * period + month_num +
                   cadre + fac_location + (1 | phc),
                 data = ext_for_mixed,
                 REML = TRUE,
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
  
  ft_mix <- flextable::flextable(fixed_df) |>
    flextable::set_caption("Table 2.7. Mixed-effects linear regression - fixed effects. Random intercept: PHC. Worker-month observations (n facilities = number of unique PHCs).") |>
    flextable::autofit()
  obj2_outputs[["Table 2.7 Mixed-effects fixed effects"]] <- ft_mix
  
  # Random effect variance
  vc <- as.data.frame(lme4::VarCorr(mix_fit))
  vc_df <- tibble::tibble(
    Group = vc$grp,
    Term = vc$var1,
    Variance = formatC(vc$vcov, digits = 3, format = "f"),
    SD = formatC(vc$sdcor, digits = 3, format = "f")
  )
  ft_re <- flextable::flextable(vc_df) |>
    flextable::set_caption("Table 2.7a. Mixed-effects model - random-effects variance components") |>
    flextable::autofit()
  obj2_outputs[["Table 2.7a Mixed-effects variance components"]] <- ft_re
} else {
  obj2_outputs[["Table 2.7 Mixed-effects model"]] <-
    flextable::flextable(tibble::tibble(
      Note = "Mixed-effects model failed to converge."
    )) |> flextable::autofit()
}

# --- 2.3.4 Ordinal logistic regression on Q68 ------------------------------
# Q68 is only present for intervention sites in this dataset, so the model
# can only test whether covariates within the intervention arm predict
# perceived effectiveness. We document this clearly.
q68_levs_full <- c("No reduction","Slight reduction","Moderate reduction",
                   "Significant reduction")
sv_ord <- survey |>
  dplyr::filter(!is.na(q68_abs_reduced)) |>
  dplyr::mutate(q68_ord = factor(q68_abs_reduced, levels = q68_levs_full,
                                 ordered = TRUE))

if (nrow(sv_ord) >= 10 && length(unique(sv_ord$q68_ord)) >= 2) {
  # Reduced predictor set given small n (~39 intervention respondents)
  sv_ord_d <- sv_ord |>
    dplyr::mutate(governance_idx = as.numeric(governance_idx),
                  exposure_idx   = as.numeric(exposure_idx))
  
  ord_fit <- tryCatch(
    MASS::polr(q68_ord ~ cadre + gender + fac_location + governance_idx,
               data = sv_ord_d, Hess = TRUE, method = "logistic"),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (!is.null(ord_fit)) {
    ord_tbl <- ord_fit |>
      gtsummary::tbl_regression(exponentiate = TRUE) |>
      gtsummary::modify_caption("Table 2.8. Ordinal logistic regression of Q68 (perceived absenteeism reduction). Intervention sites only (n approx 39).")
    obj2_outputs[["Table 2.8 Ordinal logit Q68"]] <- ord_tbl
    
    # Brant test for proportional-odds assumption
    brant_res <- tryCatch(
      brant::brant(ord_fit, by.var = FALSE),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(brant_res)) {
      bdf <- as.data.frame(brant_res) |>
        tibble::rownames_to_column("Variable")
      # Column names from brant: 'X2', 'df', 'probability'
      if ("probability" %in% names(bdf)) {
        bdf$probability <- fmt_p(bdf$probability)
      }
      ft_brant <- flextable::flextable(bdf) |>
        flextable::set_caption("Table 2.8a. Brant test for proportional-odds assumption. Variable-level probability > 0.05 supports the assumption; Omnibus probability > 0.05 supports it overall.") |>
        flextable::autofit()
      obj2_outputs[["Table 2.8a Brant test"]] <- ft_brant
    }
  } else {
    obj2_outputs[["Table 2.8 Ordinal logit"]] <-
      flextable::flextable(tibble::tibble(
        Note = "Ordinal logit model failed to converge - likely too few cases per category."
      )) |> flextable::autofit()
  }
} else {
  obj2_outputs[["Table 2.8 Ordinal logit"]] <-
    flextable::flextable(tibble::tibble(
      Note = "Ordinal logit not run: too few unique levels of Q68 in the data."
    )) |> flextable::autofit()
}

# =============================================================================
# 2.4 Write to docx
# =============================================================================
out_path <- file.path(OUTPUT_DIR, "Objective2_Intervention_Effect.docx")
save_to_docx(obj2_outputs, out_path,
             title = "Objective 2 - Effect of the intervention on absenteeism")