# =============================================================================
# ACE-II Evaluation
# Script 02: OBJECTIVE 1 - Patterns and prevalence of absenteeism
#
# Run AFTER 00_setup.R and 01_preprocessing.R.
# Output: outputs/Objective1_Patterns_Prevalence.docx
# =============================================================================

stopifnot(exists("survey"), exists("ext"))

obj1_outputs <- list()  # we'll fill this list, then write everything to docx

# =============================================================================
# 1.1 UNIVARIATE -- descriptive statistics
# =============================================================================

# --- 1.1.1 Sociodemographic + facility-level descriptives, stratified by arm
tbl_demo <- survey |>
  dplyr::select(facility_category, gender, fac_location, lga, cadre,
                n_children_u12, n_adult_dep, q10_walk, q11_register,
                q12_oic_check) |>
  gtsummary::tbl_summary(
    by = facility_category,
    missing = "ifany",
    label = list(
      gender         ~ "Gender",
      fac_location       ~ "Location of facility",
      lga            ~ "LGA",
      cadre          ~ "Cadre",
      n_children_u12 ~ "Children <12 years (n)",
      n_adult_dep    ~ "Adult dependents (n)",
      q10_walk       ~ "Lives within walking distance",
      q11_register   ~ "Attendance register kept (Q11)",
      q12_oic_check  ~ "OIC check frequency (Q12)"
    ),
    type = list(gtsummary::all_continuous() ~ "continuous2"),
    statistic = list(
      gtsummary::all_continuous()  ~ c("{mean} ({sd})",
                                       "{median} [{p25}, {p75}]"),
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(gtsummary::all_continuous() ~ 2)
  ) |>
  gtsummary::add_overall() |>
  gtsummary::add_n() |>
  gtsummary::modify_header(label = "**Characteristic**") |>
  gtsummary::modify_caption("Table 1.1. Sociodemographic and facility characteristics of surveyed health workers, by facility category")

obj1_outputs[["Table 1.1 Sociodemographic & facility characteristics"]] <- tbl_demo

# --- 1.1.2 Absenteeism characteristics (Q18a-c, Q19, Q23-25, Q26-28)
tbl_abs_chars <- survey |>
  dplyr::select(facility_category,
                q18a_late, q18b_leave_early, q18c_full_day,
                q19_most_common,
                q23a_sanc_late, q23b_sanc_leave, q23c_sanc_fullday,
                q25_sanc_used,
                q26_inc_money, q26_inc_food, q26_inc_recog,
                q28_inc_freq) |>
  dplyr::mutate(dplyr::across(c(q18a_late, q18b_leave_early, q18c_full_day,
                                q23a_sanc_late, q23b_sanc_leave, q23c_sanc_fullday,
                                q26_inc_money, q26_inc_food, q26_inc_recog),
                              ~ factor(.x, levels = c(0,1), labels = c("No","Yes")))) |>
  gtsummary::tbl_summary(
    by = facility_category,
    missing = "ifany",
    label = list(
      q18a_late         ~ "Q18a. Coming late observed",
      q18b_leave_early  ~ "Q18b. Leaving early observed",
      q18c_full_day     ~ "Q18c. Full-day absence observed",
      q19_most_common   ~ "Q19. Most common type",
      q23a_sanc_late    ~ "Q23a. Sanction for lateness exists",
      q23b_sanc_leave   ~ "Q23b. Sanction for leaving early exists",
      q23c_sanc_fullday ~ "Q23c. Sanction for full-day absence exists",
      q25_sanc_used     ~ "Q25. Sanctions used in past 6 months",
      q26_inc_money     ~ "Q26. Money incentive available",
      q26_inc_food      ~ "Q26. Food incentive available",
      q26_inc_recog     ~ "Q26. Recognition incentive available",
      q28_inc_freq      ~ "Q28. Incentives provided regularly"
    )
  ) |>
  gtsummary::add_overall() |>
  gtsummary::modify_caption("Table 1.2. Absenteeism characteristics, sanctions, and incentives, by facility category")

obj1_outputs[["Table 1.2 Absenteeism, sanctions, incentives"]] <- tbl_abs_chars

# --- 1.1.3 Worker-month attendance metrics from extraction tool
# Test normality once and report mean(SD) AND median[IQR] together.
shapiro_safe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || length(x) > 5000) return(NA_real_)
  tryCatch(shapiro.test(x)$p.value, error = function(e) NA_real_)
}

attendance_metrics <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(period)) |>
  dplyr::select(facility_category, period,
                absent_rate, attend_rate, total_min_late, avg_min_late,
                days_scheduled, days_present, monthly_patients)

tbl_attendance <- attendance_metrics |>
  gtsummary::tbl_strata(
    strata = period,
    .tbl_fun = ~ .x |>
      gtsummary::tbl_summary(
        by = facility_category,
        type = list(gtsummary::all_continuous() ~ "continuous2"),
        statistic = list(gtsummary::all_continuous() ~ c(
          "{mean} ({sd})",
          "{median} [{p25}, {p75}]"
        )),
        label = list(
          absent_rate      ~ "Absenteeism rate (%)",
          attend_rate      ~ "Attendance rate (%)",
          total_min_late   ~ "Total minutes late",
          avg_min_late     ~ "Average minutes late per incident",
          days_scheduled   ~ "Days scheduled",
          days_present     ~ "Days present",
          monthly_patients ~ "Monthly patient volume"
        ),
        digits = list(gtsummary::all_continuous() ~ 2),
        missing = "no"
      ),
    .header = "**{strata}**"
  ) |>
  gtsummary::modify_caption(paste("Table 1.3. Attendance metrics from extraction tool",
                                  "(worker-month observations), by facility category and period.",
                                  "Note: 'Days absent with permission', 'without permission' and",
                                  "'Days late without permission' columns are empty in the source",
                                  "file, so total absence is derived as scheduled minus present."))

obj1_outputs[["Table 1.3 Attendance metrics"]] <- tbl_attendance

# Normality summary table - one-page summary that justifies later test choices
norm_tbl <- attendance_metrics |>
  dplyr::summarise(dplyr::across(c(absent_rate, attend_rate, total_min_late,
                                   avg_min_late, monthly_patients),
                                 list(n = ~ sum(is.finite(.x)),
                                      shapiro_p = ~ shapiro_safe(.x)))) |>
  tidyr::pivot_longer(everything(),
                      names_to = c("Variable",".metric"),
                      names_pattern = "(.+)_(n|shapiro_p)") |>
  tidyr::pivot_wider(names_from = .metric, values_from = value) |>
  dplyr::mutate(
    `Shapiro p`   = fmt_p(shapiro_p),
    Distribution  = ifelse(is.na(shapiro_p), "Insufficient",
                           ifelse(shapiro_p > 0.05, "Approx. normal", "Skewed"))
  ) |>
  dplyr::select(Variable, n, `Shapiro p`, Distribution) |>
  flextable::flextable() |>
  flextable::set_caption("Table 1.4. Shapiro-Wilk normality test on attendance metrics") |>
  flextable::autofit()

obj1_outputs[["Table 1.4 Normality assessment"]] <- norm_tbl

# =============================================================================
# 1.2 BIVARIATE -- tests of association
# =============================================================================
biv_rows <- list()

# --- (a) Q19 most common type vs facility_category, fac_location, cadre
# NB: in this dataset Q19 has only one realised category (92/93 = "Coming late"),
# so chi-square is degenerate.  We trap this and report a note.
test_q19 <- function(grouping, name) {
  tab <- table(survey$q19_most_common, survey[[grouping]])
  if (any(dim(tab) < 2)) {
    return(tibble::tibble(
      Outcome     = "Q19 Most common absenteeism type",
      Predictor   = name,
      Test        = "Not run",
      Statistic   = NA_real_,
      p           = NA_real_,
      Note        = "Q19 effectively constant in this dataset"
    ))
  }
  res <- choose_chisq(survey$q19_most_common, survey[[grouping]])
  tibble::tibble(Outcome = "Q19 Most common absenteeism type",
                 Predictor = name, Test = res$method,
                 Statistic = res$statistic, p = res$p, Note = NA_character_)
}
biv_rows$q19_fac <- test_q19("facility_category", "Facility category")
biv_rows$q19_loc <- test_q19("fac_location",          "Location")
biv_rows$q19_cad <- test_q19("cadre",             "Cadre")

# --- (b) absent_rate vs facility category (extraction-tool, worker-month)
test_2grp <- function(y_var, g_var, dataset, name_y, name_g) {
  y <- dataset[[y_var]]; g <- dataset[[g_var]]
  res <- choose_2group_test(y, g)
  tibble::tibble(Outcome = name_y, Predictor = name_g,
                 Test = res$method, Statistic = res$statistic,
                 p = res$p, Note = NA_character_)
}
biv_rows$ar_fac <- test_2grp("absent_rate","facility_category", ext,
                             "Absenteeism rate", "Facility category")

# --- (c) absent_rate vs cadre (>2 groups)
ext_cadre <- ext |> dplyr::filter(!is.na(cadre), !is.na(facility_category))
res <- choose_kgroup_test(ext_cadre$absent_rate, ext_cadre$cadre)
biv_rows$ar_cad <- tibble::tibble(Outcome = "Absenteeism rate",
                                  Predictor = "Cadre",
                                  Test = res$method,
                                  Statistic = res$statistic,
                                  p = res$p,
                                  Note = NA_character_)

# --- (d) avg_min_late vs proximity (Q10) - merge survey-level Q10 onto worker-month?
# DAP item is at the survey level: Q10 is asked of each respondent, not each
# worker-month. We therefore test at the survey level using the *survey
# respondent's* perceived monthly tardiness reports. There's no per-worker
# tardiness in the survey, so the proper test of this hypothesis happens with
# extraction data aggregated to the facility, joined with the survey-derived
# proportion of workers with proximity = Yes. We report it both ways:
# (i) Worker-level survey: Q10 vs Q18a "coming late observed" (binary)
res_q10_18a <- choose_chisq(survey$q10_walk, survey$q18a_late)
biv_rows$q10_18a <- tibble::tibble(
  Outcome = "Q18a. Lateness observed (Yes/No)",
  Predictor = "Proximity to facility (Q10)",
  Test = res_q10_18a$method, Statistic = res_q10_18a$statistic,
  p = res_q10_18a$p,
  Note = "Note: Q18a is constant (=Yes) for all respondents -> uninformative")

# --- (e) absent_rate vs n_children_u12 and n_adult_dep -- correlation
# Since absenteeism comes from extraction, we aggregate to a respondent's
# facility-month average and join... but there's no per-worker key linking
# the survey respondent to specific extraction rows. We use the *worker
# self-report* in survey via Q8, Q9 against a respondent-level variable that
# does exist: their facility's monthly absenteeism rate (averaged). That's
# imperfect; we flag it.
fac_avg_ar <- ext |>
  dplyr::group_by(phc) |>
  dplyr::summarise(fac_absent_rate_mean = mean(absent_rate, na.rm = TRUE),
                   .groups = "drop")
sv_with_ar <- survey |>
  dplyr::left_join(fac_avg_ar, by = "phc")

res_ch <- choose_corr_test(sv_with_ar$n_children_u12, sv_with_ar$fac_absent_rate_mean)
biv_rows$dep_ch <- tibble::tibble(
  Outcome = "Facility absenteeism rate (mean)",
  Predictor = "Number of children <12 (Q8)",
  Test = res_ch$method, Statistic = res_ch$estimate, p = res_ch$p,
  Note = "Predictor at respondent level; outcome at facility level")
res_ad <- choose_corr_test(sv_with_ar$n_adult_dep, sv_with_ar$fac_absent_rate_mean)
biv_rows$dep_ad <- tibble::tibble(
  Outcome = "Facility absenteeism rate (mean)",
  Predictor = "Number of adult dependents (Q9)",
  Test = res_ad$method, Statistic = res_ad$estimate, p = res_ad$p,
  Note = "Predictor at respondent level; outcome at facility level")

biv_combined <- dplyr::bind_rows(biv_rows) |>
  dplyr::mutate(p = fmt_p(p),
                Statistic = ifelse(is.na(Statistic), NA,
                                   formatC(Statistic, digits = 3, format = "f")))

ft_biv <- flextable::flextable(biv_combined) |>
  flextable::set_caption("Table 1.5. Bivariate associations - patterns and prevalence of absenteeism") |>
  flextable::autofit()

obj1_outputs[["Table 1.5 Bivariate associations"]] <- ft_biv

# =============================================================================
# 1.3 MULTIVARIATE -- regression models
# =============================================================================

# --- 1.3.1 Multiple linear regression: monthly absenteeism rate ---
# DV: worker-month absent_rate
# IVs: facility category, cadre, gender, fac_location, proximity, deps, Q12 score,
#      governance index, sanction index, incentive index
#
# Sociodemographic (gender, cadre, deps) and Q10 are at survey-respondent
# level; we join on phc using each PHC's mean respondent profile via the
# sv_phc table built in 01_preprocessing.R.

ext_for_lm <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(absent_rate),
                !is.na(cadre)) |>
  dplyr::left_join(sv_phc, by = "phc")

mod1_lm <- lm(absent_rate ~ facility_category + cadre + fac_location +
                q12_score_phc + governance_idx_phc + sanction_idx_phc +
                incentive_idx_phc + pct_walk_phc + n_children_phc +
                n_adult_dep_phc,
              data = ext_for_lm)

# Multicollinearity check - facility_category may be collinear with the PHC-
# level indices since intervention sites systematically have higher governance
# etc. Report VIF so reviewer can see which terms inflate variance.
vif_vec <- tryCatch(car::vif(mod1_lm), error = function(e) NA)
vif_df <- if (is.matrix(vif_vec)) {
  tibble::tibble(Variable = rownames(vif_vec),
                 GVIF = round(vif_vec[,1], 2),
                 `Df` = vif_vec[,2],
                 `Adjusted GVIF` = round(vif_vec[,3], 2))
} else if (is.numeric(vif_vec)) {
  tibble::tibble(Variable = names(vif_vec), VIF = round(vif_vec, 2))
} else {
  tibble::tibble(Note = "VIF could not be computed.")
}
ft_vif <- flextable::flextable(vif_df) |>
  flextable::set_caption("Table 1.6a. Variance Inflation Factors - full multiple linear regression model. VIF > 5 (or adjusted GVIF > sqrt(5) ~ 2.24) suggests problematic multicollinearity.") |>
  flextable::autofit()

tbl_lm <- mod1_lm |>
  gtsummary::tbl_regression(intercept = TRUE,
                            label = list(
                              facility_category   ~ "Facility category",
                              cadre               ~ "Cadre",
                              fac_location        ~ "Location",
                              q12_score_phc       ~ "OIC check frequency (Q12)",
                              governance_idx_phc  ~ "Governance Strength Index",
                              sanction_idx_phc    ~ "Sanction Enforcement Index",
                              incentive_idx_phc   ~ "Incentive Availability Index",
                              pct_walk_phc        ~ "% workers within walking distance",
                              n_children_phc      ~ "Mean # children <12 (PHC)",
                              n_adult_dep_phc     ~ "Mean # adult dependents (PHC)"
                            )) |>
  gtsummary::add_glance_source_note(
    include = c(r.squared, adj.r.squared, p.value, nobs)) |>
  gtsummary::modify_caption("Table 1.6. Multiple linear regression - full model: predictors of monthly absenteeism rate (worker-month)")

obj1_outputs[["Table 1.6 Linear regression on absenteeism (full)"]] <- tbl_lm
obj1_outputs[["Table 1.6a VIF (full model)"]] <- ft_vif

# Parsimonious model - drops the indices (which collinear with facility category)
# to give a cleaner read of the *residual* effect of cadre/fac_location/proximity
# beyond the intervention.
mod1_lm_pars <- lm(absent_rate ~ facility_category + cadre + fac_location +
                     q12_score_phc + pct_walk_phc + n_children_phc + n_adult_dep_phc,
                   data = ext_for_lm)
tbl_lm_pars <- mod1_lm_pars |>
  gtsummary::tbl_regression(intercept = TRUE) |>
  gtsummary::add_glance_source_note(
    include = c(r.squared, adj.r.squared, p.value, nobs)) |>
  gtsummary::modify_caption("Table 1.6b. Multiple linear regression - parsimonious model (composite indices removed to avoid collinearity with facility category)")
obj1_outputs[["Table 1.6b Linear regression (parsimonious)"]] <- tbl_lm_pars

# --- 1.3.2 Multinomial logistic regression: Q19 most common absenteeism type ---
# Q19 is essentially constant (92/93 in 'Coming late') in this dataset,
# making multinomial logit infeasible. We therefore substitute three
# separate binary logistic regressions for Q18a-c -- documenting the
# adjustment.
q19_levels <- length(unique(na.omit(survey$q19_most_common)))
if (q19_levels < 2) {
  note_q19 <- tibble::tibble(
    Note = paste0("Q19 has only ", q19_levels,
                  " realised category in this dataset; multinomial",
                  " logistic regression cannot be fitted. As a substitute,",
                  " three binary logistic regressions on Q18a-c are reported below.")
  ) |>
    flextable::flextable() |>
    flextable::set_caption("Table 1.7a. Note on Q19 multinomial regression") |>
    flextable::autofit()
  obj1_outputs[["Table 1.7a Note on Q19"]] <- note_q19
  
  # Build common predictor frame (only when Q23a/Q26 binary - convert to factors)
  pmax_safe <- function(...) {
    m <- cbind(...)
    out <- suppressWarnings(apply(m, 1, max, na.rm = TRUE))
    out[!is.finite(out)] <- NA
    as.integer(out)
  }
  sv_for_logit <- survey |>
    dplyr::mutate(
      q23_any = pmax_safe(q23a_sanc_late, q23b_sanc_leave, q23c_sanc_fullday),
      q26_any = pmax_safe(q26_inc_money, q26_inc_food, q26_inc_promo, q26_inc_recog),
      q23_any = factor(q23_any, levels = c(0,1), labels = c("No","Yes")),
      q26_any = factor(q26_any, levels = c(0,1), labels = c("No","Yes")),
      q10_walk = factor(q10_walk, levels = c("No","Yes"))
    )
  
  fit_q18 <- function(outcome_var, label) {
    sv_for_logit$.y <- factor(sv_for_logit[[outcome_var]],
                              levels = c(0,1), labels = c("No","Yes"))
    n_pos <- sum(sv_for_logit$.y == "Yes", na.rm = TRUE)
    n_neg <- sum(sv_for_logit$.y == "No",  na.rm = TRUE)
    if (n_pos < 2 || n_neg < 2) {
      msg <- sprintf("Outcome too rare for logistic regression (Yes=%d, No=%d).",
                     n_pos, n_neg)
      return(flextable::flextable(tibble::tibble(Note = msg)) |>
               flextable::set_caption(sprintf("Table 1.7. %s - not modelled", label)) |>
               flextable::autofit())
    }
    # Reduced predictor set to keep model identifiable with n=93
    fit <- tryCatch(
      glm(.y ~ facility_category + gender + fac_location + q10_walk + q23_any,
          data = sv_for_logit, family = binomial()),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (is.null(fit)) {
      return(flextable::flextable(tibble::tibble(
        Note = sprintf("Model did not converge for %s.", label))) |>
          flextable::set_caption(sprintf("Table 1.7. %s - model failure", label)) |>
          flextable::autofit())
    }
    fit |>
      gtsummary::tbl_regression(exponentiate = TRUE,
                                label = list(
                                  facility_category ~ "Facility category",
                                  gender ~ "Gender",
                                  fac_location ~ "Location",
                                  q10_walk ~ "Walking distance (Q10)",
                                  q23_any  ~ "Any sanction exists (Q23)"
                                )) |>
      gtsummary::modify_caption(sprintf("Table 1.7. Binary logistic regression - %s", label))
  }
  
  obj1_outputs[["Table 1.7b Logit Q18a (lateness)"]]    <- fit_q18("q18a_late",        "Q18a. Coming late observed")
  obj1_outputs[["Table 1.7c Logit Q18b (leave early)"]] <- fit_q18("q18b_leave_early", "Q18b. Leaving early observed")
  obj1_outputs[["Table 1.7d Logit Q18c (full-day)"]]    <- fit_q18("q18c_full_day",    "Q18c. Full-day absence observed")
} else {
  # Q19 has ≥2 levels -> proper multinomial
  sv_mn <- survey |>
    dplyr::filter(!is.na(q19_most_common))
  sv_mn$q19_most_common <- factor(sv_mn$q19_most_common)
  mod_mn <- nnet::multinom(q19_most_common ~ facility_category + cadre +
                             gender + fac_location + q10_walk +
                             q23a_sanc_late + q26_inc_money,
                           data = sv_mn, trace = FALSE)
  tbl_mn <- mod_mn |>
    gtsummary::tbl_regression(exponentiate = TRUE) |>
    gtsummary::modify_caption("Table 1.7. Multinomial logistic regression - most common absenteeism type (Q19)")
  obj1_outputs[["Table 1.7 Multinomial logit Q19"]] <- tbl_mn
}

# =============================================================================
# 1.4  Write everything to a single .docx
# =============================================================================
out_path <- file.path(OUTPUT_DIR, "Objective1_Patterns_Prevalence.docx")
save_to_docx(obj1_outputs, out_path,
             title = "Objective 1 - Patterns and prevalence of health worker absenteeism")