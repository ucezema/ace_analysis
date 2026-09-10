# =============================================================================
# ACE-II Evaluation
# Script 04: OBJECTIVE 3 - Predictors of health worker absenteeism
#
# Run AFTER 00_setup.R and 01_preprocessing.R.
# Output: outputs/Objective3_Predictors.docx
# =============================================================================

stopifnot(exists("survey"), exists("ext"), exists("sv_phc"))

obj3_outputs <- list()

# =============================================================================
# 3.1 UNIVARIATE -- predictors and composite indices
# =============================================================================

# --- 3.1.1 Sociodemographic + institutional predictors ----------------------
tbl_pred <- survey |>
  dplyr::select(facility_category, gender, cadre, fac_location, q10_walk,
                n_children_u12, n_adult_dep,
                q11_register, q12_oic_check, q29_wdc_monitor, q31_comm_support) |>
  dplyr::mutate(
    q31_simple = dplyr::case_when(
      is.na(q31_comm_support) ~ NA_character_,
      tolower(q31_comm_support) == "no" ~ "No",
      TRUE ~ "Yes"),
    q31_simple = factor(q31_simple, levels = c("No","Yes"))
  ) |>
  dplyr::select(-q31_comm_support) |>
  gtsummary::tbl_summary(
    by = facility_category,
    missing = "ifany",
    label = list(
      gender         ~ "Gender",
      cadre          ~ "Cadre",
      fac_location       ~ "Location",
      q10_walk       ~ "Lives within walking distance (Q10)",
      n_children_u12 ~ "Children <12 years (n)",
      n_adult_dep    ~ "Adult dependents (n)",
      q11_register   ~ "Attendance register kept (Q11)",
      q12_oic_check  ~ "OIC check frequency (Q12)",
      q29_wdc_monitor~ "WDC/HFC monitors absenteeism (Q29)",
      q31_simple     ~ "Community provides support (Q31)"
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
  gtsummary::modify_header(label = "**Variable**") |>
  gtsummary::modify_caption("Table 3.1. Distribution of candidate predictors of absenteeism, by facility category")

obj3_outputs[["Table 3.1 Predictor distribution"]] <- tbl_pred

# --- 3.1.2 Composite indices summary ---------------------------------------
tbl_idx <- survey |>
  dplyr::select(facility_category, governance_idx, sanction_idx, incentive_idx) |>
  gtsummary::tbl_summary(
    by = facility_category,
    type = list(gtsummary::all_continuous() ~ "continuous2"),
    statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                     "{median} [{p25}, {p75}]")),
    digits = list(gtsummary::all_continuous() ~ 2),
    label = list(
      governance_idx ~ "Governance Strength Index (0-9)",
      sanction_idx   ~ "Sanction Enforcement Index (0-6)",
      incentive_idx  ~ "Incentive Availability Index (0-6)"
    )
  ) |>
  gtsummary::add_overall() |>
  gtsummary::modify_caption("Table 3.2. Composite indices distributions, by facility category")

obj3_outputs[["Table 3.2 Composite indices"]] <- tbl_idx

# Outlier identification on absenteeism rate (per worker-month)
ar <- ext$absent_rate
ar <- ar[is.finite(ar)]
q1 <- quantile(ar, 0.25); q3 <- quantile(ar, 0.75); iqr <- q3 - q1
lo <- q1 - 1.5*iqr; hi <- q3 + 1.5*iqr
outlier_df <- tibble::tibble(
  Statistic = c("N", "Mean (SD)", "Median [IQR]",
                "Tukey lower fence", "Tukey upper fence",
                "Outliers below lower (n)", "Outliers above upper (n)"),
  Value = c(
    formatC(length(ar), format = "d"),
    sprintf("%.2f (%.2f)", mean(ar), sd(ar)),
    sprintf("%.2f [%.2f, %.2f]", median(ar), q1, q3),
    sprintf("%.2f", lo),
    sprintf("%.2f", hi),
    formatC(sum(ar < lo), format = "d"),
    formatC(sum(ar > hi), format = "d")
  )
)
ft_outliers <- flextable::flextable(outlier_df) |>
  flextable::set_caption("Table 3.3. Distribution and outlier detection - worker-month absenteeism rate (Tukey 1.5xIQR fences)") |>
  flextable::autofit()
obj3_outputs[["Table 3.3 Distribution & outliers"]] <- ft_outliers

# =============================================================================
# 3.2 BIVARIATE -- tests of association
# =============================================================================
biv3 <- list()

# (a) absent_rate vs gender (binary, t-test)
res <- choose_2group_test(ext$absent_rate, ext$facility_category)  # placeholder
# Gender is at survey level; merge gender via PHC majority/proportion - or use
# worker-level proxy: in extraction, no gender is recorded, so we use facility-
# level % female from sv_phc (continuous predictor) instead.
ext_g <- ext |> dplyr::left_join(sv_phc, by = "phc")
res_gender <- choose_corr_test(ext_g$pct_female_phc, ext_g$absent_rate)
biv3$gender <- tibble::tibble(
  Outcome = "Absenteeism rate (worker-month)",
  Predictor = "Facility % female (PHC-level)",
  Test = res_gender$method,
  Statistic = res_gender$estimate, p = res_gender$p,
  Note = "Gender not recorded per worker-month; using PHC-level % female as proxy"
)

# (b) absent_rate vs cadre (>2 groups, ANOVA/Kruskal)
ext_c <- ext |> dplyr::filter(!is.na(cadre))
res_c <- choose_kgroup_test(ext_c$absent_rate, ext_c$cadre)
biv3$cadre <- tibble::tibble(
  Outcome = "Absenteeism rate", Predictor = "Cadre",
  Test = res_c$method, Statistic = res_c$statistic, p = res_c$p,
  Note = NA_character_
)

# (c) absent_rate vs governance index (continuous, correlation)
res_g <- choose_corr_test(ext_g$governance_idx_phc, ext_g$absent_rate)
biv3$gov <- tibble::tibble(
  Outcome = "Absenteeism rate",
  Predictor = "Governance Strength Index (PHC-level)",
  Test = res_g$method, Statistic = res_g$estimate, p = res_g$p,
  Note = NA_character_
)

# (d) absent_rate vs sanction index (Spearman per DAP)
ok <- complete.cases(ext_g$sanction_idx_phc, ext_g$absent_rate)
ct_s <- suppressWarnings(cor.test(ext_g$sanction_idx_phc[ok],
                                  ext_g$absent_rate[ok], method = "spearman"))
biv3$sanc <- tibble::tibble(
  Outcome = "Absenteeism rate",
  Predictor = "Sanction Enforcement Index (PHC-level)",
  Test = "Spearman rank correlation",
  Statistic = unname(ct_s$estimate), p = ct_s$p.value,
  Note = NA_character_
)

# (e) absent_rate vs incentive index (Spearman, ordinal-like)
ok <- complete.cases(ext_g$incentive_idx_phc, ext_g$absent_rate)
ct_i <- suppressWarnings(cor.test(ext_g$incentive_idx_phc[ok],
                                  ext_g$absent_rate[ok], method = "spearman"))
biv3$inc <- tibble::tibble(
  Outcome = "Absenteeism rate",
  Predictor = "Incentive Availability Index (PHC-level)",
  Test = "Spearman rank correlation",
  Statistic = unname(ct_i$estimate), p = ct_i$p.value,
  Note = NA_character_
)

# (f) High absenteeism (binary, >= median) vs categorical predictors
ext_g <- ext_g |>
  dplyr::mutate(high_abs = factor(dplyr::if_else(absent_rate >= median(absent_rate, na.rm = TRUE),
                                                 "High", "Low"),
                                  levels = c("Low","High")))

cat_preds <- c("fac_location","cadre")
for (p in cat_preds) {
  res <- choose_chisq(ext_g$high_abs, ext_g[[p]])
  biv3[[paste0("hi_",p)]] <- tibble::tibble(
    Outcome = "High absenteeism (>= median)",
    Predictor = stringr::str_to_title(p),
    Test = res$method, Statistic = res$statistic, p = res$p,
    Note = NA_character_
  )
}

# Q10 (proximity) and q31 (community support) at survey level joined to extraction
# Most PHCs have multiple respondents -> Q10/Q31 will not be a single value
# per PHC; use the majority-vote value within each PHC.
mode_str <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}
phc_q10 <- survey |>
  dplyr::mutate(
    q31_yn = dplyr::case_when(
      is.na(q31_comm_support) ~ NA_character_,
      tolower(q31_comm_support) == "no" ~ "No",
      TRUE ~ "Yes")
  ) |>
  dplyr::group_by(phc) |>
  dplyr::summarise(
    q10_majority = mode_str(q10_walk),
    q31_majority = mode_str(q31_yn),
    .groups = "drop"
  )
ext_q <- ext_g |> dplyr::left_join(phc_q10, by = "phc")

res_q10 <- choose_chisq(ext_q$high_abs, ext_q$q10_majority)
biv3$hi_q10 <- tibble::tibble(
  Outcome = "High absenteeism (>= median)",
  Predictor = "Q10 walking distance (PHC-majority)",
  Test = res_q10$method, Statistic = res_q10$statistic, p = res_q10$p,
  Note = NA_character_
)
res_q31 <- choose_chisq(ext_q$high_abs, ext_q$q31_majority)
biv3$hi_q31 <- tibble::tibble(
  Outcome = "High absenteeism (>= median)",
  Predictor = "Q31 community support (PHC-majority)",
  Test = res_q31$method, Statistic = res_q31$statistic, p = res_q31$p,
  Note = NA_character_
)

biv3_combined <- dplyr::bind_rows(biv3) |>
  dplyr::mutate(p = fmt_p(p),
                Statistic = ifelse(is.na(Statistic), NA,
                                   formatC(Statistic, digits = 3, format = "f")))

ft_biv3 <- flextable::flextable(biv3_combined) |>
  flextable::set_caption("Table 3.4. Bivariate associations - candidate predictors of absenteeism") |>
  flextable::autofit()
obj3_outputs[["Table 3.4 Bivariate associations"]] <- ft_biv3

# Post-hoc tests for cadre if ANOVA significant
if (!is.na(biv3$cadre$p) && biv3$cadre$p < 0.05) {
  if (biv3$cadre$Test == "ANOVA") {
    th <- TukeyHSD(aov(absent_rate ~ factor(cadre), data = ext_c))
    th_df <- as.data.frame(th[[1]]) |>
      tibble::rownames_to_column("Comparison") |>
      dplyr::mutate(`p adj` = fmt_p(`p adj`),
                    dplyr::across(c(diff, lwr, upr),
                                  ~ formatC(.x, digits = 3, format = "f")))
    ft_th <- flextable::flextable(th_df) |>
      flextable::set_caption("Table 3.4a. Post-hoc Tukey HSD for absenteeism rate by cadre") |>
      flextable::autofit()
    obj3_outputs[["Table 3.4a Post-hoc Tukey HSD"]] <- ft_th
  } else {
    # Kruskal -> Dunn's test
    dn <- tryCatch(
      rstatix::dunn_test(ext_c, absent_rate ~ cadre, p.adjust.method = "bonferroni"),
      error = function(e) NULL
    )
    if (!is.null(dn)) {
      ft_dn <- flextable::flextable(as.data.frame(dn)) |>
        flextable::set_caption("Table 3.4a. Post-hoc Dunn's test (Bonferroni-adjusted) for absenteeism rate by cadre") |>
        flextable::autofit()
      obj3_outputs[["Table 3.4a Post-hoc Dunn's test"]] <- ft_dn
    }
  }
}

# =============================================================================
# 3.3 MULTIVARIATE
# =============================================================================

# --- 3.3.1 Multiple linear regression: absenteeism rate ---------------------
ext_for_lm3 <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(cadre), !is.na(absent_rate)) |>
  dplyr::left_join(sv_phc, by = "phc") |>
  dplyr::left_join(phc_q10, by = "phc")

# Convert q10_majority to factor
ext_for_lm3 <- ext_for_lm3 |>
  dplyr::mutate(q10_majority = factor(q10_majority, levels = c("No","Yes")))

# Diagnostic + defensive predictor selection.
# lm() errors with "contrasts can be applied only to factors with 2 or more
# levels" when any factor predictor in the model frame has fewer than 2
# realised levels (after listwise NA deletion) OR when any numeric predictor
# has zero variance. We screen each candidate predictor and drop any that
# can't contribute, then refit. The diagnostic table records what happened.
candidate_preds <- c("cadre","fac_location","q10_majority",
                     "q12_score_phc","governance_idx_phc","sanction_idx_phc",
                     "incentive_idx_phc","n_children_phc","n_adult_dep_phc")

diag_preds <- purrr::map_dfr(candidate_preds, function(p) {
  x <- ext_for_lm3[[p]]
  if (is.null(x)) {
    return(tibble::tibble(Predictor = p, Class = "MISSING", n_unique = NA_integer_,
                          Decision = "DROP - column not present"))
  }
  x_complete <- x[!is.na(x)]
  n_unique <- length(unique(x_complete))
  if (is.factor(x) || is.character(x)) {
    keep <- n_unique >= 2
  } else if (is.numeric(x)) {
    keep <- (n_unique >= 2) && (var(x_complete, na.rm = TRUE) > 0)
  } else {
    keep <- FALSE
  }
  tibble::tibble(Predictor = p,
                 Class = paste(class(x), collapse = "/"),
                 n_unique = n_unique,
                 Decision = ifelse(keep, "KEEP", "DROP - zero variance / single level"))
})
ft_pred_diag <- flextable::flextable(diag_preds) |>
  flextable::set_caption("Table 3.4b. Predictor screening for multiple regression. Predictors marked DROP were excluded automatically because they had zero variance or only one realised level after listwise NA deletion.") |>
  flextable::autofit()
obj3_outputs[["Table 3.4b Predictor screening"]] <- ft_pred_diag

kept_preds <- diag_preds$Predictor[diag_preds$Decision == "KEEP"]
mod3_form <- as.formula(paste("absent_rate ~", paste(kept_preds, collapse = " + ")))

mod3_lm <- lm(mod3_form, data = ext_for_lm3)

cat("\n=== Predictor screening result ===\n")
print(diag_preds)
cat("\nFitted formula: ", deparse(mod3_form), "\n")

# Assumption diagnostics ------------------------------------------------------
# Each tryCatch returns a list (the htest object) or NULL. The extractor
# functions below ALWAYS return a length-1 character so we can't accidentally
# build a length-2 column.
shap_test <- tryCatch(shapiro.test(rstandard(mod3_lm)),
                      error = function(e) NULL, warning = function(w) NULL)
bp_test   <- tryCatch(lmtest::bptest(mod3_lm),
                      error = function(e) NULL, warning = function(w) NULL)
dw_test   <- tryCatch(car::durbinWatsonTest(mod3_lm),
                      error = function(e) NULL, warning = function(w) NULL)

stat_str <- function(x) {
  if (is.null(x)) return("n/a")
  s <- tryCatch(unname(x$statistic), error = function(e) NA_real_)
  if (length(s) == 0 || !is.finite(s[1])) return("n/a")
  formatC(s[1], digits = 3, format = "f")
}
p_str <- function(x, p_field = "p.value") {
  if (is.null(x)) return("n/a")
  pv <- tryCatch(x[[p_field]], error = function(e) NA_real_)
  if (length(pv) == 0 || !is.finite(pv[1])) return("n/a")
  fmt_p(pv[1])
}

diag_df <- tibble::tibble(
  Assumption = c("Residual normality (Shapiro-Wilk)",
                 "Homoscedasticity (Breusch-Pagan)",
                 "Independence (Durbin-Watson)"),
  Statistic = c(stat_str(shap_test), stat_str(bp_test), stat_str(dw_test)),
  p         = c(p_str(shap_test, "p.value"),
                p_str(bp_test,   "p.value"),
                # car::durbinWatsonTest stores p in $p (no dot)
                p_str(dw_test,   "p"))
)
ft_diag <- flextable::flextable(diag_df) |>
  flextable::set_caption("Table 3.5a. Diagnostics on multiple linear regression assumptions") |>
  flextable::autofit()

# Build the label list dynamically so we never name a predictor that was
# dropped by the screening step above. gtsummary treats labels for absent
# variables as an error (in 2.x it does, anyway), so we filter to kept_preds.
all_labels <- list(
  cadre               = "Cadre",
  fac_location        = "Location",
  q10_majority        = "Walking distance (PHC majority)",
  q12_score_phc       = "OIC check frequency (Q12)",
  governance_idx_phc  = "Governance Strength Index",
  sanction_idx_phc    = "Sanction Enforcement Index",
  incentive_idx_phc   = "Incentive Availability Index",
  n_children_phc      = "Mean # children <12 (PHC)",
  n_adult_dep_phc     = "Mean # adult dependents (PHC)"
)
build_labels <- function(model_vars) {
  # gtsummary expects a list of `var ~ "label"` formulas. Convert from named
  # character vector, keeping only variables that appear in the model.
  keep <- intersect(names(all_labels), model_vars)
  setNames(lapply(keep, function(v) {
    stats::as.formula(paste0(v, " ~ '", all_labels[[v]], "'"))
  }), keep) |> unname()
}

tbl_lm3 <- mod3_lm |>
  gtsummary::tbl_regression(intercept = TRUE,
                            label = build_labels(kept_preds)) |>
  gtsummary::add_glance_source_note(include = c(r.squared, adj.r.squared,
                                                nobs, p.value)) |>
  gtsummary::modify_caption("Table 3.5. Multiple linear regression - independent predictors of monthly absenteeism rate")

obj3_outputs[["Table 3.5 Linear regression on absenteeism"]] <- tbl_lm3
obj3_outputs[["Table 3.5a Diagnostics"]] <- ft_diag

# VIF
vif_v <- tryCatch(car::vif(mod3_lm), error = function(e) NA)
if (is.matrix(vif_v)) {
  vif_df <- tibble::tibble(Variable = rownames(vif_v),
                           GVIF = round(vif_v[,1], 2),
                           Df   = vif_v[,2],
                           `Adjusted GVIF` = round(vif_v[,3], 2))
} else if (is.numeric(vif_v)) {
  vif_df <- tibble::tibble(Variable = names(vif_v), VIF = round(vif_v, 2))
} else {
  vif_df <- tibble::tibble(Note = "VIF could not be computed")
}
ft_vif3 <- flextable::flextable(vif_df) |>
  flextable::set_caption("Table 3.5b. Variance Inflation Factors") |>
  flextable::autofit()
obj3_outputs[["Table 3.5b VIF"]] <- ft_vif3

# --- 3.3.2 Binary logistic regression: high absenteeism --------------------
ext_for_lr3 <- ext_for_lm3 |>
  dplyr::mutate(high_abs = factor(dplyr::if_else(absent_rate >= median(absent_rate, na.rm = TRUE),
                                                 "High", "Low"),
                                  levels = c("Low","High")))

# Reuse the kept_preds from the linear model screening above so the two
# models use a consistent predictor set.
mod3_logit_form <- as.formula(paste("high_abs ~", paste(kept_preds, collapse = " + ")))
mod3_logit <- glm(mod3_logit_form, data = ext_for_lr3, family = binomial())

tbl_logit3 <- mod3_logit |>
  gtsummary::tbl_regression(exponentiate = TRUE,
                            label = build_labels(kept_preds)) |>
  gtsummary::add_glance_source_note(include = c(AIC, BIC, nobs, null.deviance,
                                                deviance)) |>
  gtsummary::modify_caption("Table 3.6. Binary logistic regression - odds of high absenteeism (>= median rate)")

obj3_outputs[["Table 3.6 Logistic regression on high absenteeism"]] <- tbl_logit3

# Hosmer-Lemeshow goodness-of-fit
hl <- tryCatch({
  pred_p <- predict(mod3_logit, type = "response")
  obs    <- as.integer(ext_for_lr3$high_abs == "High")
  ResourceSelection::hoslem.test(obs, pred_p, g = 10)
}, error = function(e) NULL)
if (!is.null(hl)) {
  hl_df <- tibble::tibble(
    Statistic = c("Hosmer-Lemeshow chi-square", "df", "p-value"),
    Value = c(formatC(hl$statistic, digits = 3, format = "f"),
              formatC(hl$parameter, digits = 0, format = "d"),
              fmt_p(hl$p.value))
  )
  ft_hl <- flextable::flextable(hl_df) |>
    flextable::set_caption("Table 3.6a. Hosmer-Lemeshow goodness-of-fit test (p > 0.05 supports model fit)") |>
    flextable::autofit()
  obj3_outputs[["Table 3.6a Hosmer-Lemeshow"]] <- ft_hl
} else {
  obj3_outputs[["Table 3.6a Hosmer-Lemeshow"]] <-
    flextable::flextable(tibble::tibble(
      Note = "Hosmer-Lemeshow test not run (install ResourceSelection package)."
    )) |> flextable::autofit()
}

# Stepwise (backward) selection by AIC
mod3_stepped <- tryCatch(
  MASS::stepAIC(mod3_logit, direction = "backward", trace = FALSE),
  error = function(e) NULL
)
if (!is.null(mod3_stepped)) {
  tbl_step <- mod3_stepped |>
    gtsummary::tbl_regression(exponentiate = TRUE) |>
    gtsummary::add_glance_source_note(include = c(AIC, BIC, nobs)) |>
    gtsummary::modify_caption("Table 3.6b. Backward-stepwise (AIC-minimising) logistic regression")
  obj3_outputs[["Table 3.6b Stepwise logit"]] <- tbl_step
}

# =============================================================================
# 3.4 Write to docx
# =============================================================================
out_path <- file.path(OUTPUT_DIR, "Objective3_Predictors.docx")
save_to_docx(obj3_outputs, out_path,
             title = "Objective 3 - Predictors of health worker absenteeism")