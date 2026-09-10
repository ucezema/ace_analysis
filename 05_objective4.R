# =============================================================================
# ACE-II Evaluation
# Script 05: OBJECTIVE 4 - Relative contribution of intervention components
#
# Run AFTER 00_setup.R and 01_preprocessing.R.
# Output: outputs/Objective4_Components.docx
#
# CRITICAL CAVEAT - PLEASE READ:
# In this dataset, every intervention-site respondent answered "Yes" to all
# component questions (Q34, Q40, Q43, Q44, Q46, Q47, Q49). Control-site
# respondents were skipped on these items by ODK skip-logic.
# Consequence: Q34, Q40 and Q46 are PERFECTLY COLLINEAR with each other and
# with facility_category. We cannot statistically separate the contributions
# of transport allowance vs. peer support vs. CMSC from this data set alone.
# The DAP describes a design that requires component variation within the
# intervention arm (e.g. dose, frequency, missed payments). That variation
# must come from the FACILITY-LEVEL ROLLOUT RECORDS, not the worker survey.
# This script reports what the survey allows: descriptive uptake of each
# component, ordinal/frequency variation (Q35, Q41, Q48, Q53), and a
# multivariate model using the Intervention Exposure Score (which IS
# constant=7 within intervention sites, but is treated honestly).
# =============================================================================

stopifnot(exists("survey"), exists("ext"), exists("sv_phc"))

obj4_outputs <- list()

# Helper: majority value (mode) for a character vector
mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Subset to intervention sites for component-level analyses
sv_intv <- survey |> dplyr::filter(facility_category == "Intervention")

# Caveat note in the docx
caveat_df <- tibble::tibble(
  Note = c(paste("All intervention-site respondents (n =", nrow(sv_intv),
                 ") answered 'Yes' to Q34 (transport allowance),",
                 "Q40 (peer support), and Q46 (CMSC). Component uptake is",
                 "therefore 100% within the intervention arm in this dataset."),
           paste("Q34/Q40/Q46 are perfectly collinear with each other and with",
                 "facility_category, so a multiple regression entering the three",
                 "components simultaneously is not identifiable from the worker",
                 "survey alone. We report uptake (Univariate), comparisons of",
                 "intervention vs control on the change in absenteeism (Bivariate),",
                 "and use COMPONENT FREQUENCY/DOSE variables (Q35, Q41, Q53)",
                 "for component-specific tests within the intervention arm."))
)
obj4_outputs[["Note 4.0 Important caveat"]] <-
  flextable::flextable(caveat_df) |> flextable::autofit()

# =============================================================================
# 4.1 UNIVARIATE
# =============================================================================

# Component uptake within intervention sites
tbl_uptake <- sv_intv |>
  dplyr::select(q34_transport_disb, q35_transport_freq,
                q40_peer_estab, q41_peer_freq,
                q43_peer_teamwork, q44_peer_redabs,
                q46_cmsc_estab, q47_cmsc_monitor, q49_cmsc_redabs,
                exposure_idx, exposure_cat) |>
  gtsummary::tbl_summary(
    missing = "ifany",
    label = list(
      q34_transport_disb ~ "Q34. Transport allowance disbursed",
      q35_transport_freq ~ "Q35. Frequency of disbursement",
      q40_peer_estab     ~ "Q40. Peer support network established",
      q41_peer_freq      ~ "Q41. Frequency of peer support meetings",
      q43_peer_teamwork  ~ "Q43. Peer network improved teamwork",
      q44_peer_redabs    ~ "Q44. Peer network reduced absenteeism",
      q46_cmsc_estab     ~ "Q46. CMSC established",
      q47_cmsc_monitor   ~ "Q47. CMSC actively monitors",
      q49_cmsc_redabs    ~ "Q49. CMSC reduced absenteeism",
      exposure_idx       ~ "Intervention Exposure Score (0-7)",
      exposure_cat       ~ "Exposure tier (Low/Mod/High)"
    ),
    type = list(exposure_idx ~ "continuous2"),
    statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                     "{median} [{p25}, {p75}]"))
  ) |>
  gtsummary::modify_caption("Table 4.1. Uptake of intervention components, intervention sites only (n approx 39)")
obj4_outputs[["Table 4.1 Component uptake"]] <- tbl_uptake

# Perceived impact + challenges (challenges are open text - we just count
# non-empty)
raw_challenges <- list(
  transport = "39. Challenges experienced with transport allowance",
  peer      = "45. Challenges with peer support network",
  cmsc      = "50. Challenges experienced with CMSC monitoring"
)
chal_df <- raw_survey |>
  dplyr::filter(`Is this facility an intervention or control site?` %in%
                  c("Intervention site","Intervention","intervention site")) |>
  dplyr::transmute(
    transport_challenge = !is.na(.data[[raw_challenges$transport]]) &
      stringr::str_length(.data[[raw_challenges$transport]]) > 0,
    peer_challenge      = !is.na(.data[[raw_challenges$peer]]) &
      stringr::str_length(.data[[raw_challenges$peer]]) > 0,
    cmsc_challenge      = !is.na(.data[[raw_challenges$cmsc]]) &
      stringr::str_length(.data[[raw_challenges$cmsc]]) > 0
  )
chal_summary <- tibble::tibble(
  Component = c("Transport allowance (Q39)", "Peer support (Q45)", "CMSC (Q50)"),
  `Reported challenges, n (%)` = c(
    sprintf("%d (%.1f%%)", sum(chal_df$transport_challenge),
            100*mean(chal_df$transport_challenge)),
    sprintf("%d (%.1f%%)", sum(chal_df$peer_challenge),
            100*mean(chal_df$peer_challenge)),
    sprintf("%d (%.1f%%)", sum(chal_df$cmsc_challenge),
            100*mean(chal_df$cmsc_challenge))
  )
)
ft_chal <- flextable::flextable(chal_summary) |>
  flextable::set_caption("Table 4.2. Frequency of reported challenges per component (free-text fields, intervention sites only)") |>
  flextable::autofit()
obj4_outputs[["Table 4.2 Component challenges"]] <- ft_chal

# =============================================================================
# 4.2 BIVARIATE
# =============================================================================
# DAP wants: "change in absenteeism rate vs (component) at worker level".
# Worker-level change requires pre-vs-post per worker, which the data doesn't
# support (no stable worker_id across months). We instead compute change at
# the facility level and test against component variation.
#
# Within-intervention component variation: we use Q53 (sanction consistency
# 5-pt ordinal), Q41 (peer meeting frequency: weekly vs monthly), Q35
# (transport disbursement frequency).

# Facility-level change in absenteeism
fac_change <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(period)) |>
  dplyr::group_by(phc, facility_category, period) |>
  dplyr::summarise(absent_rate = mean(absent_rate, na.rm = TRUE),
                   .groups = "drop") |>
  tidyr::pivot_wider(names_from = period, values_from = absent_rate) |>
  dplyr::mutate(delta = Intervention - `Pre-intervention`) |>
  tidyr::drop_na(delta)

biv4 <- list()

# (a) Change vs facility category (already in Obj 2, but reproduce here)
res <- choose_2group_test(fac_change$delta, fac_change$facility_category)
biv4$delta_fc <- tibble::tibble(
  Outcome = "Change in absenteeism rate (post - pre)",
  Predictor = "Facility category",
  Test = res$method, Statistic = res$statistic, p = res$p,
  Note = "Reproduced from Objective 2"
)

# Build PHC-level summary of within-intervention component frequency
# variation, then merge with fac_change.
phc_components <- survey |>
  dplyr::filter(facility_category == "Intervention") |>
  dplyr::group_by(phc) |>
  dplyr::summarise(
    transport_freq_majority = mode_chr(q35_transport_freq),
    peer_freq_majority      = mode_chr(q41_peer_freq),
    sanc_consist_majority   = mode_chr(q53_sanc_consist),
    .groups = "drop"
  )

fac_change_intv <- fac_change |>
  dplyr::filter(facility_category == "Intervention") |>
  dplyr::left_join(phc_components, by = "phc")

# (b) Change vs peer meeting frequency
if (length(unique(na.omit(fac_change_intv$peer_freq_majority))) >= 2 &&
    nrow(fac_change_intv) >= 4) {
  res <- choose_2group_test(fac_change_intv$delta,
                            factor(fac_change_intv$peer_freq_majority))
  biv4$delta_peer <- tibble::tibble(
    Outcome = "Change in absenteeism rate",
    Predictor = "Peer meeting frequency (Q41)",
    Test = res$method, Statistic = res$statistic, p = res$p,
    Note = sprintf("n facilities = %d", nrow(fac_change_intv))
  )
} else {
  biv4$delta_peer <- tibble::tibble(
    Outcome = "Change in absenteeism rate",
    Predictor = "Peer meeting frequency (Q41)",
    Test = "Not run", Statistic = NA_real_, p = NA_real_,
    Note = "Insufficient variation across intervention facilities"
  )
}

# (c) Pearson correlation: Intervention Exposure Score vs absenteeism rate
ext_with_exp <- ext |>
  dplyr::left_join(survey |>
                     dplyr::group_by(phc) |>
                     dplyr::summarise(exposure_phc = mean(exposure_idx, na.rm = TRUE),
                                      .groups = "drop"),
                   by = "phc")

res_exp <- choose_corr_test(ext_with_exp$exposure_phc, ext_with_exp$absent_rate)
biv4$exposure <- tibble::tibble(
  Outcome = "Absenteeism rate (worker-month)",
  Predictor = "Intervention Exposure Score (PHC-level)",
  Test = res_exp$method, Statistic = res_exp$estimate, p = res_exp$p,
  Note = "NB: exposure is exactly 0 (control) or 7 (intervention); this test is mathematically equivalent to a 2-group comparison."
)

# (d) Q37 (perceived absenteeism reduction by transport, binary) vs Q35 (frequency)
sv_intv$q37_yn <- ifelse(sv_intv$q37_transport_redabs == "Yes", "Yes", "No")
res_q37 <- choose_chisq(sv_intv$q37_yn, sv_intv$q35_transport_freq)
biv4$q37_q35 <- tibble::tibble(
  Outcome = "Q37. Perceived reduction by transport allowance",
  Predictor = "Q35. Disbursement frequency",
  Test = res_q37$method, Statistic = res_q37$statistic, p = res_q37$p,
  Note = sprintf("n respondents = %d", sum(!is.na(sv_intv$q37_yn))))

biv4_combined <- dplyr::bind_rows(biv4) |>
  dplyr::mutate(p = fmt_p(p),
                Statistic = ifelse(is.na(Statistic), NA,
                                   formatC(Statistic, digits = 3, format = "f")))

ft_biv4 <- flextable::flextable(biv4_combined) |>
  flextable::set_caption("Table 4.3. Bivariate associations - intervention components and outcomes") |>
  flextable::autofit()
obj4_outputs[["Table 4.3 Bivariate associations"]] <- ft_biv4

# =============================================================================
# 4.3 MULTIVARIATE
# =============================================================================
# A. Multiple linear regression with Q34, Q40, Q46 cannot run (perfect
#    collinearity). We instead enter:
#       - Intervention Exposure Score (continuous, but binary 0/7 in this data)
#       - Cadre, gender (% female), fac_location as covariates
#    This estimates the association between exposure and absenteeism while
#    controlling for measurable confounders.

ext_for_lm4 <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(absent_rate),
                !is.na(cadre)) |>
  dplyr::left_join(sv_phc, by = "phc") |>
  dplyr::left_join(survey |>
                     dplyr::group_by(phc) |>
                     dplyr::summarise(exposure_phc = mean(exposure_idx, na.rm = TRUE),
                                      .groups = "drop"),
                   by = "phc")

mod4_lm <- lm(absent_rate ~ exposure_phc + cadre + fac_location +
                pct_female_phc + n_children_phc + n_adult_dep_phc,
              data = ext_for_lm4)

tbl_lm4 <- mod4_lm |>
  gtsummary::tbl_regression(intercept = TRUE,
                            label = list(
                              exposure_phc    ~ "Intervention Exposure Score (PHC)",
                              cadre           ~ "Cadre",
                              fac_location        ~ "Location",
                              pct_female_phc  ~ "% female (PHC)",
                              n_children_phc  ~ "Mean # children <12",
                              n_adult_dep_phc ~ "Mean # adult dependents"
                            )) |>
  gtsummary::add_glance_source_note(include = c(r.squared, adj.r.squared,
                                                nobs, p.value)) |>
  gtsummary::modify_caption("Table 4.4. Multiple linear regression - intervention exposure and absenteeism rate. NB: exposure_phc is bimodal (0 = control, 7 = intervention) so the coefficient quantifies the same group-difference as a 2-arm comparison.")
obj4_outputs[["Table 4.4 Linear regression - exposure"]] <- tbl_lm4

# B. Hierarchical (block) regression - within INTERVENTION sites only,
#    using component frequency/dose variables that DO vary at the PHC level.
#    Block 1: cadre + fac_location
#    Block 2: + transport_freq, peer_freq, sanc_consist (component dose proxies)
#    We compare Adjusted R^2 and report ANOVA model comparison.
ext_intv_for_lm4 <- ext |>
  dplyr::filter(facility_category == "Intervention",
                !is.na(absent_rate), !is.na(cadre)) |>
  dplyr::left_join(sv_phc |> dplyr::select(phc, fac_location), by = "phc") |>
  dplyr::left_join(phc_components, by = "phc") |>
  dplyr::mutate(
    transport_freq_f = factor(transport_freq_majority),
    peer_freq_f      = factor(peer_freq_majority),
    sanc_consist_f   = factor(sanc_consist_majority)
  )

# Drop levels with <5 obs to avoid rank-deficient design
trim_levels <- function(x, min_n = 5) {
  if (!is.factor(x)) return(x)
  tab <- table(x)
  keep <- names(tab[tab >= min_n])
  factor(ifelse(as.character(x) %in% keep, as.character(x), NA_character_),
         levels = keep)
}
ext_intv_for_lm4 <- ext_intv_for_lm4 |>
  dplyr::mutate(
    transport_freq_f = trim_levels(transport_freq_f),
    peer_freq_f      = trim_levels(peer_freq_f),
    sanc_consist_f   = trim_levels(sanc_consist_f),
    # Drop empty cadre/fac_location levels that survived from the full dataset
    # but have zero observations in the intervention-only subset
    cadre        = droplevels(cadre),
    fac_location = droplevels(fac_location)
  )

# Block 1
m_block1 <- lm(absent_rate ~ cadre + fac_location, data = ext_intv_for_lm4)
# Block 2 - try to add component dose variables that have >=2 levels
add_terms <- c()
for (v in c("transport_freq_f","peer_freq_f","sanc_consist_f")) {
  if (length(levels(ext_intv_for_lm4[[v]])) >= 2) add_terms <- c(add_terms, v)
}
if (length(add_terms) > 0) {
  fmla2 <- as.formula(paste("absent_rate ~ cadre + fac_location +",
                            paste(add_terms, collapse = " + ")))
  m_block2 <- lm(fmla2, data = ext_intv_for_lm4)
  comp <- anova(m_block1, m_block2)
  hier_df <- tibble::tibble(
    Block = c("Block 1: cadre + fac_location",
              paste("Block 2: + ", paste(add_terms, collapse = ", "))),
    `Adj R-sq` = formatC(c(summary(m_block1)$adj.r.squared,
                           summary(m_block2)$adj.r.squared),
                         digits = 3, format = "f"),
    `Delta R-sq` = c("-", formatC(summary(m_block2)$adj.r.squared -
                                    summary(m_block1)$adj.r.squared,
                                  digits = 3, format = "f")),
    `F change` = c("-", formatC(comp$F[2], digits = 3, format = "f")),
    `df1, df2` = c("-", sprintf("%d, %d", comp$Df[2], comp$Res.Df[2])),
    p = c("-", fmt_p(comp$`Pr(>F)`[2]))
  )
  ft_hier <- flextable::flextable(hier_df) |>
    flextable::set_caption("Table 4.5. Hierarchical (block) regression - incremental contribution of intervention component dose variables (intervention sites only)") |>
    flextable::autofit()
  obj4_outputs[["Table 4.5 Hierarchical regression"]] <- ft_hier
  
  # Show Block 2 coefficients
  tbl_b2 <- m_block2 |>
    gtsummary::tbl_regression(intercept = TRUE) |>
    gtsummary::modify_caption("Table 4.5a. Block 2 coefficients - component dose effects within intervention arm")
  obj4_outputs[["Table 4.5a Block 2 coefficients"]] <- tbl_b2
} else {
  obj4_outputs[["Table 4.5 Hierarchical regression"]] <-
    flextable::flextable(tibble::tibble(
      Note = "Hierarchical regression skipped: no component dose variable has >=2 categories with sufficient observations within intervention arm."
    )) |> flextable::autofit()
}

# =============================================================================
# 4.4 Write to docx
# =============================================================================
out_path <- file.path(OUTPUT_DIR, "Objective4_Components.docx")
save_to_docx(obj4_outputs, out_path,
             title = "Objective 4 - Relative contribution of intervention components")