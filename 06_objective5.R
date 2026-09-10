# =============================================================================
# ACE-II Evaluation
# Script 06: OBJECTIVE 5 - Implementation fidelity, adoption, sustainability
#
# Run AFTER 00_setup.R and 01_preprocessing.R.
# Output: outputs/Objective5_Implementation.docx
#         outputs/Objective5_RadarPlot.pdf
#
# CAVEAT: PCI and RE-AIM items (Q51-Q82) are only filled for intervention
# respondents (n approx 39) by ODK skip logic. All Objective 5 analyses
# operate on the intervention subsample. The DAP acknowledges this.
# =============================================================================

stopifnot(exists("survey"))

obj5_outputs <- list()

sv_intv <- survey |>
  dplyr::filter(facility_category == "Intervention")

# =============================================================================
# 5.1 UNIVARIATE
# =============================================================================

# --- 5.1.1 RE-AIM item-level frequencies + domain scores -----------------------
tbl_reaim_items <- sv_intv |>
  dplyr::select(q67_proportion,
                q68_abs_reduced, q69_punct_improved, q70_leave_reduced,
                q71_unintended, q72_service, q73_commit_improved,
                q74_lead_approve, q75_staff_willing,
                q76_transport_sched, q77_peer_planned,
                q78_cmsc_planned, q79_adherence,
                q80_ongoing, q81_integrated, q82_likely_12mo) |>
  gtsummary::tbl_summary(
    missing = "ifany",
    label = list(
      q67_proportion       ~ "Q67. Proportion of staff benefited (Reach)",
      q68_abs_reduced      ~ "Q68. Extent absenteeism reduced (Effectiveness)",
      q69_punct_improved   ~ "Q69. Punctuality improved (Effectiveness)",
      q70_leave_reduced    ~ "Q70. Leaving early reduced (Effectiveness)",
      q71_unintended       ~ "Q71. Unintended consequences (Effectiveness)",
      q72_service          ~ "Q72. Service delivery improved (Effectiveness)",
      q73_commit_improved  ~ "Q73. Staff commitment improved (Effectiveness)",
      q74_lead_approve     ~ "Q74. Leadership approved (Adoption)",
      q75_staff_willing    ~ "Q75. Staff willingness (Adoption)",
      q76_transport_sched  ~ "Q76. Transport on schedule (Implementation)",
      q77_peer_planned     ~ "Q77. Peer meetings as planned (Implementation)",
      q78_cmsc_planned     ~ "Q78. CMSC visits as planned (Implementation)",
      q79_adherence        ~ "Q79. Overall design adherence (Implementation)",
      q80_ongoing          ~ "Q80. Still ongoing (Maintenance)",
      q81_integrated       ~ "Q81. Integrated into routine ops (Maintenance)",
      q82_likely_12mo      ~ "Q82. Likely active in 12 mo (Maintenance)"
    )
  ) |>
  gtsummary::modify_caption("Table 5.1. RE-AIM item frequencies, intervention sites only (n approx 39)")
obj5_outputs[["Table 5.1 RE-AIM items"]] <- tbl_reaim_items

# Domain scores summary
tbl_domain <- sv_intv |>
  dplyr::select(reaim_reach, reaim_eff, reaim_adopt,
                reaim_implem, reaim_maint, reaim_total,
                pci_power, pci_capabil, pci_interest) |>
  gtsummary::tbl_summary(
    missing = "ifany",
    type = list(gtsummary::all_continuous() ~ "continuous2"),
    statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                     "{median} [{p25}, {p75}]")),
    digits = list(gtsummary::all_continuous() ~ 2),
    label = list(
      reaim_reach   ~ "RE-AIM: Reach (Q67)",
      reaim_eff     ~ "RE-AIM: Effectiveness (Q68-73)",
      reaim_adopt   ~ "RE-AIM: Adoption (Q74-75)",
      reaim_implem  ~ "RE-AIM: Implementation (Q76-79)",
      reaim_maint   ~ "RE-AIM: Maintenance (Q80-82)",
      reaim_total   ~ "RE-AIM Composite",
      pci_power     ~ "PCI Power (Q51-55)",
      pci_capabil   ~ "PCI Capabilities (Q56-61)",
      pci_interest  ~ "PCI Interest (Q62-66)"
    )
  ) |>
  gtsummary::modify_caption("Table 5.2. RE-AIM domain scores and PCI component scores - intervention sites")
obj5_outputs[["Table 5.2 RE-AIM domains + PCI scores"]] <- tbl_domain

# --- 5.1.2 Radar/spider chart of RE-AIM domain profile (per facility) ------
# Normalise each domain score to 0-100 of its theoretical max.
# Theoretical maxima (verified against scoring in 01_preprocessing.R):
#   Reach Q67       : 5 levels via score_or_na -> 0..4, max = 4
#   Effectiveness   : Q68(max 3) + Q69(2) + Q70(2) + Q71(2) + Q72(3) + Q73(2) = 14
#   Adoption        : Q74(max 2) + Q75(max 3) = 5
#   Implementation  : Q76(2) + Q77(3) + Q78(2) + Q79(3) = 10
#   Maintenance     : Q80(2) + Q81(2) + Q82(4) = 8
phc_radar <- sv_intv |>
  dplyr::group_by(phc) |>
  dplyr::summarise(
    Reach          = 100 * mean(reaim_reach, na.rm = TRUE) / 4,
    Effectiveness  = 100 * mean(reaim_eff,   na.rm = TRUE) / 14,
    Adoption       = 100 * mean(reaim_adopt, na.rm = TRUE) / 5,
    Implementation = 100 * mean(reaim_implem,na.rm = TRUE) / 10,
    Maintenance    = 100 * mean(reaim_maint, na.rm = TRUE) / 8,
    .groups = "drop"
  )

# Build a tidy long version for ggplot polar-coordinate radar
radar_long <- phc_radar |>
  tidyr::pivot_longer(-phc, names_to = "Domain", values_to = "Score") |>
  dplyr::mutate(Domain = factor(Domain,
                                levels = c("Reach","Effectiveness","Adoption",
                                           "Implementation","Maintenance")))

p_radar <- ggplot2::ggplot(radar_long,
                           ggplot2::aes(x = Domain, y = Score,
                                        fill = Domain)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", Score)),
                     vjust = -0.3, size = 3) +
  ggplot2::facet_wrap(~ phc, ncol = 2) +
  ggplot2::scale_y_continuous(limits = c(0, 110),
                              breaks = seq(0, 100, 25)) +
  ggplot2::labs(title = "RE-AIM domain profile by intervention facility",
                subtitle = "Each domain expressed as % of theoretical maximum",
                y = "Score (% of maximum)", x = NULL) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                 strip.text = ggplot2::element_text(face = "bold"))

# Optional radar version (community package fmsb if user has it installed)
# Saved as the main figure here:
fig_path <- file.path(OUTPUT_DIR, "Objective5_RadarPlot.pdf")
ggplot2::ggsave(fig_path, p_radar, width = 9, height = 8)
message("Wrote: ", fig_path)

# Also append the radar source table to the docx
ft_radar <- phc_radar |>
  dplyr::mutate(dplyr::across(where(is.numeric),
                              ~ formatC(.x, digits = 1, format = "f"))) |>
  flextable::flextable() |>
  flextable::set_caption("Table 5.3. Source data for radar plot - normalised RE-AIM domain scores (% of theoretical maximum) by facility") |>
  flextable::autofit()
obj5_outputs[["Table 5.3 RE-AIM by facility (radar source)"]] <- ft_radar

# =============================================================================
# 5.2 BIVARIATE
# =============================================================================
biv5 <- list()

# (a) RE-AIM Implementation score vs cadre (ANOVA / Kruskal)
res <- choose_kgroup_test(sv_intv$reaim_implem, sv_intv$cadre)
biv5$impl_cadre <- tibble::tibble(
  Outcome = "RE-AIM Implementation Score",
  Predictor = "Cadre",
  Test = res$method, Statistic = res$statistic, p = res$p)

# (b) RE-AIM Implementation score vs fac_location
res <- choose_kgroup_test(sv_intv$reaim_implem, sv_intv$fac_location)
biv5$impl_loc <- tibble::tibble(
  Outcome = "RE-AIM Implementation Score",
  Predictor = "Location",
  Test = res$method, Statistic = res$statistic, p = res$p)

# (c) RE-AIM Maintenance score vs PCI Interest (correlation)
res <- choose_corr_test(sv_intv$reaim_maint, sv_intv$pci_interest)
biv5$maint_int <- tibble::tibble(
  Outcome = "RE-AIM Maintenance Score",
  Predictor = "PCI Interest Score",
  Test = res$method, Statistic = res$estimate, p = res$p)

# (d) Adoption Q74 binary vs PCI Power Score (point-biserial via correlation;
#     equivalent to t-test mathematically)
ok <- complete.cases(sv_intv$q74_lead_approve_bin, sv_intv$pci_power)
if (sum(ok) >= 5 && length(unique(sv_intv$q74_lead_approve_bin[ok])) == 2) {
  ct <- suppressWarnings(cor.test(sv_intv$q74_lead_approve_bin[ok],
                                  sv_intv$pci_power[ok], method = "pearson"))
  biv5$adopt_power <- tibble::tibble(
    Outcome = "Q74 Leadership approval (binary)",
    Predictor = "PCI Power Score",
    Test = "Point-biserial correlation",
    Statistic = unname(ct$estimate), p = ct$p.value)
} else {
  biv5$adopt_power <- tibble::tibble(
    Outcome = "Q74 Leadership approval (binary)",
    Predictor = "PCI Power Score",
    Test = "Not run (insufficient variation)",
    Statistic = NA_real_, p = NA_real_)
}

# (e) RE-AIM Effectiveness vs Intervention Exposure Score
res <- choose_corr_test(sv_intv$reaim_eff, sv_intv$exposure_idx)
biv5$eff_exp <- tibble::tibble(
  Outcome = "RE-AIM Effectiveness Score",
  Predictor = "Intervention Exposure Score",
  Test = res$method, Statistic = res$estimate, p = res$p)

# (f) Sustainability likelihood Q82 (ordinal) vs PCI Capabilities (Spearman)
ok <- complete.cases(sv_intv$q82_score, sv_intv$pci_capabil)
ct <- suppressWarnings(cor.test(sv_intv$q82_score[ok],
                                sv_intv$pci_capabil[ok], method = "spearman"))
biv5$q82_cap <- tibble::tibble(
  Outcome = "Q82 Sustainability likelihood (ordinal)",
  Predictor = "PCI Capabilities Score",
  Test = "Spearman rank correlation",
  Statistic = unname(ct$estimate), p = ct$p.value)

biv5_combined <- dplyr::bind_rows(biv5) |>
  dplyr::mutate(p = fmt_p(p),
                Statistic = ifelse(is.na(Statistic), NA,
                                   formatC(Statistic, digits = 3, format = "f")))

ft_biv5 <- flextable::flextable(biv5_combined) |>
  flextable::set_caption("Table 5.4. Bivariate associations - implementation, adoption, sustainability (intervention sites only)") |>
  flextable::autofit()
obj5_outputs[["Table 5.4 Bivariate associations"]] <- ft_biv5

# =============================================================================
# 5.3 MULTIVARIATE
# =============================================================================

# --- 5.3.1 Multiple linear regression: RE-AIM Composite ~ PCI components ---
mod5_lm <- lm(reaim_total ~ pci_power + pci_capabil + pci_interest +
                cadre + fac_location + governance_idx,
              data = sv_intv)

tbl_lm5 <- mod5_lm |>
  gtsummary::tbl_regression(intercept = TRUE,
                            label = list(
                              pci_power      ~ "PCI Power Score",
                              pci_capabil    ~ "PCI Capabilities Score",
                              pci_interest   ~ "PCI Interest Score",
                              cadre          ~ "Cadre",
                              fac_location       ~ "Location",
                              governance_idx ~ "Governance Strength Index"
                            )) |>
  gtsummary::add_glance_source_note(include = c(r.squared, adj.r.squared,
                                                nobs, p.value)) |>
  gtsummary::modify_caption("Table 5.5. Multiple linear regression - PCI components predicting RE-AIM Composite Score")
obj5_outputs[["Table 5.5 Linear regression on RE-AIM composite"]] <- tbl_lm5

# Standardised beta coefficients to compare relative importance
sv_for_std <- sv_intv |>
  dplyr::filter(!is.na(reaim_total), !is.na(pci_power), !is.na(pci_capabil),
                !is.na(pci_interest)) |>
  dplyr::mutate(dplyr::across(c(reaim_total, pci_power, pci_capabil,
                                pci_interest, governance_idx),
                              ~ as.numeric(scale(.x))))
mod5_std <- tryCatch(
  lm(reaim_total ~ pci_power + pci_capabil + pci_interest + governance_idx,
     data = sv_for_std),
  error = function(e) NULL
)
if (!is.null(mod5_std)) {
  std_df <- broom::tidy(mod5_std) |>
    dplyr::mutate(p.value = fmt_p(p.value),
                  dplyr::across(c(estimate, std.error, statistic),
                                ~ formatC(.x, digits = 3, format = "f"))) |>
    dplyr::select(Term = term, `Standardised beta` = estimate,
                  SE = std.error, t = statistic, p = p.value)
  ft_std <- flextable::flextable(std_df) |>
    flextable::set_caption("Table 5.5a. Standardised beta coefficients - relative importance of PCI dimensions for RE-AIM Composite") |>
    flextable::autofit()
  obj5_outputs[["Table 5.5a Standardised betas"]] <- ft_std
}

# --- 5.3.2 Ordinal logistic regression: Q82 sustainability ----------------
sv_ord <- sv_intv |>
  dplyr::filter(!is.na(q82_likely_ord)) |>
  dplyr::mutate(q82_likely_ord = droplevels(q82_likely_ord))

if (nlevels(sv_ord$q82_likely_ord) >= 2 && nrow(sv_ord) >= 10) {
  ord_fit <- tryCatch(
    MASS::polr(q82_likely_ord ~ pci_power + pci_capabil + pci_interest +
                 reaim_implem + cadre + fac_location,
               data = sv_ord, Hess = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  
  if (!is.null(ord_fit)) {
    tbl_ord <- ord_fit |>
      gtsummary::tbl_regression(exponentiate = TRUE,
                                label = list(
                                  pci_power     ~ "PCI Power",
                                  pci_capabil   ~ "PCI Capabilities",
                                  pci_interest  ~ "PCI Interest",
                                  reaim_implem  ~ "RE-AIM Implementation Score",
                                  cadre         ~ "Cadre",
                                  fac_location      ~ "Location"
                                )) |>
      gtsummary::modify_caption("Table 5.6. Ordinal logistic regression - predictors of Q82 sustainability likelihood")
    obj5_outputs[["Table 5.6 Ordinal logit Q82"]] <- tbl_ord
    
    # Brant test for proportional odds
    brant_res <- tryCatch(
      brant::brant(ord_fit, by.var = FALSE),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(brant_res)) {
      bdf <- as.data.frame(brant_res) |>
        tibble::rownames_to_column("Variable")
      if ("probability" %in% names(bdf)) {
        bdf$probability <- fmt_p(bdf$probability)
      }
      ft_brant <- flextable::flextable(bdf) |>
        flextable::set_caption("Table 5.6a. Brant test for proportional-odds assumption (probability > 0.05 supports the assumption)") |>
        flextable::autofit()
      obj5_outputs[["Table 5.6a Brant test"]] <- ft_brant
    }
  } else {
    obj5_outputs[["Table 5.6 Ordinal logit"]] <-
      flextable::flextable(tibble::tibble(
        Note = "Ordinal logistic regression failed to converge (likely too few cases per category)."
      )) |> flextable::autofit()
  }
} else {
  obj5_outputs[["Table 5.6 Ordinal logit"]] <-
    flextable::flextable(tibble::tibble(
      Note = sprintf("Ordinal regression skipped: %d cases, %d categories.",
                     nrow(sv_ord), nlevels(sv_ord$q82_likely_ord))
    )) |> flextable::autofit()
}

# =============================================================================
# 5.4 Write to docx
# =============================================================================
out_path <- file.path(OUTPUT_DIR, "Objective5_Implementation.docx")
save_to_docx(obj5_outputs, out_path,
             title = "Objective 5 - Implementation fidelity, adoption, sustainability")