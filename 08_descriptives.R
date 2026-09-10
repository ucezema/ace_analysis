# =============================================================================
# ACE-II Evaluation
# Descriptive statistics for both datasets (provider survey + attendance
# extraction), grouped by questionnaire section, using gtsummary::tbl_summary.
#
# Output: outputs/ACE2_Descriptive_Statistics.docx
#
# Style (matches the AiA reference document):
#   - Bold column headers
#   - Variable labels (question text) shown in bold italic
#   - Categorical: n (%);  Continuous: mean (SD) and median [IQR]
#   - 95% CIs added on the key continuous outcomes only
#   - Cross-tabs where natural (stratified columns)
#   - Box plots / histograms / scatter for continuous variables
#
# Stratification:
#   - Survey      -> by facility category (Intervention vs Control) + Overall
#   - Extraction  -> by period (Pre-intervention vs Intervention) + Overall
#
# Run:  Rscript 08_descriptives.R
# Requires: tidyverse, gtsummary, flextable, officer, readxl, lubridate, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(lubridate)
  library(forcats)
  library(gtsummary)
  library(flextable)
  library(officer)
  library(ggplot2)
})

# Force gtsummary to use flextable for Word output, and set a clean theme
gtsummary::theme_gtsummary_compact()

SURVEY_FILE     <- "data/ACE2_Provider_Survey_Manual_Rebuild_data.xlsx"
SURVEY_SHEET    <- "ace1"
EXTRACTION_FILE <- "data/ace2_extraction.xlsx"
EXTRACTION_SHEET<- "Merged Data"
OUT_DOCX        <- "outputs/ACE2_Descriptive_Statistics.docx"

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/fig", showWarnings = FALSE, recursive = TRUE)

# Helper: collapse a tbl_summary into a flextable with the house style ------
# tbl_summary already bolds the header via modify; we additionally bold-italicise
# the variable labels (the rows where row_type == "label").
as_styled_flex <- function(tbl, caption) {
  ft <- tbl |>
    gtsummary::as_flex_table()
  ft <- ft |>
    flextable::set_caption(caption) |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::autofit()
  ft
}

# Helper: build a tbl_summary with our standard statistics -------------------
# `by_var` may be NULL for an overall-only table.
make_summary <- function(data, vars, labels, by_var = NULL,
                         ci_vars = character(0)) {
  d <- data |> dplyr::select(dplyr::all_of(c(by_var, vars)))
  
  tbl <- d |>
    gtsummary::tbl_summary(
      by = if (is.null(by_var)) NULL else by_var,
      missing = "ifany",
      missing_text = "(Missing)",
      type = list(
        gtsummary::all_continuous() ~ "continuous2"
      ),
      statistic = list(
        gtsummary::all_continuous()  ~ c("{mean} ({sd})", "{median} [{p25}, {p75}]"),
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(gtsummary::all_continuous() ~ 1),
      label = labels
    )
  
  if (!is.null(by_var)) {
    tbl <- tbl |> gtsummary::add_overall()
  }
  
  # Add 95% CI for the requested continuous outcomes (mean CI) via add_ci.
  # Wrapped defensively: gtsummary's add_ci signature has changed across
  # versions, so if it errors we keep the table without CIs rather than abort.
  if (length(ci_vars) > 0) {
    tbl <- tryCatch(
      tbl |> gtsummary::add_ci(include = dplyr::all_of(ci_vars)),
      error = function(e) {
        message("add_ci skipped (", conditionMessage(e), ")")
        tbl
      }
    )
  }
  
  tbl <- tbl |>
    gtsummary::bold_labels() |>
    gtsummary::italicize_labels() |>
    gtsummary::modify_header(label = "**Characteristic**")
  
  tbl
}

# =============================================================================
# LOAD + PREP: SURVEY
# =============================================================================
sv <- read_excel(SURVEY_FILE, sheet = SURVEY_SHEET, guess_max = 5000)

# Convenience renamer: pull a column by its exact header into a short name.
col <- function(df, header) df[[header]]

# Some headers contain characters that are awkward to type literally in R
# (non-breaking hyphen, en-dash, embedded tab). Rather than risk a mismatch,
# resolve those columns by their leading "Qn." prefix via regex on the names,
# and copy them into clean, plainly-named helper columns BEFORE the main
# mutate(). Doing this inside mutate() fails because dplyr treats the bare
# data-frame argument as a column-selection context.
pick_name <- function(df, prefix_regex) {
  hit <- grep(prefix_regex, names(df), value = TRUE)
  if (length(hit) == 0) stop("No column matches: ", prefix_regex)
  hit[1]
}

# Pre-attach the nine special-character columns as clean helper columns.
sv[["q12_oic_check_src"]]        <- sv[[pick_name(sv, "^12\\. How often does the OIC check")]]
sv[["q30_local_monitor_src"]]    <- sv[[pick_name(sv, "^30\\. 30\\.")]]
sv[["q53_sanction_consist_src"]] <- sv[[pick_name(sv, "^53\\. During the intervention period")]]
sv[["q61_capacity_src"]]         <- sv[[pick_name(sv, "^61\\.")]]
sv[["q64_lead_commit_src"]]      <- sv[[pick_name(sv, "^64\\. During August 2025")]]
sv[["q65_comm_support_src"]]     <- sv[[pick_name(sv, "^65\\.")]]
sv[["q66_lead_interest_src"]]    <- sv[[pick_name(sv, "^66\\.")]]
sv[["q67_reach_src"]]            <- sv[[pick_name(sv, "^67\\.")]]
sv[["q80_ongoing_src"]]          <- sv[[pick_name(sv, "^80\\.")]]

sv <- sv |>
  mutate(
    facility_category = case_when(
      str_detect(tolower(`Is this facility an intervention or control site?`), "intervention") ~ "Intervention",
      str_detect(tolower(`Is this facility an intervention or control site?`), "control") ~ "Control",
      TRUE ~ NA_character_),
    facility_category = factor(facility_category, levels = c("Control", "Intervention")),
    lga = `Select LGA`,
    gender = `3. Gender of health worker`,
    fac_location = `4. Location of facility`,
    cadre_raw = `7. Cadre of respondent`,
    n_children_u12 = as.numeric(`8. Number of children under 12 years under your care`),
    n_adult_dep    = as.numeric(`9. Number of adult dependents under your care`),
    walk_distance  = `10. Does the health worker live within walking distance of the facility?`,
    register_kept  = `11. Does the facility keep an attendance register?`,
    oic_check      = q12_oic_check_src,
    register_verified = `13. Enumerator verified the attendance register exists?`,
    n_present_today = as.numeric(`20. Number of health workers meant to be present today`),
    n_absent_today  = as.numeric(`21. Number of health workers meant to be present today but absent`),
    abs_most_common = `19. Which absenteeism type is most common?`,
    sanctions_used  = `25. Have sanctions been used in the past 6 months?`,
    incentive_freq  = `28. How often do incentives come?`,
    wdc_monitor     = `29. Are health workers monitored by WDC/HFC for absenteeism?`,
    local_monitor_obs = q30_local_monitor_src,
    community_support = `31. Does the community provide support to the facility?`,
    # Intervention components
    transport_disbursed = `34. Was a transport allowance disbursed in the past 6 months?`,
    transport_freq      = `35. Frequency of transport allowance disbursement`,
    transport_reduce    = `37. Did transport allowance reduce absenteeism?`,
    transport_punct     = `38. Did transport allowance improve punctuality?`,
    peer_established     = `40. Was a peer support network established?`,
    peer_freq            = `41. Frequency of peer support meetings`,
    peer_teamwork        = `43. Did the peer network improve teamwork?`,
    peer_reduce          = `44. Did the peer network reduce absenteeism?`,
    cmsc_established     = `46. Was a CMSC established or strengthened?`,
    cmsc_monitor         = `47. Does CMSC actively monitor staff attendance?`,
    cmsc_freq            = `48. Frequency CMSC engages facility`,
    cmsc_reduce          = `49. Did CMSC monitoring reduce absenteeism?`,
    # Authority / consistency
    authority_sanction   = `51. Who has the authority to sanction absenteeism?`,
    formal_authority     = `52. Does the facility have formal authority to sanction absenteeism?`,
    sanction_consistency = q53_sanction_consist_src,
    cmsc_influence       = `54. Influence of CMSC in promoting attendance`,
    peer_importance      = `55. Importance of peer support network in preventing absenteeism`,
    # Systems / capacity
    register_before_jul  = `56. Did facility maintain attendance register before July 2025?`,
    records_reviewed_freq= `57. During the period of the intervention, how frequently were attendance records reviewed by any supervising agency/body from either the SPHCDA or the LGA?`,
    doc_system_exists    = `58. System for documenting absenteeism exists`,
    records_for_mgmt     = `59. Attendance records used for management decisions`,
    sufficient_staff     = `60. During the period of the interventiondid the facility have sufficient staff to ensure service coverage even when some staff were absent?`,
    capacity_sustain     = q61_capacity_src,
    # PCI perceptions
    pci_importance_before= `62. Before August  2025, how important was reducing staff absenteeism to the functioning of this facility?`,
    pci_commit_before    = `63. Before August 2025, how committed were staff to reducing absenteeism?`,
    pci_lead_commit      = q64_lead_commit_src,
    pci_comm_support     = q65_comm_support_src,
    pci_lead_interest    = q66_lead_interest_src,
    # RE-AIM
    reaim_reach          = q67_reach_src,
    reaim_abs_reduced    = `68. As of February 2026, to what extent has absenteeism reduced in this facility?`,
    reaim_punct          = `69. As of February 2026, had staff punctuality improved?`,
    reaim_leave_reduced  = `70. As of February 2026, had staff leaving before close of work or shift reduced?`,
    reaim_unintended     = `71. During August 2025 to February 2026, were there any unintended negative consequences?`,
    reaim_service        = `72. Did reduced absenteeism improve service delivery in the facility?`,
    reaim_commit         = `73. Did overall staff commitment to work improve?`,
    reaim_lead_approve   = `74. Did facility leadership approve the interventions?`,
    reaim_staff_willing  = `75. Staff willingness to participate at start`,
    reaim_transport_sched= `76. During August 2025 to February 2026, were transport allowances disbursed according to the planned schedule?`,
    reaim_peer_planned   = `77. During August 2025 to February 2026, were peer support meetings conducted as originally planned?`,
    reaim_cmsc_planned   = `78. During August 2025 to February 2026, did the CMSC conduct monitoring visits as scheduled?`,
    reaim_adherence      = `79. Overall, how closely did the facility adhere to the original intervention design between August 2025 to February 2026?`,
    reaim_ongoing        = q80_ongoing_src,
    reaim_integrated     = `81. Have any of the interventions been integrated into routine facility operations after February 2026?`,
    reaim_likely_12mo    = `82. How likely is it that these interventions will still be active 12 months after February 2026?`
  )

# Harmonise cadre into a tidy factor
harmonise_cadre <- function(x){
  s <- toupper(str_trim(as.character(x)))
  dplyr::case_when(
    s=="" | is.na(s) ~ NA_character_,
    str_detect(s,"PHYSIC|DOCTOR|MEDICAL OFFICER") ~ "Physician",
    str_detect(s,"MIDWIFE|NURSE") ~ "Nurse/Midwife",
    str_detect(s,"JCHEW|J\\.?CHEW|JUNIOR CHEW") ~ "JCHEW",
    str_detect(s,"AD-?HOC|ADHOC") ~ "Ad-hoc",
    str_detect(s,"VOLUNT") ~ "Volunteer",
    str_detect(s,"CHEW|CHO\\b|SCHEW|PCHEW|HCHEW|DDCHEW|CCHEW|ACCHEW|DDCHT|CCO\\b") ~ "CHEW",
    str_detect(s,"\\bH[\\./]?A\\b|HEALTH ATTENDANT|^SHA$|^PHA$|HELPING STAFF") ~ "Health Attendant",
    str_detect(s,"LAB|SMLT|PHARM|MLT|TECH") ~ "Lab/Tech",
    str_detect(s,"ADMIN|MESSENGER|FSO|COMMUNITY HEALTH EDUC|^DDNS$|^DDSN$|^ASNS$|P-PTECH") ~ "Admin/Other support",
    TRUE ~ "Other")
}
sv <- sv |> mutate(cadre = harmonise_cadre(cadre_raw))

# Cadre count columns (numeric, present-on-interview-date) for staffing table
# Headers 27-41 are repeated 'a. Physicians' etc. Use position-based access.
# Expected scheduled = cols 27-33; Actual present = cols 35-41 (1-indexed)
sched_idx <- 27:33
pres_idx  <- 35:41
cadre_lbls <- c("Physicians","Nurses","CHEW","JCHEW","Volunteers","Ad-hoc","Others")
sv_raw <- read_excel(SURVEY_FILE, sheet = SURVEY_SHEET, guess_max = 5000,
                     .name_repair = "minimal")
for (k in seq_along(cadre_lbls)) {
  sv[[paste0("sched_", cadre_lbls[k])]] <- as.numeric(sv_raw[[sched_idx[k]]])
  sv[[paste0("present_", cadre_lbls[k])]] <- as.numeric(sv_raw[[pres_idx[k]]])
}

# ---- Multi-select (check-all) groups: compute Yes/No per option ------------
# For Q18 (types of absenteeism), Q23 (sanctions exist), Q26 (incentives),
# Q17 (local structures), Q32 (community support). These are stored as separate
# 0/1 or TRUE/blank columns in the export.
yn_from_check <- function(x) {
  # KoboToolbox check-all columns are usually 1/0 or "True"/blank
  v <- tolower(as.character(x))
  factor(ifelse(v %in% c("1","true","yes"), "Yes", "No"), levels = c("No","Yes"))
}
sv <- sv |>
  mutate(
    abs_late   = yn_from_check(`18. Types of absenteeism observed in the last 6 months/Coming late to work`),
    abs_early  = yn_from_check(`18. Types of absenteeism observed in the last 6 months/Leaving before shift ends`),
    abs_fullday= yn_from_check(`18. Types of absenteeism observed in the last 6 months/Not coming to work for a whole day`),
    sanc_late  = yn_from_check(`23. Are there sanctions for any of all the following types of absenteeism?/Coming late to work`),
    sanc_early = yn_from_check(`23. Are there sanctions for any of all the following types of absenteeism?/Leaving before shift ends`),
    sanc_fullday=yn_from_check(`23. Are there sanctions for any of all the following types of absenteeism?/Not coming to work for a whole day`),
    inc_money  = yn_from_check(`26. Incentives for punctual staff/Money`),
    inc_food   = yn_from_check(`26. Incentives for punctual staff/Food`),
    inc_promo  = yn_from_check(`26. Incentives for punctual staff/Promotion`),
    inc_recog  = yn_from_check(`26. Incentives for punctual staff/Recognition`),
    struct_vda = yn_from_check(`17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Village Development Association`),
    struct_tu  = yn_from_check(`17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Town Union`),
    struct_trad= yn_from_check(`17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Local Traditional Leadership`),
    struct_wdc = yn_from_check(`17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/WDC`),
    struct_hfc = yn_from_check(`17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Health Facility Committee`),
    csupport_security = yn_from_check(`32. What type of support does the community provide?/Security`),
    csupport_housing  = yn_from_check(`32. What type of support does the community provide?/Staff housing`),
    csupport_cash     = yn_from_check(`32. What type of support does the community provide?/Cash support`),
    csupport_cleaning = yn_from_check(`32. What type of support does the community provide?/Cleaning the premises`),
    csupport_materials= yn_from_check(`32. What type of support does the community provide?/Supply of essential materials needed in the health facility`)
  )

# =============================================================================
# SURVEY TABLES (by facility category)
# =============================================================================
survey_tables <- list()

# ---- Table S1: Respondent & facility profile -------------------------------
survey_tables[["S1. Respondent and facility profile"]] <- make_summary(
  sv,
  vars = c("lga","gender","fac_location","cadre",
           "n_children_u12","n_adult_dep","walk_distance"),
  by_var = "facility_category",
  labels = list(
    lga ~ "Local Government Area",
    gender ~ "Gender of health worker",
    fac_location ~ "Location of facility",
    cadre ~ "Cadre of respondent",
    n_children_u12 ~ "Number of children under 12 under care",
    n_adult_dep ~ "Number of adult dependents under care",
    walk_distance ~ "Lives within walking distance of facility"
  )
)

# ---- Table S2: Attendance register & monitoring ----------------------------
survey_tables[["S2. Attendance register and monitoring"]] <- make_summary(
  sv,
  vars = c("register_kept","oic_check","register_verified",
           "wdc_monitor","local_monitor_obs","register_before_jul",
           "records_reviewed_freq","doc_system_exists","records_for_mgmt"),
  by_var = "facility_category",
  labels = list(
    register_kept ~ "Facility keeps an attendance register (Q11)",
    oic_check ~ "OIC checks/cross-signs the register (Q12)",
    register_verified ~ "Enumerator verified register exists (Q13)",
    wdc_monitor ~ "WDC/HFC monitors absenteeism (Q29)",
    local_monitor_obs ~ "Direct observation of local monitoring (Q30)",
    register_before_jul ~ "Maintained register before July 2025 (Q56)",
    records_reviewed_freq ~ "Frequency records reviewed by supervisor (Q57)",
    doc_system_exists ~ "Documentation system exists (Q58)",
    records_for_mgmt ~ "Records used for management decisions (Q59)"
  )
)

# ---- Table S3: Staffing on interview day (continuous) ----------------------
staffing_vars <- c("n_present_today","n_absent_today",
                   paste0("sched_", cadre_lbls), paste0("present_", cadre_lbls))
staffing_labels <- c(
  "Health workers expected present today (Q20)",
  "Expected but absent today (Q21)",
  paste0("Scheduled on duty \u2013 ", cadre_lbls),
  paste0("Actually present \u2013 ", cadre_lbls)
)
staffing_label_list <- mapply(
  function(nm, lab) stats::as.formula(paste0("`", nm, "` ~ \"", lab, "\"")),
  staffing_vars, staffing_labels, SIMPLIFY = FALSE, USE.NAMES = FALSE
)
survey_tables[["S3. Staffing on interview day"]] <- make_summary(
  sv,
  vars = staffing_vars,
  by_var = "facility_category",
  labels = staffing_label_list
)

# ---- Table S4: Types of absenteeism & most common --------------------------
survey_tables[["S4. Types of absenteeism observed"]] <- make_summary(
  sv,
  vars = c("abs_late","abs_early","abs_fullday","abs_most_common"),
  by_var = "facility_category",
  labels = list(
    abs_late ~ "Coming late observed (Q18a)",
    abs_early ~ "Leaving before shift ends observed (Q18b)",
    abs_fullday ~ "Full-day absence observed (Q18c)",
    abs_most_common ~ "Most common type of absenteeism (Q19)"
  )
)

# ---- Table S5: Sanctions ---------------------------------------------------
survey_tables[["S5. Sanctions for absenteeism"]] <- make_summary(
  sv,
  vars = c("sanc_late","sanc_early","sanc_fullday","sanctions_used"),
  by_var = "facility_category",
  labels = list(
    sanc_late ~ "Sanction exists for lateness (Q23a)",
    sanc_early ~ "Sanction exists for leaving early (Q23b)",
    sanc_fullday ~ "Sanction exists for full-day absence (Q23c)",
    sanctions_used ~ "Sanctions used in past 6 months (Q25)"
  )
)

# ---- Table S6: Incentives --------------------------------------------------
survey_tables[["S6. Incentives for punctual staff"]] <- make_summary(
  sv,
  vars = c("inc_money","inc_food","inc_promo","inc_recog","incentive_freq"),
  by_var = "facility_category",
  labels = list(
    inc_money ~ "Money incentive (Q26)",
    inc_food ~ "Food incentive (Q26)",
    inc_promo ~ "Promotion incentive (Q26)",
    inc_recog ~ "Recognition incentive (Q26)",
    incentive_freq ~ "How often incentives come (Q28)"
  )
)

# ---- Table S7: Local structures & community support ------------------------
survey_tables[["S7. Local structures and community support"]] <- make_summary(
  sv,
  vars = c("struct_vda","struct_tu","struct_trad","struct_wdc","struct_hfc",
           "community_support",
           "csupport_security","csupport_housing","csupport_cash",
           "csupport_cleaning","csupport_materials"),
  by_var = "facility_category",
  labels = list(
    struct_vda ~ "Village Development Association (Q17)",
    struct_tu ~ "Town Union (Q17)",
    struct_trad ~ "Local Traditional Leadership (Q17)",
    struct_wdc ~ "Ward Development Committee (Q17)",
    struct_hfc ~ "Health Facility Committee (Q17)",
    community_support ~ "Community provides support (Q31)",
    csupport_security ~ "Support: security (Q32)",
    csupport_housing ~ "Support: staff housing (Q32)",
    csupport_cash ~ "Support: cash (Q32)",
    csupport_cleaning ~ "Support: cleaning premises (Q32)",
    csupport_materials ~ "Support: essential materials (Q32)"
  )
)

# ---- Table S8: Transport allowance component -------------------------------
survey_tables[["S8. Transport allowance"]] <- make_summary(
  sv,
  vars = c("transport_disbursed","transport_freq","transport_reduce","transport_punct"),
  by_var = "facility_category",
  labels = list(
    transport_disbursed ~ "Transport allowance disbursed (Q34)",
    transport_freq ~ "Frequency of disbursement (Q35)",
    transport_reduce ~ "Reduced absenteeism (Q37)",
    transport_punct ~ "Improved punctuality (Q38)"
  )
)

# ---- Table S9: Peer support component --------------------------------------
survey_tables[["S9. Peer support network"]] <- make_summary(
  sv,
  vars = c("peer_established","peer_freq","peer_teamwork","peer_reduce"),
  by_var = "facility_category",
  labels = list(
    peer_established ~ "Peer support network established (Q40)",
    peer_freq ~ "Frequency of peer meetings (Q41)",
    peer_teamwork ~ "Improved teamwork (Q43)",
    peer_reduce ~ "Reduced absenteeism (Q44)"
  )
)

# ---- Table S10: CMSC component ---------------------------------------------
survey_tables[["S10. CMSC monitoring"]] <- make_summary(
  sv,
  vars = c("cmsc_established","cmsc_monitor","cmsc_freq","cmsc_reduce"),
  by_var = "facility_category",
  labels = list(
    cmsc_established ~ "CMSC established/strengthened (Q46)",
    cmsc_monitor ~ "CMSC actively monitors attendance (Q47)",
    cmsc_freq ~ "Frequency CMSC engages facility (Q48)",
    cmsc_reduce ~ "CMSC reduced absenteeism (Q49)"
  )
)

# ---- Table S11: Sanction authority & consistency ---------------------------
survey_tables[["S11. Sanction authority and consistency"]] <- make_summary(
  sv,
  vars = c("authority_sanction","formal_authority","sanction_consistency",
           "cmsc_influence","peer_importance"),
  by_var = "facility_category",
  labels = list(
    authority_sanction ~ "Who has authority to sanction (Q51)",
    formal_authority ~ "Facility has formal authority (Q52)",
    sanction_consistency ~ "Consistency sanctions applied (Q53)",
    cmsc_influence ~ "Influence of CMSC on attendance (Q54)",
    peer_importance ~ "Importance of peer support (Q55)"
  )
)

# ---- Table S12: Systems & capacity -----------------------------------------
survey_tables[["S12. Systems and sustainability capacity"]] <- make_summary(
  sv,
  vars = c("sufficient_staff","capacity_sustain"),
  by_var = "facility_category",
  labels = list(
    sufficient_staff ~ "Sufficient staff for coverage (Q60)",
    capacity_sustain ~ "Capacity to sustain after Feb 2026 (Q61)"
  )
)

# ---- Table S13: PCI perceptions --------------------------------------------
survey_tables[["S13. PCI perceptions"]] <- make_summary(
  sv,
  vars = c("pci_importance_before","pci_commit_before","pci_lead_commit",
           "pci_comm_support","pci_lead_interest"),
  by_var = "facility_category",
  labels = list(
    pci_importance_before ~ "Importance of reducing absenteeism, pre (Q62)",
    pci_commit_before ~ "Staff commitment to reducing absenteeism, pre (Q63)",
    pci_lead_commit ~ "Leadership commitment to enforcement (Q64)",
    pci_comm_support ~ "Community supportiveness (Q65)",
    pci_lead_interest ~ "Leadership interest in continuing (Q66)"
  )
)

# ---- Table S14: RE-AIM (Reach, Effectiveness, Adoption) --------------------
survey_tables[["S14. RE-AIM \u2013 Reach, Effectiveness, Adoption"]] <- make_summary(
  sv,
  vars = c("reaim_reach","reaim_abs_reduced","reaim_punct","reaim_leave_reduced",
           "reaim_unintended","reaim_service","reaim_commit",
           "reaim_lead_approve","reaim_staff_willing"),
  by_var = "facility_category",
  labels = list(
    reaim_reach ~ "Proportion of staff benefited (Q67)",
    reaim_abs_reduced ~ "Extent absenteeism reduced (Q68)",
    reaim_punct ~ "Punctuality improved (Q69)",
    reaim_leave_reduced ~ "Leaving early reduced (Q70)",
    reaim_unintended ~ "Unintended negative consequences (Q71)",
    reaim_service ~ "Service delivery improved (Q72)",
    reaim_commit ~ "Staff commitment improved (Q73)",
    reaim_lead_approve ~ "Leadership approved interventions (Q74)",
    reaim_staff_willing ~ "Staff willingness at start (Q75)"
  )
)

# ---- Table S15: RE-AIM (Implementation, Maintenance) -----------------------
survey_tables[["S15. RE-AIM \u2013 Implementation, Maintenance"]] <- make_summary(
  sv,
  vars = c("reaim_transport_sched","reaim_peer_planned","reaim_cmsc_planned",
           "reaim_adherence","reaim_ongoing","reaim_integrated","reaim_likely_12mo"),
  by_var = "facility_category",
  labels = list(
    reaim_transport_sched ~ "Transport disbursed on schedule (Q76)",
    reaim_peer_planned ~ "Peer meetings as planned (Q77)",
    reaim_cmsc_planned ~ "CMSC visits as scheduled (Q78)",
    reaim_adherence ~ "Overall adherence to design (Q79)",
    reaim_ongoing ~ "Interventions still ongoing (Q80)",
    reaim_integrated ~ "Integrated into routine ops (Q81)",
    reaim_likely_12mo ~ "Likely active in 12 months (Q82)"
  )
)

# =============================================================================
# LOAD + PREP: EXTRACTION
# =============================================================================
ext <- read_excel(EXTRACTION_FILE, sheet = EXTRACTION_SHEET, guess_max = 5000)

ext <- ext |>
  mutate(
    lga = `Select Local Government Area`,
    phc = `Select Health Facility`,
    month_raw = `Select month for data extraction`,
    month_date = lubridate::my(str_squish(month_raw)),
    period = case_when(
      is.na(month_date) ~ NA_character_,
      month_date < as.Date("2025-08-01") ~ "Pre-intervention",
      TRUE ~ "Intervention"),
    period = factor(period, levels = c("Pre-intervention","Intervention")),
    patients = as.numeric(`Total number of patients seen this month (ANC + Immunization registers)`),
    cadre = harmonise_cadre(`Designation / Cadre`),
    days_sched = as.numeric(`Number of scheduled duty days`),
    days_present = as.numeric(`Number of days present`),
    days_late = as.numeric(`Days late recorded in register`),
    minutes_late = as.numeric(`Total minutes late`),
    avg_minutes_late = as.numeric(`Average minutes late`),
    days_absent = pmax(0, days_sched - days_present),
    absent_rate = ifelse(days_sched > 0, 100 * days_absent / days_sched, NA_real_),
    attend_rate = ifelse(days_sched > 0, 100 * days_present / days_sched, NA_real_)
  )

ext_tables <- list()

# ---- Table E1: Coverage ----------------------------------------------------
ext_tables[["E1. Extraction coverage"]] <- make_summary(
  ext,
  vars = c("lga","cadre"),
  by_var = "period",
  labels = list(
    lga ~ "Local Government Area",
    cadre ~ "Designation / Cadre"
  )
)

# ---- Table E2: Attendance metrics (continuous, with CI on key outcomes) ----
ext_tables[["E2. Attendance metrics"]] <- make_summary(
  ext,
  vars = c("days_sched","days_present","days_absent","absent_rate","attend_rate",
           "days_late","minutes_late","avg_minutes_late"),
  by_var = "period",
  ci_vars = c("absent_rate","attend_rate","minutes_late"),
  labels = list(
    days_sched ~ "Number of scheduled duty days",
    days_present ~ "Number of days present",
    days_absent ~ "Number of days absent (scheduled \u2013 present)",
    absent_rate ~ "Absenteeism rate (%)",
    attend_rate ~ "Attendance rate (%)",
    days_late ~ "Days late recorded in register",
    minutes_late ~ "Total minutes late",
    avg_minutes_late ~ "Average minutes late per incident"
  )
)

# ---- Table E3: Service volume (continuous, with CI) ------------------------
ext_tables[["E3. Service volume"]] <- make_summary(
  ext,
  vars = c("patients"),
  by_var = "period",
  ci_vars = c("patients"),
  labels = list(
    patients ~ "Total patients seen per month (ANC + Immunisation)"
  )
)

# =============================================================================
# FIGURES (continuous variables only)
# =============================================================================
save_fig <- function(p, name, w = 7, h = 4.2) {
  path <- file.path("outputs/fig", name)
  ggsave(path, p, width = w, height = h, dpi = 150)
  path
}

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold", size = 12)))

fig_paths <- list()

# Histogram: absenteeism rate
fig_paths[["F1"]] <- save_fig(
  ext |> filter(!is.na(absent_rate)) |>
    ggplot(aes(absent_rate)) +
    geom_histogram(binwidth = 5, fill = "#4575b4", colour = "white") +
    labs(title = "Distribution of worker-month absenteeism rate",
         x = "Absenteeism rate (%)", y = "Count"),
  "F1_hist_absrate.png")

# Histogram: patient volume
fig_paths[["F2"]] <- save_fig(
  ext |> filter(!is.na(patients)) |>
    ggplot(aes(patients)) +
    geom_histogram(bins = 30, fill = "#4575b4", colour = "white") +
    labs(title = "Distribution of monthly patient volume",
         x = "Patients per facility-month", y = "Count"),
  "F2_hist_patients.png")

# Boxplot: absenteeism rate by period
fig_paths[["F3"]] <- save_fig(
  ext |> filter(!is.na(absent_rate), !is.na(period)) |>
    ggplot(aes(period, absent_rate, fill = period)) +
    geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
    scale_fill_manual(values = c("#fc8d59","#91bfdb"), guide = "none") +
    labs(title = "Absenteeism rate by period",
         x = NULL, y = "Absenteeism rate (%)"),
  "F3_box_absrate_period.png")

# Boxplot: minutes late by period
fig_paths[["F4"]] <- save_fig(
  ext |> filter(!is.na(minutes_late), !is.na(period)) |>
    ggplot(aes(period, minutes_late, fill = period)) +
    geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
    scale_fill_manual(values = c("#fc8d59","#91bfdb"), guide = "none") +
    labs(title = "Total minutes late by period",
         x = NULL, y = "Total minutes late (per worker-month)"),
  "F4_box_minuteslate_period.png")

# Boxplot: absenteeism rate by cadre
fig_paths[["F5"]] <- save_fig(
  ext |> filter(!is.na(absent_rate), !is.na(cadre)) |>
    mutate(cadre = fct_reorder(cadre, absent_rate, .fun = median)) |>
    ggplot(aes(cadre, absent_rate)) +
    geom_boxplot(outlier.alpha = 0.3, fill = "#91bfdb") +
    coord_flip() +
    labs(title = "Absenteeism rate by cadre",
         x = NULL, y = "Absenteeism rate (%)"),
  "F5_box_absrate_cadre.png", h = 5)

# Scatter: absenteeism rate vs patient volume
fig_paths[["F6"]] <- save_fig(
  ext |> filter(!is.na(absent_rate), !is.na(patients)) |>
    ggplot(aes(absent_rate, patients)) +
    geom_point(alpha = 0.3, colour = "#4575b4") +
    geom_smooth(method = "lm", se = TRUE, colour = "#d73027") +
    labs(title = "Patient volume vs absenteeism rate",
         x = "Absenteeism rate (%)", y = "Patients per facility-month"),
  "F6_scatter_abs_patients.png")

# Scatter: days present vs minutes late
fig_paths[["F7"]] <- save_fig(
  ext |> filter(!is.na(days_present), !is.na(minutes_late)) |>
    ggplot(aes(days_present, minutes_late)) +
    geom_point(alpha = 0.3, colour = "#4575b4") +
    geom_smooth(method = "lm", se = TRUE, colour = "#d73027") +
    labs(title = "Total minutes late vs days present",
         x = "Days present", y = "Total minutes late"),
  "F7_scatter_present_late.png")

# =============================================================================
# ASSEMBLE WORD DOCUMENT
# =============================================================================
doc <- officer::read_docx()

# Title
doc <- doc |>
  officer::body_add_par("ACE-II Evaluation", style = "heading 1") |>
  officer::body_add_par("Descriptive Summary Statistics", style = "heading 2") |>
  officer::body_add_par(
    paste0("Provider survey (n = ", nrow(sv),
           " health workers) and attendance extraction (n = ", nrow(ext),
           " worker-month records). Survey tables are stratified by facility ",
           "category; extraction tables by study period. Categorical variables ",
           "are summarised as n (%); continuous variables as mean (SD) and ",
           "median [Q1, Q3]. 95% confidence intervals are shown for the key ",
           "continuous outcomes."),
    style = "Normal") |>
  officer::body_add_par("", style = "Normal")

# Section 1: survey
doc <- doc |>
  officer::body_add_par("Part 1. Provider survey", style = "heading 1")

add_tbl <- function(doc, tbl, caption) {
  ft <- as_styled_flex(tbl, caption)
  doc |>
    officer::body_add_par(caption, style = "heading 3") |>
    flextable::body_add_flextable(ft) |>
    officer::body_add_par("", style = "Normal")
}

for (nm in names(survey_tables)) {
  doc <- add_tbl(doc, survey_tables[[nm]], nm)
}

# Section 2: extraction
doc <- doc |>
  officer::body_add_par("Part 2. Attendance extraction", style = "heading 1")
for (nm in names(ext_tables)) {
  doc <- add_tbl(doc, ext_tables[[nm]], nm)
}

# Section 3: figures
doc <- doc |>
  officer::body_add_par("Part 3. Distributions and relationships (figures)",
                        style = "heading 1")
fig_caps <- c(
  F1 = "Figure 1. Distribution of worker-month absenteeism rate",
  F2 = "Figure 2. Distribution of monthly patient volume",
  F3 = "Figure 3. Absenteeism rate by period",
  F4 = "Figure 4. Total minutes late by period",
  F5 = "Figure 5. Absenteeism rate by cadre",
  F6 = "Figure 6. Patient volume vs absenteeism rate",
  F7 = "Figure 7. Total minutes late vs days present"
)
for (key in names(fig_paths)) {
  doc <- doc |>
    officer::body_add_par(fig_caps[[key]], style = "heading 3") |>
    officer::body_add_img(src = fig_paths[[key]], width = 6.5, height = 6.5 * 4.2/7) |>
    officer::body_add_par("", style = "Normal")
}

print(doc, target = OUT_DOCX)
cat("\nWrote", OUT_DOCX, "\n")