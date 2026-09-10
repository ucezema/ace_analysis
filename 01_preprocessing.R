# =============================================================================
# ACE-II Evaluation
# Script 01: Preprocessing & composite-variable derivation
#
# Inputs : ACE2_Provider_Survey_Manual_Rebuild_data.xlsx
#          ace2_extraction.xlsx
# Outputs: survey   - one row per respondent  (n = 93)
#          ext      - one row per worker-month (n = 752)
#          fac_mo   - one row per facility-month  (for service-delivery + DiD)
#
# Run AFTER 00_setup.R.  These three tibbles are kept in memory and used by
# every objective script.  Re-source this file any time you change a
# derivation rule.
# =============================================================================

stopifnot(exists("SURVEY_FILE"), exists("EXTRACTION_FILE"))

# =============================================================================
# 1. Load raw files
# =============================================================================
raw_survey <- readxl::read_excel(
  SURVEY_FILE,
  sheet = "ace1",  # the only sheet with data
  guess_max = 5000
)
# In case the sheet name was truncated when downloaded, fall back:
if (nrow(raw_survey) < 50) {
  sheets <- readxl::excel_sheets(SURVEY_FILE)
  data_sheet <- sheets[which.max(sapply(sheets, function(s)
    nrow(readxl::read_excel(SURVEY_FILE, sheet = s, n_max = 1))))]
  raw_survey <- readxl::read_excel(SURVEY_FILE, sheet = data_sheet, guess_max = 5000)
}

raw_ext <- readxl::read_excel(EXTRACTION_FILE, sheet = "Merged Data",
                              guess_max = 5000)

message("Survey rows: ", nrow(raw_survey), "   cols: ", ncol(raw_survey))
message("Extraction rows: ", nrow(raw_ext), "   cols: ", ncol(raw_ext))

# =============================================================================
# 2. Survey - select & rename to short, analyst-friendly names
#    The original ODK headers are very long and contain symbols; we copy by
#    EXACT column name (more robust than position).
# =============================================================================
sv_lookup <- tibble::tribble(
  ~new,                  ~old,
  "facility_category",   "Is this facility an intervention or control site?",
  "lga",                 "Select LGA",
  "phc",                 "1. Select PHC",
  "respondent_id",       "respondent_id",
  "gender",              "3. Gender of health worker",
  "location",            "4. Location of facility",
  "cadre_raw",           "7. Cadre of respondent",
  "cadre_other",         "cadre_other",
  "n_children_u12",      "8. Number of children under 12 years under your care",
  "n_adult_dep",         "9. Number of adult dependents under your care",
  "q10_walk",            "10. Does the health worker live within walking distance of the facility?",
  "q11_register",        "11. Does the facility keep an attendance register?",
  "q12_oic_check",       "12. How often does the OIC check and cross\u2011sign the attendance register?",
  "q13_register_verified","13. Enumerator verified the attendance register exists?",
  "q17_vda",             "17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Village Development Association",
  "q17_townunion",       "17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Town Union",
  "q17_tradlead",        "17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Local Traditional Leadership",
  "q17_wdc",             "17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/WDC",
  "q17_hfc",             "17. Local structures connected to the facility (VDA, town union, traditional leadership, WDC, HFC etc.)/Health Facility Committee",
  "q17b_wdc_cosig",      "17b. Does the WDC chairman act as co\u2011signatory to facility account?",
  "q18a_late",           "18. Types of absenteeism observed in the last 6 months/Coming late to work",
  "q18b_leave_early",    "18. Types of absenteeism observed in the last 6 months/Leaving before shift ends",
  "q18c_full_day",       "18. Types of absenteeism observed in the last 6 months/Not coming to work for a whole day",
  "q19_most_common",     "19. Which absenteeism type is most common?",
  "q23a_sanc_late",      "23. Are there sanctions for any of all the following types of absenteeism?/Coming late to work",
  "q23b_sanc_leave",     "23. Are there sanctions for any of all the following types of absenteeism?/Leaving before shift ends",
  "q23c_sanc_fullday",   "23. Are there sanctions for any of all the following types of absenteeism?/Not coming to work for a whole day",
  "q25_sanc_used",       "25. Have sanctions been used in the past 6 months?",
  "q25b_used_late",      "25b. What type of absenteeism has sanction been used for in the last 6 months?/Coming late to work",
  "q25b_used_leave",     "25b. What type of absenteeism has sanction been used for in the last 6 months?/Leaving before shift ends",
  "q25b_used_fullday",   "25b. What type of absenteeism has sanction been used for in the last 6 months?/Not coming to work for a whole day",
  "q26_inc_money",       "26. Incentives for punctual staff/Money",
  "q26_inc_food",        "26. Incentives for punctual staff/Food",
  "q26_inc_promo",       "26. Incentives for punctual staff/Promotion",
  "q26_inc_recog",       "26. Incentives for punctual staff/Recognition",
  "q26_inc_notsure",     "26. Incentives for punctual staff/Not sure",
  "q28_inc_freq",        "28. How often do incentives come?",
  "q29_wdc_monitor",     "29. Are health workers monitored by WDC/HFC for absenteeism?",
  "q30_obs_monitor",     "30. 30.\tAny direct observation of local monitoring with respect to health facility absenteeism?",
  "q31_comm_support",    "31. Does the community provide support to the facility?",
  "q34_transport_disb",  "34. Was a transport allowance disbursed in the past 6 months?",
  "q35_transport_freq",  "35. Frequency of transport allowance disbursement",
  "q37_transport_redabs","37. Did transport allowance reduce absenteeism?",
  "q38_transport_punct", "38. Did transport allowance improve punctuality?",
  "q40_peer_estab",      "40. Was a peer support network established?",
  "q41_peer_freq",       "41. Frequency of peer support meetings",
  "q43_peer_teamwork",   "43. Did the peer network improve teamwork?",
  "q44_peer_redabs",     "44. Did the peer network reduce absenteeism?",
  "q46_cmsc_estab",      "46. Was a CMSC established or strengthened?",
  "q47_cmsc_monitor",    "47. Does CMSC actively monitor staff attendance?",
  "q49_cmsc_redabs",     "49. Did CMSC monitoring reduce absenteeism?",
  "q51_authority",       "51. Who has the authority to sanction absenteeism?",
  "q52_formal_auth",     "52. Does the facility have formal authority to sanction absenteeism?",
  "q53_sanc_consist",    "53. During the intervention period (August 2025 \u2013February 2026), how consistently were sanctions applied when absenteeism occurred?",
  "q54_cmsc_influence",  "54. Influence of CMSC in promoting attendance",
  "q55_peer_influence",  "55. Importance of peer support network in preventing absenteeism",
  "q56_pre_register",    "56. Did facility maintain attendance register before July 2025?",
  "q57_super_freq",      "57. During the period of the intervention, how frequently were attendance records reviewed by any supervising agency/body from either the SPHCDA or the LGA?",
  "q58_doc_system",      "58. System for documenting absenteeism exists",
  "q59_records_used",    "59. Attendance records used for management decisions",
  "q60_staff_adequate",  "60. During the period of the interventiondid the facility have sufficient staff to ensure service coverage even when some staff were absent?",
  "q61_sustain_cap",     "61. \tAfter February 2026, does the facility have the capacity to sustain these interventions without external support?",
  "q62_imp_reduce",      "62. Before August  2025, how important was reducing staff absenteeism to the functioning of this facility?",
  "q63_staff_commit",    "63. Before August 2025, how committed were staff to reducing absenteeism?",
  "q64_lead_commit",     "64. During August 2025\u2013February 2026, how committed was facility leadership to enforcing attendance policies?",
  "q65_comm_support_pci","65.\tDuring August 2025\u2013February 2026, how supportive was the community in absenteeism reduction measures?",
  "q66_lead_interest",   "66.\tAfter February 2026, how interested is facility leadership in continuing absenteeism reduction measures?",
  "q67_proportion",      "67.\tDuring period of the intervention, what proportion of eligible staff benefited from the intervention?",
  "q68_abs_reduced",     "68. As of February 2026, to what extent has absenteeism reduced in this facility?",
  "q69_punct_improved",  "69. As of February 2026, had staff punctuality improved?",
  "q70_leave_reduced",   "70. As of February 2026, had staff leaving before close of work or shift reduced?",
  "q71_unintended",      "71. During August 2025 to February 2026, were there any unintended negative consequences?",
  "q72_service",         "72. Did reduced absenteeism improve service delivery in the facility?",
  "q73_commit_improved", "73. Did overall staff commitment to work improve?",
  "q74_lead_approve",    "74. Did facility leadership approve the interventions?",
  "q75_staff_willing",   "75. Staff willingness to participate at start",
  "q76_transport_sched", "76. During August 2025 to February 2026, were transport allowances disbursed according to the planned schedule?",
  "q77_peer_planned",    "77. During August 2025 to February 2026, were peer support meetings conducted as originally planned?",
  "q78_cmsc_planned",    "78. During August 2025 to February 2026, did the CMSC conduct monitoring visits as scheduled?",
  "q79_adherence",       "79. Overall, how closely did the facility adhere to the original intervention design between August 2025 to February 2026?",
  "q80_ongoing",         "80.\tAfter February 2026, are any of the absenteeism-reduction interventions still ongoing?",
  "q81_integrated",      "81. Have any of the interventions been integrated into routine facility operations after February 2026?",
  "q82_likely_12mo",     "82. How likely is it that these interventions will still be active 12 months after February 2026?"
)

# Fall back gracefully if a column header changed slightly
missing_in_raw <- setdiff(sv_lookup$old, names(raw_survey))
if (length(missing_in_raw)) {
  warning("These expected columns are missing from the survey file:\n",
          paste("  ", missing_in_raw, collapse = "\n"))
}

survey <- raw_survey |>
  dplyr::select(dplyr::any_of(setNames(sv_lookup$old, sv_lookup$new)))

# =============================================================================
# 3. Survey - type cleaning & factor labels
# =============================================================================

# Logical 0/1 multi-select items - keep numeric 0/1 (not factor) so they sum
multi01 <- c("q17_vda","q17_townunion","q17_tradlead","q17_wdc","q17_hfc",
             "q18a_late","q18b_leave_early","q18c_full_day",
             "q23a_sanc_late","q23b_sanc_leave","q23c_sanc_fullday",
             "q25b_used_late","q25b_used_leave","q25b_used_fullday",
             "q26_inc_money","q26_inc_food","q26_inc_promo","q26_inc_recog",
             "q26_inc_notsure")

survey <- survey |>
  dplyr::mutate(dplyr::across(dplyr::any_of(multi01),
                              ~ as.numeric(as.character(.x))))

# Numeric continuous
survey <- survey |>
  dplyr::mutate(dplyr::across(c(n_children_u12, n_adult_dep), ~ as.numeric(.x)))

# Facility category - cleaner factor with intervention as the test arm but
# Control as the reference (standard for trial-style analyses)
survey <- survey |>
  dplyr::mutate(
    facility_category = dplyr::case_when(
      stringr::str_detect(tolower(facility_category), "intervention") ~ "Intervention",
      stringr::str_detect(tolower(facility_category), "control")      ~ "Control",
      TRUE ~ NA_character_),
    facility_category = factor(facility_category,
                               levels = c("Control", "Intervention"))
  )

# Gender
survey <- survey |>
  dplyr::mutate(gender = factor(gender, levels = c("Female", "Male")))

# Location - drop trailing whitespace and reorder
# We name it `fac_location` rather than `location` because flextable exports
# a function called location() that shadows columns named `location` inside
# lm()/glm()/polr() formulas. Calling the column fac_location avoids the
# clash everywhere downstream.
survey <- survey |>
  dplyr::mutate(fac_location = stringr::str_trim(location),
                fac_location = factor(fac_location,
                                      levels = c("Rural", "Semi-urban", "Urban"))) |>
  dplyr::select(-location)

# Cadre harmonisation -- the raw column has 50+ ODK free-text variants in the
# *extraction* file, but the *survey* file is mostly clean already.
# Order matters: more specific patterns must come BEFORE more general ones
# (e.g. JCHEW before CHEW, MIDWIFE before AD-HOC/MIDWIFE handling).
harmonise_cadre <- function(x) {
  s <- toupper(stringr::str_trim(as.character(x)))
  dplyr::case_when(
    s == "" | is.na(s) ~ NA_character_,
    stringr::str_detect(s, "PHYSIC|DOCTOR|MEDICAL OFFICER")                  ~ "Physician",
    stringr::str_detect(s, "MIDWIFE|NURSE")                                   ~ "Nurse/Midwife",
    stringr::str_detect(s, "JCHEW|J\\.?CHEW|JUNIOR CHEW|JCHEw|JCH ")          ~ "JCHEW",
    stringr::str_detect(s, "AD-?HOC|ADHOC")                                   ~ "Ad-hoc",
    stringr::str_detect(s, "VOLUNT")                                          ~ "Volunteer",
    stringr::str_detect(s, "CHEW|CHO\\b|SCHEW|PCHEW|HCHEW|DDCHEW|CCHEW|ACCHEW|DDCHT|CCO\\b") ~ "CHEW",
    stringr::str_detect(s, "\\bH[\\./]?A\\b|HEALTH ATTENDANT|^SHA$|^PHA$|HELPING STAFF") ~ "Health Attendant",
    stringr::str_detect(s, "LAB|SMLT|PHARM|MLT|TECH")                         ~ "Lab/Tech",
    stringr::str_detect(s, "ADMIN|MESSENGER|FSO|COMMUNITY HEALTH EDUC|^DDNS$|^DDSN$|^ASNS$|P-PTECH") ~ "Admin/Other support",
    TRUE                                                                      ~ "Other"
  )
}
survey <- survey |>
  dplyr::mutate(
    cadre = harmonise_cadre(cadre_raw),
    cadre = factor(cadre,
                   levels = c("CHEW","Nurse/Midwife","JCHEW","Physician",
                              "Volunteer","Ad-hoc","Health Attendant",
                              "Lab/Tech","Admin/Other support","Other"))
  )

# Walking distance (Q10): Yes/No -> binary
survey <- survey |>
  dplyr::mutate(q10_walk_bin = dplyr::if_else(q10_walk == "Yes", 1L, 0L))

# Q11 register exists (we keep "Don't know" as separate level for description)
survey <- survey |>
  dplyr::mutate(q11_register = factor(q11_register, levels = c("Yes","No")))

# Q12 OIC check frequency - ordered (high-frequency = high score)
q12_levels <- c("Never","Once in a while","Monthly","Once a week",
                "Not every day but several times each week","Daily")
survey <- survey |>
  dplyr::mutate(
    q12_oic_check_clean = dplyr::case_when(
      q12_oic_check %in% q12_levels ~ q12_oic_check,
      TRUE ~ NA_character_),
    q12_oic_check = factor(q12_oic_check_clean, levels = q12_levels,
                           ordered = TRUE),
    q12_oic_score = as.integer(q12_oic_check)   # 1..6
  ) |>
  dplyr::select(-q12_oic_check_clean)

# Q19 most common type (note: in this dataset 92/93 = "Coming late" so this
# is essentially constant -- kept for completeness, downstream tests will
# warn the user if there are <2 categories)
survey <- survey |>
  dplyr::mutate(q19_most_common = factor(q19_most_common))

# Q25 sanctions used (Yes/No/Not sure)
survey <- survey |>
  dplyr::mutate(q25_sanc_used = factor(q25_sanc_used,
                                       levels = c("No","Yes","Not sure")))

# Q29-31 (treat "Not sure" as missing for the index; full distribution preserved)
yes_no_to01 <- function(x) {
  s <- tolower(as.character(x))
  dplyr::case_when(
    s == "yes" ~ 1L,
    s == "no"  ~ 0L,
    TRUE       ~ NA_integer_)
}
# Q31 has free-text; recode any non-"No" string as 1 (community provides support)
q31_to01 <- function(x) {
  s <- tolower(stringr::str_trim(as.character(x)))
  dplyr::case_when(
    is.na(s) | s == ""        ~ NA_integer_,
    s == "no"                 ~ 0L,
    TRUE                      ~ 1L
  )
}
survey <- survey |>
  dplyr::mutate(
    q29_wdc_monitor_01 = yes_no_to01(q29_wdc_monitor),
    q30_obs_monitor_01 = yes_no_to01(q30_obs_monitor),
    q31_comm_support_01 = q31_to01(q31_comm_support)
  )

# Intervention components (Q34, Q40, Q43, Q44, Q46, Q47, Q49) -> 0/1 with
# control respondents (NA from skip logic) coded as 0 (= "not exposed").
# This matches the DAP definition "1=Yes, 0=No for each".
intv_yes_to01 <- function(x) {
  s <- tolower(as.character(x))
  dplyr::case_when(
    s == "yes" ~ 1L,
    is.na(s)   ~ 0L,    # skip-logic NA = control = not exposed
    TRUE       ~ 0L)    # any other answer (e.g. "No"/"Not sure") = 0
}
survey <- survey |>
  dplyr::mutate(
    q34_transport_01 = intv_yes_to01(q34_transport_disb),
    q40_peer_01      = intv_yes_to01(q40_peer_estab),
    q43_peer_team_01 = intv_yes_to01(q43_peer_teamwork),
    q44_peer_red_01  = intv_yes_to01(q44_peer_redabs),
    q46_cmsc_01      = intv_yes_to01(q46_cmsc_estab),
    q47_cmsc_mon_01  = intv_yes_to01(q47_cmsc_monitor),
    q49_cmsc_red_01  = intv_yes_to01(q49_cmsc_redabs)
  )

# =============================================================================
# 4. PCI ordinal scoring (intervention-site rows only - rest stay NA)
#    Each item scored 1..k from least to most.  If a value isn't on the
#    expected list it becomes NA so we never silently mis-score.
# =============================================================================
     # score_levels <- function(x, levs) {
      #  f <- factor(x, levels = levs, ordered = TRUE)
       # as.integer(f)
      #}

score_levels <- function(x, levs) {
  f <- factor(x, levels = levs, ordered = TRUE)
  as.integer(f) - 1L
}

# PCI Power -------------------------------------------------------------------
# Q51: who has authority -> not really ordinal; recode "any formal authority" = 1, else 0
survey <- survey |>
  dplyr::mutate(
    q51_score = dplyr::case_when(
      q51_authority %in% c("Officer-in-Charge","LGA Health Authority",
                           "SMOH") ~ 5L,
      q51_authority == "other"     ~ 3L,
      is.na(q51_authority)          ~ NA_integer_,
      TRUE                          ~ 1L)
  )

# Q52 formal authority Yes/No/Not sure -> 5/1/3
survey <- survey |>
  dplyr::mutate(
    q52_score = dplyr::case_when(
      q52_formal_auth == "Yes"      ~ 5L,
      q52_formal_auth == "Not sure" ~ 3L,
      q52_formal_auth == "No"       ~ 1L,
      TRUE                          ~ NA_integer_)
  )

# Q53 consistency
q53_levs <- c("Not active","Not applied","Occasionally","Monthly","Weekly")
survey <- survey |>
  dplyr::mutate(
    q53_clean = dplyr::if_else(q53_sanc_consist %in% q53_levs,
                               q53_sanc_consist, NA_character_),
    q53_score = score_levels(q53_clean, q53_levs)
  ) |>
  dplyr::select(-q53_clean)

# Q54 CMSC influence
q54_levs <- c("Not influential","Slightly influential","Moderately influential",
              "Highly influential")
survey <- survey |>
  dplyr::mutate(
    q54_clean = dplyr::if_else(q54_cmsc_influence %in% q54_levs,
                               q54_cmsc_influence, NA_character_),
    q54_score = score_levels(q54_clean, q54_levs)
  ) |>
  dplyr::select(-q54_clean)

# Q55 peer influence (same scale)
survey <- survey |>
  dplyr::mutate(
    q55_clean = dplyr::if_else(q55_peer_influence %in% q54_levs,
                               q55_peer_influence, NA_character_),
    q55_score = score_levels(q55_clean, q54_levs)
  ) |>
  dplyr::select(-q55_clean)

# PCI Capabilities ------------------------------------------------------------
# Q56 Yes/No/Not sure -> 2/0/1 (3-pt)
survey <- survey |>
  dplyr::mutate(
    q56_score = dplyr::case_when(
      q56_pre_register == "Yes"      ~ 2L,
      q56_pre_register == "Not sure" ~ 1L,
      q56_pre_register == "No"       ~ 0L,
      TRUE                            ~ NA_integer_)
  )

# Q57 supervision frequency: Occasionally / Monthly / Weekly  (1..3)
q57_levs <- c("Occasionally","Monthly","Weekly")
survey <- survey |>
  dplyr::mutate(
    q57_clean = dplyr::if_else(q57_super_freq %in% q57_levs,
                               q57_super_freq, NA_character_),
    q57_score = score_levels(q57_clean, q57_levs)
  ) |>
  dplyr::select(-q57_clean)

# Q58 system documenting (Yes/Not sure/No -> 2/1/0)
survey <- survey |>
  dplyr::mutate(
    q58_score = dplyr::case_when(
      q58_doc_system == "Yes"      ~ 2L,
      q58_doc_system == "Not sure" ~ 1L,
      q58_doc_system == "No"       ~ 0L,
      TRUE                          ~ NA_integer_)
  )

# Q59 records used decisions (Yes/Not sure/No -> 2/1/0)
survey <- survey |>
  dplyr::mutate(
    q59_score = dplyr::case_when(
      q59_records_used == "Yes"      ~ 2L,
      q59_records_used == "Not sure" ~ 1L,
      q59_records_used == "No"       ~ 0L,
      TRUE                            ~ NA_integer_)
  )

# Q60 staffing adequacy (Yes/Not sure/No -> 2/1/0)
survey <- survey |>
  dplyr::mutate(
    q60_score = dplyr::case_when(
      q60_staff_adequate == "Yes"      ~ 2L,
      q60_staff_adequate == "Not sure" ~ 1L,
      q60_staff_adequate == "No"       ~ 0L,
      TRUE                              ~ NA_integer_)
  )

# Q61 sustainability capacity
q61_levs <- c("Not capable","Not sure","Partially capable","Fully capable")
survey <- survey |>
  dplyr::mutate(
    q61_clean = dplyr::if_else(q61_sustain_cap %in% q61_levs,
                               q61_sustain_cap, NA_character_),
    q61_score = score_levels(q61_clean, q61_levs)
  ) |>
  dplyr::select(-q61_clean)

# PCI Interest ----------------------------------------------------------------
imp_levs <- c("Not important","Not sure","Slightly important",
              "Moderately important","Very important")
commit_levs <- c("Not committed","Not sure","Slightly committed",
                 "Moderately committed","Very committed")
support_levs <- c("Not supportive","Not sure","Slightly supportive",
                  "Moderately supportive","Very supportive")
interest_levs <- c("Not interested","Not sure","Slightly interested",
                   "Moderately interested","Very interested")

score_or_na <- function(x, levs) {
  x <- ifelse(x %in% levs, x, NA_character_)
  score_levels(x, levs)
}
survey <- survey |>
  dplyr::mutate(
    q62_score = score_or_na(q62_imp_reduce,        imp_levs),
    q63_score = score_or_na(q63_staff_commit,      commit_levs),
    q64_score = score_or_na(q64_lead_commit,       commit_levs),
    q65_score = score_or_na(q65_comm_support_pci,  support_levs),
    q66_score = score_or_na(q66_lead_interest,     interest_levs)
  )

# =============================================================================
# 5. RE-AIM scoring -----------------------------------------------------------
# =============================================================================
# Reach: Q67 ordinal proportion benefited
q67_levs <- c("0","<25%","25-49%","25\u201349%","50-74%","50\u201374%",
              "75-100%","75\u2013100%")
# Use endash variants seen in data
survey <- survey |>
  dplyr::mutate(
    q67_score = dplyr::case_when(
      q67_proportion %in% c("75\u2013100%","75-100%") ~ 4L,
      q67_proportion %in% c("50\u201374%","50-74%")    ~ 3L,
      q67_proportion %in% c("25\u201349%","25-49%")    ~ 2L,
      q67_proportion %in% c("<25%","0")                ~ 1L,
      TRUE                                              ~ NA_integer_)
  )

# Effectiveness Q68-73
q68_levs <- c("No reduction","Slight reduction","Moderate reduction",
              "Significant reduction")
survey <- survey |>
  dplyr::mutate(
    q68_score = score_or_na(q68_abs_reduced, q68_levs),
    q69_score = dplyr::case_when(q69_punct_improved == "Yes" ~ 2L,
                                 q69_punct_improved == "Not sure" ~ 1L,
                                 q69_punct_improved == "No" ~ 0L,
                                 TRUE ~ NA_integer_),
    q70_score = dplyr::case_when(q70_leave_reduced == "Yes" ~ 2L,
                                 q70_leave_reduced == "Not sure" ~ 1L,
                                 q70_leave_reduced == "No" ~ 0L,
                                 TRUE ~ NA_integer_),
    # Q71 unintended -> reverse so HIGHER = better (0 = had unintended; 2 = none)
    q71_score = dplyr::case_when(q71_unintended == "No" ~ 2L,
                                 q71_unintended == "Not sure" ~ 1L,
                                 q71_unintended == "Yes" ~ 0L,
                                 TRUE ~ NA_integer_),
    q72_score = dplyr::case_when(
      q72_service == "Yes, significantly" ~ 3L,
      q72_service == "Yes, somewhat"      ~ 2L,
      q72_service == "No"                 ~ 1L,
      q72_service == "Not sure"           ~ 0L,
      TRUE ~ NA_integer_),
    q73_score = dplyr::case_when(q73_commit_improved == "Yes" ~ 2L,
                                 q73_commit_improved == "Not sure" ~ 1L,
                                 q73_commit_improved == "No" ~ 0L,
                                 TRUE ~ NA_integer_)
  )

# Adoption Q74-75
survey <- survey |>
  dplyr::mutate(
    q74_score = dplyr::case_when(q74_lead_approve == "Yes" ~ 2L,
                                 q74_lead_approve == "Not sure" ~ 1L,
                                 q74_lead_approve == "No" ~ 0L,
                                 TRUE ~ NA_integer_),
    q74_lead_approve_bin = dplyr::case_when(
      q74_lead_approve == "Yes" ~ 1L,
      q74_lead_approve %in% c("No","Not sure") ~ 0L,
      TRUE ~ NA_integer_),
    q75_score = score_or_na(q75_staff_willing,
                            c("Reluctant","Somewhat willing","Very willing"))
  )

# Implementation Q76-79
q76_levs <- c("Always delayed","Sometimes delayed","Always on schedule")
q77_levs <- c("No meetings held","Few meetings held","Most meetings held",
              "All planned meetings held")
q78_levs <- c("Not as scheduled","Partially as scheduled","Fully as scheduled")
q79_levs <- c("Did not adhere","Partially adhered","Mostly adhered","Fully adhered")
survey <- survey |>
  dplyr::mutate(
    q76_score = score_or_na(q76_transport_sched, q76_levs),
    q77_score = score_or_na(q77_peer_planned,    q77_levs),
    q78_score = score_or_na(q78_cmsc_planned,    q78_levs),
    q79_score = score_or_na(q79_adherence,       q79_levs)
  )

# Maintenance Q80-82
survey <- survey |>
  dplyr::mutate(
    q80_score = dplyr::case_when(q80_ongoing == "Yes" ~ 2L,
                                 q80_ongoing == "Not sure" ~ 1L,
                                 q80_ongoing == "No" ~ 0L,
                                 TRUE ~ NA_integer_),
    q81_score = score_or_na(q81_integrated,
                            c("Not integrated","Partially integrated","Fully integrated")),
    q82_score = score_or_na(q82_likely_12mo,
                            c("Very unlikely","Unlikely","Not sure",
                              "Somewhat likely","Very likely"))
  )

# Q82 ordinal factor (kept for ordinal logistic regression)
survey <- survey |>
  dplyr::mutate(
    q82_likely_ord = factor(q82_likely_12mo,
                            levels = c("Very unlikely","Unlikely","Not sure",
                                       "Somewhat likely","Very likely"),
                            ordered = TRUE)
  )

# =============================================================================
# 6. Composite indices --------------------------------------------------------
# =============================================================================
sum_na <- function(...) {
  v <- c(...); if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
}

survey <- survey |>
  dplyr::rowwise() |>
  dplyr::mutate(
    # Governance Strength Index: Q17 (5 binary + WDC chairman cosig + Q29 + Q30 + Q31)
    # DAP says "Sum of Q17 + Q29 + Q30 + Q31"; Q17 contributes the count of
    # structures linked (max 5)
    n_q17 = sum_na(q17_vda, q17_townunion, q17_tradlead, q17_wdc, q17_hfc),
    governance_idx = sum_na(n_q17, q29_wdc_monitor_01,
                            q30_obs_monitor_01, q31_comm_support_01),
    # Sanction Enforcement: 3 sanctions exist + 3 sanctions applied (max 6)
    sanction_idx   = sum_na(q23a_sanc_late, q23b_sanc_leave, q23c_sanc_fullday,
                            q25b_used_late, q25b_used_leave, q25b_used_fullday),
    # Incentives: 5 types + 1 frequency-yes (max 6)
    incentive_idx  = sum_na(q26_inc_money, q26_inc_food, q26_inc_promo,
                            q26_inc_recog,
                            dplyr::if_else(q28_inc_freq == "Yes", 1, 0,
                                           missing = 0)),
    # Intervention Exposure (DAP: Q34 + Q40 + Q43 + Q44 + Q46 + Q47 + Q49 = max 7)
    exposure_idx   = sum_na(q34_transport_01, q40_peer_01, q43_peer_team_01,
                            q44_peer_red_01, q46_cmsc_01, q47_cmsc_mon_01,
                            q49_cmsc_red_01),
    # PCI scores (NA-tolerant sum so partial responses still scored)
    pci_power      = sum_na(q51_score, q52_score, q53_score, q54_score, q55_score),
    pci_capabil    = sum_na(q56_score, q57_score, q58_score, q59_score,
                            q60_score, q61_score),
    pci_interest   = sum_na(q62_score, q63_score, q64_score, q65_score, q66_score),
    # RE-AIM domain scores
    reaim_reach    = q67_score,
    reaim_eff      = sum_na(q68_score, q69_score, q70_score, q71_score,
                            q72_score, q73_score),
    reaim_adopt    = sum_na(q74_score, q75_score),
    reaim_implem   = sum_na(q76_score, q77_score, q78_score, q79_score),
    reaim_maint    = sum_na(q80_score, q81_score, q82_score)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    reaim_total = reaim_reach + reaim_eff + reaim_adopt + reaim_implem + reaim_maint
  )

# Categorical exposure tier (Low/Moderate/High) based on the 0..7 score
survey <- survey |>
  dplyr::mutate(
    exposure_cat = dplyr::case_when(
      is.na(exposure_idx)   ~ NA_character_,
      exposure_idx <= 2     ~ "Low",
      exposure_idx <= 5     ~ "Moderate",
      TRUE                  ~ "High"),
    exposure_cat = factor(exposure_cat, levels = c("Low","Moderate","High"))
  )

# =============================================================================
# 7. Attendance extraction - clean column names, types, derive variables
# =============================================================================
ext <- raw_ext |>
  dplyr::transmute(
    extraction_id     = `_id`,
    submission_time   = `_submission_time`,
    lga               = `Select Local Government Area`,
    phc               = `Select Health Facility`,
    extraction_date   = `Date of data extraction`,
    enumerator        = `Enumerator name`,
    month_label       = `Select month for data extraction`,
    monthly_patients  = as.numeric(`Total number of patients seen this month (ANC + Immunization registers)`),
    worker_index      = as.integer(worker_index),
    worker_name       = `Health worker name`,
    cadre_raw         = `Designation / Cadre`,
    days_scheduled    = as.numeric(`Number of scheduled duty days`),
    days_present      = as.numeric(`Number of days present`),
    days_late_reg     = as.numeric(`Days late recorded in register`),
    total_min_late    = as.numeric(`Total minutes late`),
    avg_min_late      = as.numeric(`Average minutes late`),
    days_abs_perm     = as.numeric(`Days absent with permission`),
    days_abs_noperm   = as.numeric(`Days absent without permission`),
    days_late_noperm  = as.numeric(`Days late without permission`)
  )

# Harmonise messy cadre column
ext <- ext |>
  dplyr::mutate(
    cadre = harmonise_cadre(cadre_raw),
    cadre = factor(cadre,
                   levels = c("CHEW","Nurse/Midwife","JCHEW","Physician",
                              "Volunteer","Ad-hoc","Health Attendant",
                              "Lab/Tech","Admin/Other support","Other"))
  )

# Convert "February 2025" -> first-of-month Date
ext <- ext |>
  dplyr::mutate(
    month_date = lubridate::my(stringr::str_squish(month_label)),
    month_num  = lubridate::month(month_date),
    year       = lubridate::year(month_date)
  )

# Map facility -> intervention/control using survey crosswalk
phc_xwalk <- survey |>
  dplyr::distinct(phc, facility_category) |>
  tidyr::drop_na()

ext <- ext |>
  dplyr::left_join(phc_xwalk, by = "phc")
n_unmatched <- sum(is.na(ext$facility_category))
if (n_unmatched > 0) {
  warning(n_unmatched, " extraction rows had no facility-category match. ",
          "These PHCs are not in the survey: ",
          paste(unique(ext$phc[is.na(ext$facility_category)]), collapse = ", "))
}

# Worker-month rates
# NB: 'Days absent with/without permission' columns are EMPTY in the raw data,
# so total absence is derived as scheduled - present.  This is documented
# in the analysis report.
ext <- ext |>
  dplyr::mutate(
    days_absent_total = pmax(0, days_scheduled - days_present, na.rm = FALSE),
    absent_rate = dplyr::if_else(days_scheduled > 0,
                                 100 * days_absent_total / days_scheduled,
                                 NA_real_),
    attend_rate = dplyr::if_else(days_scheduled > 0,
                                 100 * days_present / days_scheduled,
                                 NA_real_),
    # Period flag (intervention period in DAP = Aug 2025 - Feb 2026)
    period = dplyr::case_when(
      is.na(month_date)                          ~ NA_character_,
      month_date <  as.Date("2025-08-01")        ~ "Pre-intervention",
      month_date >= as.Date("2025-08-01")        ~ "Intervention",
      TRUE                                       ~ NA_character_),
    period = factor(period, levels = c("Pre-intervention","Intervention")),
    intervention_dummy = dplyr::if_else(facility_category == "Intervention",
                                        1L, 0L),
    post_dummy = dplyr::if_else(period == "Intervention", 1L, 0L)
  )

# =============================================================================
# 8. Facility-month aggregate - for service-delivery + DiD analyses
# =============================================================================
fac_mo <- ext |>
  dplyr::filter(!is.na(facility_category), !is.na(month_date)) |>
  dplyr::group_by(phc, facility_category, lga, month_date, period) |>
  dplyr::summarise(
    n_workers           = dplyr::n(),
    monthly_patients    = dplyr::first(monthly_patients),  # already facility-level
    fac_days_scheduled  = sum(days_scheduled, na.rm = TRUE),
    fac_days_present    = sum(days_present, na.rm = TRUE),
    fac_days_absent     = sum(days_absent_total, na.rm = TRUE),
    fac_absent_rate     = dplyr::if_else(sum(days_scheduled, na.rm = TRUE) > 0,
                                         100 * sum(days_absent_total, na.rm = TRUE) /
                                           sum(days_scheduled, na.rm = TRUE),
                                         NA_real_),
    fac_attend_rate     = dplyr::if_else(sum(days_scheduled, na.rm = TRUE) > 0,
                                         100 * sum(days_present, na.rm = TRUE) /
                                           sum(days_scheduled, na.rm = TRUE),
                                         NA_real_),
    fac_total_min_late  = sum(total_min_late, na.rm = TRUE),
    fac_avg_min_late    = mean(avg_min_late, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(post_dummy = dplyr::if_else(period == "Intervention", 1L, 0L),
                intervention_dummy = dplyr::if_else(facility_category == "Intervention",
                                                    1L, 0L))

# =============================================================================
# 8b. PHC-level survey aggregate - used as covariates in regressions where we
#     join survey-respondent attributes onto worker-month attendance rows.
# =============================================================================
# Helper: majority value (mode) for any vector, returned as character.
# Used for PHC-level rollups of categorical respondent attributes where
# within-facility disagreement exists (e.g. some respondents say their PHC
# is "Urban", others say "Rural"). Conversion back to factor happens once,
# after summarise(), so all rows share the same factor levels.
mode_value <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

sv_phc <- survey |>
  dplyr::group_by(phc) |>
  dplyr::summarise(
    fac_location       = mode_value(fac_location),
    governance_idx_phc = mean(governance_idx,  na.rm = TRUE),
    sanction_idx_phc   = mean(sanction_idx,    na.rm = TRUE),
    incentive_idx_phc  = mean(incentive_idx,   na.rm = TRUE),
    q12_score_phc      = mean(q12_oic_score,   na.rm = TRUE),
    pct_walk_phc       = mean(q10_walk_bin,    na.rm = TRUE),
    pct_female_phc     = mean(gender == "Female", na.rm = TRUE),
    n_children_phc     = mean(n_children_u12,  na.rm = TRUE),
    n_adult_dep_phc    = mean(n_adult_dep,     na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(fac_location = factor(fac_location,
                                      levels = c("Rural","Semi-urban","Urban")))

# =============================================================================
# 9. Quick sanity print -------------------------------------------------------
# =============================================================================
cat("\n--- Survey: ", nrow(survey), " respondents ---\n")
print(table(survey$facility_category, useNA = "always"))
cat("\n--- Extraction: ", nrow(ext), " worker-months ---\n")
print(table(ext$facility_category, ext$period, useNA = "always"))
cat("\n--- Facility-months in fac_mo: ", nrow(fac_mo), " ---\n")
print(table(fac_mo$facility_category, fac_mo$period, useNA = "always"))

cat("\nPreprocessing complete. Available objects in workspace:\n",
    " - survey  (one row per respondent, n = ",   nrow(survey), ")\n",
    " - ext     (one row per worker-month, n = ", nrow(ext),    ")\n",
    " - fac_mo  (one row per facility-month, n = ",nrow(fac_mo), ")\n",
    " - sv_phc  (one row per PHC, n = ",          nrow(sv_phc), ")\n", sep="")