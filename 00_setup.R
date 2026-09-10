# =============================================================================
# ACE-II Evaluation | HPRG, University of Nigeria
# Script 00: Setup - packages, paths, shared helpers
# Run this ONCE per R session BEFORE any objective script.
# Author: HPRG Data Science Team
# =============================================================================

# --- 1. Packages -------------------------------------------------------------
# Install any missing packages the first time you run this.
required_pkgs <- c(
  "readxl",        # read .xlsx
  "dplyr", "tidyr", "stringr", "purrr", "forcats", "tibble", "lubridate",
  "janitor",       # clean column names
  "gtsummary",     # publication-ready descriptive & regression tables
  "flextable",     # docx-friendly tables
  "officer",       # write to .docx
  "broom",         # tidy regression output
  "broom.helpers", # for gtsummary
  "broom.mixed",   # tidy mixed-effects output
  "rstatix",       # tidy tests of association
  "car",           # ANOVA, Levene's test, vif, durbinWatsonTest
  "DescTools",     # post-hoc tests (Games-Howell), MultinomLogit etc.
  "lmtest",        # Breusch-Pagan test
  "ResourceSelection", # Hosmer-Lemeshow test
  "nnet",          # multinomial logistic
  "MASS",          # polr (proportional odds), stepAIC
  "ordinal",       # clm (ordinal regression)
  "brant",         # Brant test for proportional-odds assumption
  "lme4", "lmerTest", # mixed-effects
  "MatchIt",       # propensity scores
  "ggplot2", "scales", "patchwork"
)

missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

suppressPackageStartupMessages({
  invisible(lapply(required_pkgs, library, character.only = TRUE))
})

# --- 2. Project paths --------------------------------------------------------
# EDIT THESE TWO LINES TO MATCH YOUR LOCAL FOLDER STRUCTURE
PROJECT_DIR <- "C:/Users/USER/Desktop/Research/HPRG/ACE/ace_analysis"           # change to your project folder
DATA_DIR    <- file.path(PROJECT_DIR, "data")       # put both .xlsx files here
OUTPUT_DIR  <- file.path(PROJECT_DIR, "outputs")    # tables/figures land here

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

SURVEY_FILE     <- file.path(DATA_DIR, "ACE2_Provider_Survey_Manual_Rebuild_data.xlsx")
EXTRACTION_FILE <- file.path(DATA_DIR, "ace2_extraction.xlsx")

# --- 3. Global options -------------------------------------------------------
options(scipen = 999)                # avoid scientific notation
gtsummary::theme_gtsummary_compact() # tighter tables
gtsummary::theme_gtsummary_journal(journal = "jama")  # JAMA-style p-values etc.

# --- 4. Helpers --------------------------------------------------------------

# Save a gtsummary or flextable object to a single .docx file.
# Pass a NAMED list of objects (the names become headings).
save_to_docx <- function(named_obj_list, file_path, title = NULL) {
  doc <- officer::read_docx()
  if (!is.null(title)) {
    doc <- doc |>
      officer::body_add_par(title, style = "heading 1") |>
      officer::body_add_par("")
  }
  for (nm in names(named_obj_list)) {
    obj <- named_obj_list[[nm]]
    if (is.null(obj)) next
    doc <- doc |>
      officer::body_add_par(nm, style = "heading 2") |>
      officer::body_add_par("")
    ft <- if (inherits(obj, "flextable")) {
      obj
    } else if (inherits(obj, "gtsummary")) {
      gtsummary::as_flex_table(obj)
    } else if (inherits(obj, "data.frame")) {
      flextable::flextable(obj) |> flextable::autofit()
    } else {
      flextable::flextable(as.data.frame(obj)) |> flextable::autofit()
    }
    doc <- doc |>
      flextable::body_add_flextable(ft, align = "left") |>
      officer::body_add_par("")
  }
  print(doc, target = file_path)
  message("Wrote: ", file_path)
  invisible(file_path)
}

# Format a p-value for display
fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f")))
}

# Generic numeric summary chooser - returns mean (SD) if normal, median [IQR] if not
describe_continuous <- function(x, normal = NULL) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_character_)
  if (is.null(normal)) {
    # Shapiro requires 3<=n<=5000
    normal <- if (length(x) <= 5000) {
      tryCatch(shapiro.test(x)$p.value > 0.05, error = function(e) FALSE)
    } else FALSE
  }
  if (normal) sprintf("%.2f (%.2f)", mean(x), sd(x))
  else        sprintf("%.2f [%.2f-%.2f]", median(x),
                       quantile(x, .25), quantile(x, .75))
}

# Decide t-test vs Mann-Whitney based on Shapiro p (per-group)
choose_2group_test <- function(y, g) {
  ok <- complete.cases(y, g)
  y <- y[ok]; g <- g[ok]
  if (length(unique(g)) != 2 || length(y) < 4) {
    return(list(test = NA, statistic = NA, p = NA, method = "insufficient data"))
  }
  yg <- split(y, g)
  shap_ps <- sapply(yg, function(v) {
    if (length(v) >= 3 && length(v) <= 5000) {
      tryCatch(shapiro.test(v)$p.value, error = function(e) 0)
    } else 0
  })
  if (all(shap_ps > 0.05)) {
    tt <- t.test(y ~ g)
    list(test = "t-test", statistic = unname(tt$statistic), p = tt$p.value,
         method = "Independent samples t-test")
  } else {
    wt <- suppressWarnings(wilcox.test(y ~ g))
    list(test = "Mann-Whitney U", statistic = unname(wt$statistic), p = wt$p.value,
         method = "Mann-Whitney U test")
  }
}

# Decide ANOVA vs Kruskal based on overall Shapiro and Levene
choose_kgroup_test <- function(y, g) {
  ok <- complete.cases(y, g)
  y <- y[ok]; g <- g[ok]
  if (length(unique(g)) < 2 || length(y) < 4) {
    return(list(test = NA, statistic = NA, p = NA, method = "insufficient data"))
  }
  yg <- split(y, g)
  shap_ps <- sapply(yg, function(v) {
    if (length(v) >= 3 && length(v) <= 5000) {
      tryCatch(shapiro.test(v)$p.value, error = function(e) 0)
    } else 0
  })
  if (all(shap_ps > 0.05)) {
    aov_fit <- aov(y ~ factor(g))
    s <- summary(aov_fit)[[1]]
    list(test = "ANOVA", statistic = s[["F value"]][1], p = s[["Pr(>F)"]][1],
         method = "One-way ANOVA")
  } else {
    kt <- kruskal.test(y ~ factor(g))
    list(test = "Kruskal-Wallis", statistic = unname(kt$statistic), p = kt$p.value,
         method = "Kruskal-Wallis test")
  }
}

# Decide Pearson vs Spearman based on shapiro of both
choose_corr_test <- function(x, y) {
  ok <- complete.cases(x, y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(list(test = NA, estimate = NA, p = NA))
  shap_x <- if (length(x) >= 3 && length(x) <= 5000)
    tryCatch(shapiro.test(x)$p.value, error = function(e) 0) else 0
  shap_y <- if (length(y) >= 3 && length(y) <= 5000)
    tryCatch(shapiro.test(y)$p.value, error = function(e) 0) else 0
  if (shap_x > 0.05 && shap_y > 0.05) {
    ct <- suppressWarnings(cor.test(x, y, method = "pearson"))
    list(test = "Pearson", estimate = unname(ct$estimate), p = ct$p.value,
         method = "Pearson correlation")
  } else {
    ct <- suppressWarnings(cor.test(x, y, method = "spearman"))
    list(test = "Spearman", estimate = unname(ct$estimate), p = ct$p.value,
         method = "Spearman rank correlation")
  }
}

# Chi-square with Fisher fallback for sparse cells
choose_chisq <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 4) return(list(test = NA, statistic = NA, p = NA))
  tab <- table(x[ok], y[ok])
  if (any(dim(tab) < 2)) return(list(test = NA, statistic = NA, p = NA,
                                      method = "single category"))
  ex_low <- tryCatch({
    e <- chisq.test(tab)$expected
    any(e < 5)
  }, error = function(e) TRUE, warning = function(e) TRUE)
  if (ex_low) {
    ft <- tryCatch(fisher.test(tab, simulate.p.value = TRUE, B = 10000),
                   error = function(e) NULL)
    if (is.null(ft)) return(list(test = "Fisher", statistic = NA, p = NA,
                                  method = "Fisher exact (failed)"))
    list(test = "Fisher", statistic = NA, p = ft$p.value,
         method = "Fisher's exact test")
  } else {
    cs <- suppressWarnings(chisq.test(tab))
    list(test = "Chi-square", statistic = unname(cs$statistic), p = cs$p.value,
         method = "Pearson chi-square test")
  }
}

message("Setup loaded.  Project dir = ", PROJECT_DIR)
