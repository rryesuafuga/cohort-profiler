#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# make_synthetic_fixture.R
#
# Generates a fully SYNTHETIC dataset that mimics the shape of a VITAL-HMB
# REDCap label export. No real participant data is involved at any point.
#
# The point of this file is to be the only dataset in the repo: development,
# tests and the end-to-end render smoke test all run against it.
#
# What it reproduces from the real export:
#   * column NAMES (so label-substring matching resolves the same way)
#   * value VOCABULARIES ("Yes"/"No", "Checked"/"Unchecked", Likert wordings)
#   * one row per participant per event, with a non-Baseline event to filter out
#   * ISO-8859-1 encoding, and a degree sign in a header to exercise it
#   * approximate marginal distributions and effect directions, so univariate
#     models converge and the tables look plausible
#
# What it deliberately breaks, so the validation gate has something to catch:
#   * blanks, "Unknown", "Don't know", "Prefer not to say"
#   * values outside the plausible range (BMI, heart rate)
#   * consistency violations (menarche >= age, SBP < DBP, bleeding > cycle)
#   * one duplicated Record ID
#   * an all-missing column (Serum Iron) and two near-empty ones (HGB, ferritin)
#   * two perfectly separating predictors, as in the real cohort
#
# Usage:
#   Rscript data-raw/make_synthetic_fixture.R [n] [outfile]
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
n       <- if (length(args) >= 1) as.integer(args[1]) else 400L
outfile <- if (length(args) >= 2) args[2] else "data-raw/synthetic_vital_hmb.csv"

set.seed(20260630)  # fixed: the fixture must be byte-identical between runs

# --- helpers ---------------------------------------------------------------

# Numeric drawn from a different normal depending on outcome, clamped and rounded.
num_by <- function(hmb, m0, s0, m1, s1, lo = -Inf, hi = Inf, digits = 1) {
  x <- ifelse(hmb == 1L, rnorm(length(hmb), m1, s1), rnorm(length(hmb), m0, s0))
  round(pmin(pmax(x, lo), hi), digits)
}

# Yes/No with outcome-dependent probability.
yn_by <- function(hmb, p0, p1) {
  p <- ifelse(hmb == 1L, p1, p0)
  ifelse(runif(length(p)) < p, "Yes", "No")
}

# Categorical with a different level distribution per outcome group.
cat_by <- function(hmb, levels, p0, p1) {
  out <- character(length(hmb))
  i0 <- hmb == 0L; i1 <- hmb == 1L
  out[i0] <- sample(levels, sum(i0), TRUE, prob = p0)
  out[i1] <- sample(levels, sum(i1), TRUE, prob = p1)
  out
}

# A block of Likert items sharing a latent severity, e.g. PHQ-9 or GAD-7.
# Column names are "<prefix><j>. item <j>" so the ^L\d+\. style regex resolves.
item_block <- function(hmb, prefix, k, vocab, mu0, mu1, sd_person = 0.55,
                       sd_item = 0.45) {
  latent <- ifelse(hmb == 1L,
                   rnorm(length(hmb), mu1, sd_person),
                   rnorm(length(hmb), mu0, sd_person))
  cols <- lapply(seq_len(k), function(j) {
    z   <- latent + rnorm(length(latent), 0, sd_item)
    idx <- pmin(pmax(round(z), 0), length(vocab) - 1L) + 1L
    vocab[idx]
  })
  names(cols) <- sprintf("%s%d. %s item %d", prefix, seq_len(k), prefix, seq_len(k))
  cols
}

# Scatter NA through a vector.
blank_out <- function(x, p) { x[runif(length(x)) < p] <- NA; x }

# Replace a small share of values with non-answer strings the cleaner must drop.
dirty <- function(x, p = 0.01,
                  vals = c("Unknown", "Don't know", "", "Prefer not to say")) {
  i <- which(runif(length(x)) < p)
  x[i] <- sample(vals, length(i), TRUE)
  x
}

# --- outcome ---------------------------------------------------------------

hmb <- rbinom(n, 1L, 0.14)   # ~14% HMB+, matching the real cohort

d <- list(
  `Record ID`  = sprintf("VH-%04d", seq_len(n)),
  `Event Name` = rep("Baseline", n),
  `HMB calc`   = hmb
)

# --- demographics ----------------------------------------------------------

age <- num_by(hmb, 16.8, 1.8, 16.9, 1.7, lo = 12, hi = 24, digits = 0)

d[["agecalc"]] <- age
d[["B1. Which region do you come from?"]] <- cat_by(
  hmb, c("Central", "Eastern", "Northern", "Western", "Other"),
  p0 = c(.064, .843, .078, .010, .005),
  p1 = c(.054, .810, .088, .027, .021))
d[["B2. In what type of environment do you live?"]] <- cat_by(
  hmb, c("Rural", "Semi-urban", "Urban"),
  p0 = c(.209, .355, .436), p1 = c(.189, .311, .500))
d[["B3. What is your marital status?"]] <- cat_by(
  hmb, c("Single", "Married", "Separated"),
  p0 = c(.992, .006, .002), p1 = c(1, 0, 0))
d[["B4. Are you currently in school?"]] <- yn_by(hmb, .998, .986)
d[["B5. What is your highest level of education completed?"]] <- cat_by(
  hmb, c("Secondary", "Tertiary"), p0 = c(.945, .055), p1 = c(.932, .068))
d[["B6. What is your religious affiliation?"]] <- cat_by(
  hmb, c("Anglican", "Catholic", "Moslem", "Pentecostal", "Other"),
  p0 = c(.316, .239, .278, .150, .017),
  p1 = c(.162, .284, .345, .203, .007))
d[["B9. How well would you say you are managing financially these days?"]] <- cat_by(
  hmb, c("Living comfortably", "Doing alright", "Just getting by",
         "Finding it quite difficult", "Finding it very difficult"),
  p0 = c(.30, .38, .22, .07, .03), p1 = c(.29, .37, .23, .08, .03))

# --- household assets (drive the SES principal component) -------------------

wealth <- ifelse(hmb == 1L, rnorm(n, -0.35, 1), rnorm(n, 0.06, 1))
assets <- c("Radio", "Television", "Bicycle", "Motor cycle", "Computer",
            "Tablet", "Business", "Bathroom", "Running water", "Electricity",
            "Solar", "Car", "Generator", "Mattress", "Bed with mattress",
            "Cupboard", "Truck", "Microwave", "Dining table", "Family home",
            "Smartphone")
base_p <- qlogis(runif(length(assets), 0.15, 0.85))
for (j in seq_along(assets)) {
  d[[assets[j]]] <- ifelse(
    runif(n) < plogis(base_p[j] + 0.9 * wealth), "Checked", "Unchecked")
}

# --- social history --------------------------------------------------------

d[["D1. Do you currently smoke cigarettes or any tobacco product?"]] <- cat_by(
  hmb, c("Never smoked", "Yes, occasionally"), p0 = c(1, 0), p1 = c(.993, .007))
d[["D9. Which of the following best describes your use of alcohol?"]] <- cat_by(
  hmb, c("Never drink alcohol", "Drink occasionally", "Drink weekly"),
  p0 = c(.954, .039, .007), p1 = c(.899, .081, .020))
d[["B11. What is the main source of drinking water for your household?"]] <- cat_by(
  hmb, c("Piped water", "Borehole", "Protected spring", "Surface water"),
  p0 = c(.587, .229, .130, .054), p1 = c(.595, .223, .128, .054))
d[["B16. What is the main source of energy for cooking in your household?"]] <- cat_by(
  hmb, c("Charcoal", "Fire wood", "Gas", "Electricity for cooking"),
  p0 = c(.627, .173, .140, .060), p1 = c(.561, .149, .200, .090))

# --- obstetrics and gynaecology --------------------------------------------

menarche <- num_by(hmb, 13.2, 1.3, 12.8, 1.4, lo = 9, hi = 18, digits = 0)
cycle    <- num_by(hmb, 29.0, 3.6, 28.8, 3.5, lo = 18, hi = 45, digits = 0)
bleed    <- num_by(hmb,  4.0, 1.1,  4.8, 1.9, lo = 1,  hi = 12, digits = 0)

d[["E1. At what age did you start your first menstrual period?"]] <- menarche
d[["E3. How long was your previous menstrual cycle in days?"]]    <- cycle
d[["E4. During the last 6 months, did you have menstrual periods every month?"]] <-
  yn_by(hmb, .985, .966)
d[["E6. During the last 6 months, on average, how many days did you spend actively bleeding?"]] <- bleed
d[["E7. During the last 6 months, how many days do you experience on average in between periods?"]] <-
  num_by(hmb, 28.7, 3.2, 28.6, 3.7, lo = 15, hi = 60, digits = 0)
d[["E8. During the last 6 months, what was the longest interval between periods?"]] <-
  num_by(hmb, 28.4, 2.1, 27.7, 2.7, lo = 15, hi = 45, digits = 0)
d[["E9. During the last 6 months, what was the shortest interval between periods?"]] <-
  num_by(hmb, 26.1, 3.4, 25.9, 4.1, lo = 12, hi = 40, digits = 0)
d[["E10.In the past 6 months, have you experienced bleeding between periods?"]] <-
  yn_by(hmb, .096, .155)
d[["E11. During the last 6 months, how would you characterize your menstrual flow?"]] <-
  cat_by(hmb, c("Light", "Normal", "Heavy"),
         p0 = c(.079, .744, .176), p1 = c(.095, .486, .419))
d[["E19. How many times have you been pregnant?"]] <-
  rbinom(n, 1L, ifelse(hmb == 1L, .014, .001))
d[["H10. Have you ever been pregnant?"]] <- yn_by(hmb, .001, .014)
d[["I42. If you experience pelvic pain, rate its severity (0-10)"]] <-
  pmin(pmax(round(ifelse(hmb == 1L, rnorm(n, 3.4, 2.6), rnorm(n, 1.9, 2.3))), 0), 10)

# --- medical history -------------------------------------------------------

d[["C2. Do you currently have any chronic illness?"]]  <- yn_by(hmb, .031, .090)
d[["C3. Malaria in the last 3 months"]]                <- yn_by(hmb, .345, .439)
d[["C7. Urinary Tract Infection in the last 3 months"]] <- yn_by(hmb, .136, .182)
d[["C9. Peptic Ulcer disease"]]                        <- yn_by(hmb, .200, .243)
d[["C15. Have you taken any iron supplements or multi-vitamins?"]] <-
  yn_by(hmb, .118, .196)

# Perfect separation, exactly as in the real cohort: these must be reported as
# non-estimable rather than yielding an absurd odds ratio.
d[["H4. Have you ever been treated for anemia?"]] <-
  ifelse(hmb == 1L, ifelse(runif(n) < .21, "Yes", "No"), "No")
d[["H5. Has anyone in your family ever been diagnosed with a bleeding disorder?"]] <-
  ifelse(hmb == 1L, ifelse(runif(n) < .35, "Yes", "No"), "No")

d[["H6. Have you ever had a tooth extracted and bled longer than expected?"]] <-
  yn_by(hmb, .299, .561)
d[["H8. Have you ever had surgery other than dental with heavy bleeding?"]] <-
  yn_by(hmb, .021, .041)
d[["H12. Do you have a history of easy bruising or nosebleeds?"]] <-
  yn_by(hmb, .152, .212)
d[["I36. Have you sought health care for menstrual problems?"]] <-
  yn_by(hmb, .246, .419)

# --- clinical measurements -------------------------------------------------

height <- num_by(hmb, 161.1, 5.9, 161.0, 5.9, lo = 135, hi = 185)
bmi    <- num_by(hmb,  22.5, 3.5,  22.8, 3.8, lo = 14,  hi = 42)
sbp    <- num_by(hmb, 109.7, 10.4, 106.7, 11.4, lo = 85, hi = 155, digits = 0)
dbp    <- num_by(hmb,  72.9,  8.6,  70.4,  8.9, lo = 50, hi = 100, digits = 0)

d[["F6. BMI is (auto-calculated)"]]     <- bmi
d[["Average Weight: (kg)"]]             <- round(bmi * (height / 100)^2, 1)
d[["Average Height: (cm)"]]             <- height
d[["Average - Systolic BP (mmHg)"]]     <- sbp
d[["Average - Diastolic BP (mmHg)"]]    <- dbp
d[["Average Heart Rate: (bpm)"]]        <- num_by(hmb, 79.9, 11.8, 79.8, 12.6,
                                                  lo = 50, hi = 130, digits = 0)
# Placeholder: as.data.frame() mangles non-ASCII names under a C locale, so the
# degree sign is substituted back in just before writing (see "write" below).
d[["Average Temperature (__DEG__C)"]]   <- num_by(hmb, 36.8, 0.4, 36.8, 0.5,
                                                  lo = 35, hi = 39)

# --- ultrasound (about 30% not scanned) ------------------------------------

d[["S2. Position of the uterus"]] <- blank_out(cat_by(
  hmb, c("Anteverted", "Retroverted", "Others"),
  p0 = c(.888, .098, .014), p1 = c(.936, .053, .011)), .30)
d[["S5. What is the myometrial echo texture?"]] <- blank_out(cat_by(
  hmb, c("Homogenous", "Heterogenous", "Others"),
  p0 = c(.991, .008, .001), p1 = c(1, 0, 0)), .30)
d[["S13. Endometrial thickness (mm)"]] <- blank_out(
  num_by(hmb, 8.5, 3.1, 9.0, 3.7, lo = 1, hi = 25), .30)
d[["S15. Endometrial echo pattern"]] <- blank_out(cat_by(
  hmb, c("Uniform", "Bright edge", "Non-uniform"),
  p0 = c(.975, .022, .003), p1 = c(1, 0, 0)), .30)

# --- laboratory (largely not yet collected in the real cohort) --------------

d[["HGB (g/dL)"]]      <- blank_out(num_by(hmb, 10.8, 2.2, 10.4, 2.0,
                                           lo = 5, hi = 17), .86)
d[["Serum Ferritin (ng/mL)"]] <- blank_out(num_by(hmb, 19.5, 7.3, 17.1, 14.0,
                                                  lo = 2, hi = 120), .93)
d[["Serum Iron (ug/dL)"]]     <- rep(NA_real_, n)   # never collected

# --- mental health and quality of life -------------------------------------

phq_vocab  <- c("Not at all", "Several days", "More than half the days",
                "Nearly every day")
sev_vocab  <- c("Not at all", "Mild", "Moderate", "Severe")
freq_vocab <- c("Never", "Once or twice", "A few times", "In most periods",
                "In every period")

d <- c(d,
       item_block(hmb, "L", 9, phq_vocab,  mu0 = 0.40, mu1 = 0.65),
       item_block(hmb, "M", 7, phq_vocab,  mu0 = 0.40, mu1 = 0.62),
       item_block(hmb, "J", 8, sev_vocab,  mu0 = 0.75, mu1 = 1.05),
       item_block(hmb, "K", 6, freq_vocab, mu0 = 0.90, mu1 = 1.40))

df <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Deliberate defects. Every line below exists so a specific validation rule has
# something to fire on; keep them in sync with tests/testthat/test-validate.R.
# ---------------------------------------------------------------------------

# 1. Non-answer strings scattered through the categorical columns.
chr_cols <- names(df)[vapply(df, is.character, logical(1))]
chr_cols <- setdiff(chr_cols, c("Record ID", "Event Name"))
for (cl in chr_cols) df[[cl]] <- dirty(df[[cl]], p = 0.012)

# 2. Genuine item-level missingness.
for (cl in chr_cols) df[[cl]] <- blank_out(df[[cl]], 0.02)

# 3. Out-of-range values -> must become NA and be counted, not silently kept.
df[[ "F6. BMI is (auto-calculated)" ]][c(3, 17)]        <- c(6.2, 78.4)
df[[ "Average Heart Rate: (bpm)" ]][c(9, 25)]           <- c(12, 240)

# 4. Consistency-rule violations.
df[[ "E1. At what age did you start your first menstrual period?" ]][5]  <-
  df[["agecalc"]][5] + 2                                    # menarche >= age
tmp <- df[["Average - Systolic BP (mmHg)"]][11]
df[["Average - Systolic BP (mmHg)"]][11]  <- df[["Average - Diastolic BP (mmHg)"]][11]
df[["Average - Diastolic BP (mmHg)"]][11] <- tmp            # SBP < DBP
df[["E6. During the last 6 months, on average, how many days did you spend actively bleeding?"]][13] <-
  df[["E3. How long was your previous menstrual cycle in days?"]][13] + 3

# 5. A duplicated Record ID within the analytic sample.
df[["Record ID"]][n] <- df[["Record ID"]][1]

# 6. Follow-up rows: right Record IDs, no alkaline hematin result. These must be
#    excluded by the event filter, not counted as participants.
fu <- df[sample(seq_len(n), floor(n * 0.35)), ]
fu[["Event Name"]] <- "Month 6"
fu[["HMB calc"]]   <- NA
fu[, setdiff(names(fu), c("Record ID", "Event Name", "HMB calc", "agecalc"))] <- NA

out <- rbind(df, fu)
out <- out[order(out[["Record ID"]],
                 factor(out[["Event Name"]], levels = c("Baseline", "Month 6"))), ]

# --- write -----------------------------------------------------------------

dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

# Everything generated above is ASCII, so write plainly and then swap the
# placeholder for a single Latin-1 degree byte (0xB0). Doing it at byte level
# rather than via fileEncoding keeps the output identical under any locale --
# R silently transliterates non-ASCII names to "<U+00B0>" under LANG=C.
#
# The result is an ISO-8859-1 file with exactly one non-ASCII header. A reader
# that forgets encoding = "ISO-8859-1" will fail to match this column, which is
# the point: it makes the encoding assumption testable.
write.csv(out, outfile, row.names = FALSE, na = "")

b   <- readBin(outfile, "raw", file.info(outfile)$size)
pat <- charToRaw("__DEG__")
hit <- Filter(
  function(i) i + length(pat) - 1L <= length(b) &&
              identical(b[i:(i + length(pat) - 1L)], pat),
  which(b == pat[1]))
stopifnot("degree-sign placeholder not found exactly once" = length(hit) == 1L)
writeBin(c(b[seq_len(hit - 1L)],
           as.raw(0xB0),
           b[(hit + length(pat)):length(b)]), outfile)

cat(sprintf(
  "Wrote %s\n  %d rows (%d Baseline, %d follow-up) x %d columns\n  HMB+: %d (%.1f%%)\n",
  outfile, nrow(out), sum(out[["Event Name"]] == "Baseline"),
  sum(out[["Event Name"]] == "Month 6"), ncol(out),
  sum(out[["HMB calc"]] == 1, na.rm = TRUE),
  100 * mean(out[["HMB calc"]] == 1, na.rm = TRUE)))
