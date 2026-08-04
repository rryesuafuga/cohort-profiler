#!/usr/bin/env Rscript
# Replicates the original report's column resolution against the fixture and
# checks that every key matches exactly one column. Ambiguous or missing keys
# are the failure mode this whole repo exists to prevent.

f <- commandArgs(trailingOnly = TRUE)[1]
raw <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
                fileEncoding = "latin1", colClasses = "character")
nm <- names(raw)

keys <- c(
  "Record ID", "Event Name", "HMB calc", "agecalc",
  "B1. Which region", "B2. In what type of environment",
  "B3. What is your marital status", "B4. Are you currently in school",
  "B5. What is your highest level of education", "B6. What is your religious",
  "B9. How well would you say you are managing",
  "B11. What is the main source of drinking water",
  "B16. What is the main source of energy",
  "D1. Do you currently smoke", "D9. Which of the following best describes",
  "E1. At what age did you start", "E3. How long was your previous menstrual cycle",
  "E4. During the last 6 months, did you have menstrual periods every month",
  "E6. During the last 6 months, on average, how many days did you spend actively bleeding",
  "E7. During the last 6 months, how many days do you experience on average in between",
  "E8. During the last 6 months, what was the longest",
  "E9. During the last 6 months, what was the shortest",
  "E10.In the past 6 months, have you experienced bleeding",
  "E11. During the last 6 months, how would you characterize",
  "E19. How many times have you been pregnant", "H10. Have you ever been pregnant",
  "I42. If you experience pelvic",
  "C2. Do you currently have any chronic", "C3. Malaria",
  "C7. Urinary Tract Infection", "C9. Peptic Ulcer",
  "C15. Have you taken any iron supplements or multi-vitamins",
  "H4. Have you ever been treated for anemia",
  "H5. Has anyone in your family ever been diagnosed with a bleeding",
  "H6. Have you ever had a tooth extracted",
  "H8. Have you ever had surgery other than dental",
  "H12. Do you have a history of easy bruising",
  "I36. Have you sought health care for menstrual",
  "F6. BMI is", "Average Weight:", "Average Height:",
  "Average - Systolic BP", "Average - Diastolic BP",
  "Average Heart Rate:", "Average Temperature",
  "S13. Endometrial thickness", "S2. Position of the uterus",
  "S5. What is the myometrial echo texture", "S15. Endometrial echo pattern",
  "HGB", "Serum Ferritin", "Serum Iron")

fail <- 0L
for (k in keys) {
  hit <- if (k %in% nm) 1L else sum(grepl(k, nm, fixed = TRUE))
  if (hit != 1L) {
    cat(sprintf("  %-12s %s\n", if (hit == 0L) "MISSING" else "AMBIGUOUS", k))
    fail <- fail + 1L
  }
}
cat(sprintf("Column keys: %d checked, %d unresolved\n", length(keys), fail))

# Item blocks used by item_score()
for (r in c("^L\\d+\\.", "^M\\d+\\.", "^J\\d+\\.", "^K\\d+\\."))
  cat(sprintf("  %-10s -> %d columns\n", r, sum(grepl(r, nm))))

# Asset columns feeding the SES principal component
asset_pat <- paste0("radio|television|bicycle|motor ?cycle|computer|tablet|business|bathroom|",
                    "running water|electricity|solar|^car|generator|mattress|bed with|cupboard|",
                    "truck|microwave|dining|dinning|family home|smartphone")
acols <- nm[grepl(asset_pat, tolower(nm))]
cat(sprintf("  asset regex -> %d columns\n", length(acols)))

# The analytic sample, exactly as the report defines it
hmb <- suppressWarnings(as.numeric(raw[["HMB calc"]]))
df  <- raw[!is.na(hmb) & raw[["Event Name"]] == "Baseline", ]
cat(sprintf("\nAnalytic participants: %d | HMB+: %d (%.1f%%) | duplicate IDs: %d\n",
            nrow(df), sum(suppressWarnings(as.numeric(df[["HMB calc"]])) == 1, na.rm = TRUE),
            100 * mean(suppressWarnings(as.numeric(df[["HMB calc"]])) == 1, na.rm = TRUE),
            sum(duplicated(df[["Record ID"]]))))

# Does the SES PCA actually run?
amat <- sapply(acols, function(c) as.integer(tolower(trimws(df[[c]])) %in% c("checked", "yes")))
amat <- amat[, apply(amat, 2, function(z) sd(z, na.rm = TRUE) > 0), drop = FALSE]
ses  <- as.numeric(prcomp(scale(amat))$x[, 1])
cat(sprintf("SES PC1: mean %.2f, sd %.2f\n", mean(ses), sd(ses)))

# Does a univariate logistic on the strongest predictor converge?
y <- as.integer(suppressWarnings(as.numeric(df[["HMB calc"]])) == 1)
x <- suppressWarnings(as.numeric(df[[grep("E6. During the last 6 months, on average",
                                          nm, fixed = TRUE)[1]]]))
fit <- glm(y ~ x, family = binomial, subset = !is.na(x))
cat(sprintf("Bleeding days OR: %.2f (p = %.4g)\n",
            exp(coef(fit)[2]), summary(fit)$coefficients[2, 4]))

# Perfect separation must be detectable, not silently fitted
sep <- table(df[[grep("H5. Has anyone in your family", nm, fixed = TRUE)[1]]], y)
cat("Family bleeding disorder x outcome (expect a zero cell):\n")
print(sep)
