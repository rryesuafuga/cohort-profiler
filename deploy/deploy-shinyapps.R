# ---------------------------------------------------------------------------
# Deploy cohort-profiler to shinyapps.io.
#
# Run this from an R session on your own machine, from the repo root
# (one-time: install.packages("rsconnect")).
#
# What a shinyapps.io deployment is, honestly:
#   * free tier: 25 active hours per month, then the app sleeps until the 1st
#   * the URL is PUBLIC -- there is no authentication below the paid tiers.
#     Anyone with the link can open the app and upload data through it.
#   * DOCX only: shinyapps.io has no LibreOffice and no way to install one,
#     so the PDF button does not appear there. The Docker deployment keeps
#     both formats.
#
# One-time account setup:
#   1. Create a free account at https://www.shinyapps.io
#   2. Dashboard -> Account -> Tokens -> Show -> "Copy to clipboard"
#   3. Paste the copied rsconnect::setAccountInfo(...) call into your R
#      console and run it. Never commit that line to the repo.
# ---------------------------------------------------------------------------

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("First run: install.packages('rsconnect')", call. = FALSE)
}
if (!file.exists("app.R")) {
  stop("Run this from the repo root (the folder containing app.R).",
       call. = FALSE)
}
if (!length(rsconnect::accounts()$name)) {
  stop(paste("No shinyapps.io account is linked yet. Paste the",
             "rsconnect::setAccountInfo(...) line from your shinyapps.io",
             "dashboard (Account -> Tokens) into the console first."),
       call. = FALSE)
}

# Packages installed from Posit Package Manager carry "Repository: RSPM" in
# their DESCRIPTION. The deploy manifest resolves that name against
# options("repos"); when no repo named RSPM exists there, the bare name leaks
# into the manifest and the shinyapps build dies fetching any package missing
# from its cache ("Unsupported url scheme: RSPM/src/contrib/..."). Give both
# conventional names real URLs so every package in the manifest resolves.
repos <- getOption("repos")
if (!"RSPM" %in% names(repos)) {
  repos <- c(repos, RSPM = "https://packagemanager.posit.co/cran/latest")
}
if (!"CRAN" %in% names(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  repos["CRAN"] <- "https://cloud.r-project.org"
}
options(repos = repos)

# .rscignore trims the bundle; what remains is exactly what the server needs.
rsconnect::deployApp(
  appDir = ".",
  appName = "cohort-profiler",
  appTitle = "Cohort Profiler",
  forceUpdate = TRUE
)
