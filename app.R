# ---------------------------------------------------------------------------
# Shiny front end.
#
# Thin by design: this file collects a file, calls into R/, and offers the
# results for download. All the analysis logic lives in R/ so it can be tested
# without a browser.
#
# Uploaded exports contain participant data. Nothing is written to the app
# directory, and everything is deleted when the session ends.
# ---------------------------------------------------------------------------

library(shiny)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

MAX_UPLOAD_MB <- 50
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)

# shinyapps.io has no LibreOffice and no way to install one, so a hosted
# instance there produces Word only. Anywhere the converter exists (the
# Docker image, a desktop with LibreOffice) the PDF comes back automatically.
PDF_AVAILABLE <- soffice_available()

available_specs <- function() {
  files <- list.files("spec", pattern = "\\.ya?ml$", full.names = TRUE)
  if (!length(files)) return(character(0))
  labels <- vapply(files, function(f) {
    s <- tryCatch(yaml::yaml.load_file(f), error = function(e) NULL)
    if (is.null(s)) basename(f)
    else paste(c(s$study, s$title), collapse = " ")
  }, character(1))
  stats::setNames(files, labels)
}

# --- UI ---------------------------------------------------------------------

ui <- fluidPage(
  title = "Cohort Profiler",
  tags$head(tags$style(HTML("
    body { max-width: 1100px; margin: 0 auto; }
    .muted { color: #666; font-size: 90%; }
    .problem { background: #fdf2f2; border-left: 4px solid #c0392b;
               padding: 10px 14px; margin: 8px 0; }
    .notice  { background: #fff9e6; border-left: 4px solid #d4a017;
               padding: 10px 14px; margin: 8px 0; }
    .ok      { background: #f0f7f0; border-left: 4px solid #2e7d32;
               padding: 10px 14px; margin: 8px 0; }
  "))),

  h2("Cohort Profiler"),
  p(class = "muted",
    "Upload a REDCap label export. It is checked against the study spec, then",
    "described and analysed one variable at a time. Output is Word and PDF."),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("spec", "Study spec", choices = available_specs()),
      fileInput("upload", sprintf("REDCap export (.csv, max %d MB)", MAX_UPLOAD_MB),
                accept = ".csv", placeholder = "No file selected"),
      actionButton("run", "Check and build report", class = "btn-primary"),
      tags$hr(),
      if (!PDF_AVAILABLE) p(class = "muted",
        "This deployment produces Word (.docx) only. The PDF needs",
        "LibreOffice, which is available in the Docker version."),
      uiOutput("downloads"),
      tags$hr(),
      p(class = "muted",
        "Uploaded data stays in memory for this session only and is deleted",
        "when you close the page.")
    ),

    mainPanel(
      width = 8,
      uiOutput("status"),
      uiOutput("summary")
    )
  )
)

# --- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # Everything written during this session, deleted when it ends.
  session_files <- reactiveVal(character(0))
  track <- function(paths) session_files(c(session_files(), paths))

  results <- reactiveVal(NULL)
  status  <- reactiveVal(NULL)

  observeEvent(input$run, {
    results(NULL)
    status(NULL)

    if (is.null(input$upload)) {
      status(list(kind = "problem",
                  title = "No file selected",
                  body = "Choose a REDCap CSV export first."))
      return()
    }
    track(input$upload$datapath)

    spec_file <- input$spec
    spec <- tryCatch(read_spec(spec_file), error = function(e) e)
    if (inherits(spec, "error")) {
      status(list(kind = "problem", title = "The study spec could not be read",
                  body = conditionMessage(spec)))
      return()
    }

    raw <- tryCatch(read_export(input$upload$datapath), error = function(e) e)
    if (inherits(raw, "error")) {
      status(list(kind = "problem", title = "The file could not be read",
                  body = conditionMessage(raw)))
      return()
    }

    validation <- tryCatch(validate_export(raw, spec), error = function(e) e)
    if (inherits(validation, "error")) {
      status(list(kind = "problem", title = "The file could not be checked",
                  body = conditionMessage(validation)))
      return()
    }

    if (!validation$ok) {
      status(list(
        kind = "problem",
        title = sprintf("This export cannot be analysed yet (%d %s found)",
                        length(validation$hard),
                        if (length(validation$hard) == 1) "problem" else "problems"),
        items = vapply(validation$hard, `[[`, character(1), "message")))
      return()
    }

    # A render takes 20-40 seconds. Without progress the button reads as a crash.
    out <- withProgress(
      message = "Building the report", value = 0,
      {
        tryCatch(
          render_report(
            data_file = input$upload$datapath,
            spec_file = spec_file,
            out_dir   = file.path(tempdir(), paste0("report-", session$token)),
            basename_out = tools::file_path_sans_ext(input$upload$name),
            formats   = if (PDF_AVAILABLE) c("docx", "pdf") else "docx",
            progress = function(frac, msg) setProgress(value = frac, detail = msg)
          ),
          error = function(e) e)
      })

    if (inherits(out, "error")) {
      status(list(kind = "problem", title = "The report could not be built",
                  body = conditionMessage(out)))
      return()
    }

    track(unlist(out, use.names = FALSE))
    results(list(paths = out, validation = validation, spec = spec, raw = raw))
    status(list(kind = "ok",
                title = "Report ready",
                body = sprintf("%d participants analysed. Download below.",
                               validation$n_analytic)))
  })

  output$status <- renderUI({
    s <- status()
    if (is.null(s)) {
      return(div(class = "muted",
                 p("Select a spec and a CSV export, then press",
                   strong("Check and build report"), ".")))
    }
    body <- if (!is.null(s$items)) {
      tags$ol(lapply(s$items, tags$li))
    } else {
      p(s$body)
    }
    div(class = s$kind, h4(s$title), body)
  })

  output$summary <- renderUI({
    r <- results()
    if (is.null(r)) return(NULL)
    v <- r$validation
    soft <- v$soft
    tagList(
      h4("What was checked"),
      tags$ul(
        tags$li(sprintf("%d rows in the file, %d participants in the analysis sample",
                        v$n_rows, v$n_analytic)),
        tags$li(sprintf("%d of %d spec columns matched",
                        length(v$columns),
                        length(spec_variables(r$spec)) -
                          length(spec_variables(r$spec, type = "derived")))),
        tags$li("No duplicate participant IDs")
      ),
      if (length(soft)) {
        div(class = "notice",
            h5(sprintf("%d point%s to be aware of", length(soft),
                       if (length(soft) == 1) "" else "s")),
            tags$ul(lapply(soft, function(f) tags$li(f$message))),
            p(class = "muted", "These did not stop the report. Details are in
               the data-quality section of the document."))
      }
    )
  })

  output$downloads <- renderUI({
    r <- results()
    if (is.null(r)) {
      return(p(class = "muted", "Downloads appear here once a report is built."))
    }
    if (PDF_AVAILABLE) {
      tagList(
        downloadButton("dl_docx", "Word (.docx)", class = "btn-block"),
        tags$br(),
        downloadButton("dl_pdf", "PDF", class = "btn-block"),
        tags$br(),
        downloadButton("dl_zip", "Both (.zip)", class = "btn-block")
      )
    } else {
      downloadButton("dl_docx", "Word (.docx)", class = "btn-block")
    }
  })

  dl_name <- function(ext) {
    base <- tools::file_path_sans_ext(input$upload$name %||% "cohort-report")
    sprintf("%s-report.%s", base, ext)
  }

  output$dl_docx <- downloadHandler(
    filename = function() dl_name("docx"),
    content = function(file) file.copy(results()$paths$docx, file)
  )

  output$dl_pdf <- downloadHandler(
    filename = function() dl_name("pdf"),
    content = function(file) {
      req(results()$paths$pdf)
      file.copy(results()$paths$pdf, file)
    }
  )

  output$dl_zip <- downloadHandler(
    filename = function() dl_name("zip"),
    content = function(file) {
      zipfile <- file.path(tempdir(), paste0("bundle-", session$token, ".zip"))
      zip_outputs(results()$paths, zipfile)
      track(zipfile)
      file.copy(zipfile, file)
    }
  )

  # Participant data must not outlive the session.
  session$onSessionEnded(function() {
    paths <- isolate(session_files())
    for (p in paths) {
      if (!is.na(p) && file.exists(p)) unlink(p, recursive = TRUE, force = TRUE)
    }
    unlink(file.path(tempdir(), paste0("report-", session$token)),
           recursive = TRUE, force = TRUE)
  })
}

shinyApp(ui, server)
