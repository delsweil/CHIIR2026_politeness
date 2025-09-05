# app.R
suppressPackageStartupMessages({
  library(shiny)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(jsonlite)
  library(digest)
  library(googlesheets4)
  library(googledrive)
})

options(gargle_quiet = TRUE)

# ========= Config =========
# Defaults to your absolute paths; can be overridden via env vars.
CONV_CSV <- Sys.getenv(
  "CONVERSATIONS_CSV",
  unset = "/Users/davidelsweiler/Dropbox/r_projects/Christine/politeness_conversational_cooking/Shiny_annotation/conversations.csv"
)

# Either set SHEET_URL as an env var or keep the literal URL here.
SHEET_URL <- Sys.getenv("SHEET_URL", unset =
                          "https://docs.google.com/spreadsheets/d/1ptey95993ubeJRVcWRXY9fOlrwz0AeU1iY5_mdatfyI/edit?usp=sharing"
)
TAB_NAME  <- "annotations"

# Your service-account JSON
SERVICE_ACCOUNT_JSON <- Sys.getenv(
  "GS_SERVICE_JSON",
  unset = "/Users/davidelsweiler/keys/bahnmail-4af20d203fde.json"
)

PROFILE_LEVELS <- c(
  "C1 Hyperpolite",
  "C2 Engagement-seeking",
  "C3 Direct / Low Politeness",
  "C4 Polite + Engaged",
  "C5 Impolite"
)

FALLBACK_CSV <- "annotations_fallback.csv"

ANNOTATION_HEADER <- c(
  "annotator",
  "blind_conversation_id",
  "conversation_id",   # <-- NEW
  "chosen_profile",
  "confidence",
  "notes",
  "timestamp_utc"
)


# ========= Helpers =========
ensure_sheet_header <- function(ss, tab, header) {
  # Create tab if missing
  existing <- tryCatch(googlesheets4::sheet_names(ss), error = function(e) character(0))
  if (!tab %in% existing) googlesheets4::sheet_add(ss, sheet = tab)
  
  # Read first row with names = TRUE; see if header already matches
  rng <- tryCatch(
    googlesheets4::read_sheet(ss, sheet = tab, range = "A1:Z1", col_names = TRUE),
    error = function(e) NULL
  )
  has_header <- !is.null(rng) && length(names(rng)) >= length(header) &&
    all(header %in% names(rng))
  
  if (!has_header) {
    # Write a 0-row tibble with the intended column names
    hdr_df <- tibble::as_tibble(setNames(replicate(length(header), character(0), simplify = FALSE), header))
    googlesheets4::sheet_write(data = hdr_df, ss = ss, sheet = tab)
  }
}

append_annotation <- function(ss, tab, row_df) {
  tryCatch({
    googlesheets4::sheet_append(ss = ss, data = row_df, sheet = tab)
    TRUE
  }, error = function(e) {
    # Local CSV fallback
    if (!file.exists(FALLBACK_CSV)) {
      readr::write_csv(row_df, FALLBACK_CSV)
    } else {
      readr::write_csv(
        bind_rows(readr::read_csv(FALLBACK_CSV, show_col_types = FALSE), row_df),
        FALLBACK_CSV
      )
    }
    FALSE
  })
}

format_turn_bubble <- function(role, text) {
  div(
    class = paste("bubble", if (tolower(role) == "user") "user" else "agent"),
    span(class = "turnrole", if (tolower(role) == "user") "User:" else "Agent:"),
    span(class = "turntext", text)
  )
}

# Defensive parser for json_turns
parse_turns <- function(turns_raw) {
  if (is.null(turns_raw) || length(turns_raw) == 0) return(NULL)
  
  # Already a list-column of rows?
  if (is.list(turns_raw) && !is.character(turns_raw)) {
    df <- tryCatch(tibble::as_tibble(turns_raw), error = function(e) NULL)
    if (!is.null(df)) {
      # Normalize role/text naming
      if ("content" %in% names(df) && !"text" %in% names(df)) {
        df$text <- df$content; df$content <- NULL
      }
      if (all(c("role","text") %in% names(df))) return(df)
    }
  }
  
  # Character JSON?
  if (is.character(turns_raw)) {
    txt <- turns_raw[1]
    if (!nzchar(txt)) return(NULL)
    
    # Try as-is
    j <- tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE), error = function(e) NULL)
    # If failed, try unescaping once (handles over-escaped CSV cases)
    if (is.null(j)) {
      txt2 <- gsub('^"|"$', "", txt)      # strip outer quotes
      txt2 <- gsub('\\"', '"', txt2)      # unescape
      j <- tryCatch(jsonlite::fromJSON(txt2, simplifyDataFrame = TRUE), error = function(e) NULL)
    }
    if (is.null(j)) return(NULL)
    
    # Direct data.frame with role/text or role/content
    if (is.data.frame(j)) {
      if (all(c("role","text") %in% names(j))) return(j)
      if (all(c("role","content") %in% names(j))) {
        j$text <- j$content; j$content <- NULL
        return(j[, c("role","text"), drop = FALSE])
      }
    }
    
    # Nested under typical keys
    for (k in c("turns","messages","data","items")) {
      if (!is.null(j[[k]])) {
        inner <- j[[k]]
        if (is.data.frame(inner) && all(c("role","text") %in% names(inner))) return(inner)
        if (is.data.frame(inner) && all(c("role","content") %in% names(inner))) {
          inner$text <- inner$content; inner$content <- NULL
          return(inner[, c("role","text"), drop = FALSE])
        }
        # if list of lists
        if (is.list(inner) && !is.data.frame(inner)) {
          df <- tryCatch(tibble::as_tibble(inner), error = function(e) NULL)
          if (!is.null(df)) {
            if ("content" %in% names(df) && !"text" %in% names(df)) {
              df$text <- df$content; df$content <- NULL
            }
            if (all(c("role","text") %in% names(df))) return(df)
          }
        }
      }
    }
    
    # List of lists at top level
    if (is.list(j) && !is.data.frame(j)) {
      df <- tryCatch(tibble::as_tibble(j), error = function(e) NULL)
      if (!is.null(df)) {
        if ("content" %in% names(df) && !"text" %in% names(df)) {
          df$text <- df$content; df$content <- NULL
        }
        if (all(c("role","text") %in% names(df))) return(df)
      }
    }
  }
  
  NULL
}

# ========= Load conversations =========
validate_file <- function(path) {
  if (!file.exists(path)) {
    stop("conversations.csv not found at: ", normalizePath(path, mustWork = FALSE))
  }
}

message("Using CONVERSATIONS_CSV at: ", normalizePath(CONV_CSV, mustWork = FALSE))
validate_file(CONV_CSV)

# Be permissive with types; ensure json_turns is character if present as text.
convs <- readr::read_csv(
  CONV_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    conversation_id = readr::col_character(),
    recipe_title    = readr::col_character(),
    json_turns      = readr::col_character(),
    cluster_hidden  = readr::col_guess()
  )
)

# Surface any readr issues for debugging
prob <- tryCatch(readr::problems(convs), error = function(e) NULL)
if (!is.null(prob) && nrow(prob)) {
  message("readr parsing issues detected (showing first 10):")
  print(utils::head(prob, 10))
}

req_cols <- c("conversation_id", "recipe_title", "json_turns", "cluster_hidden")
if (!all(req_cols %in% names(convs))) {
  stop("conversations.csv is missing columns: ",
       paste(setdiff(req_cols, names(convs)), collapse = ", "))
}

# Deterministic blind ID
convs <- convs %>%
  mutate(blind_id = substr(digest::digest(conversation_id, algo = "sha1"), 1, 10))

# ========= Google auth =========
gs_ok <- FALSE
auth_msg <- NULL

if (nzchar(SHEET_URL)) {
  if (file.exists(SERVICE_ACCOUNT_JSON)) {
    # Service account (headless)
    try({
      googledrive::drive_auth(path = SERVICE_ACCOUNT_JSON)
      googlesheets4::gs4_auth(path = SERVICE_ACCOUNT_JSON)
      header <- c("annotator", "blind_conversation_id", "chosen_profile",
                  "confidence", "notes", "timestamp_utc")
      #ensure_sheet_header(SHEET_URL, TAB_NAME, header)
      ensure_sheet_header(SHEET_URL, TAB_NAME, ANNOTATION_HEADER)
      gs_ok <- TRUE
      auth_msg <- "Using service account credentials."
    }, silent = TRUE)
  } else {
    # Fallback: cached user auth (do once interactively to create .secrets)
    try({
      googledrive::drive_auth(email = TRUE, cache = ".secrets")
      googlesheets4::gs4_auth(email = TRUE, cache = ".secrets")
      header <- c("annotator", "blind_conversation_id", "chosen_profile",
                  "confidence", "notes", "timestamp_utc")
      ensure_sheet_header(SHEET_URL, TAB_NAME, header)
      gs_ok <- TRUE
      auth_msg <- "Using cached user credentials from .secrets."
    }, silent = TRUE)
  }
}

if (!gs_ok && nzchar(SHEET_URL)) {
  message("Google Sheets auth failed. Ensure the Sheet is shared with the service-account email and the JSON path is correct: ", SERVICE_ACCOUNT_JSON)
}

# ========= UI =========
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif; }
      .bubble { border-radius: 14px; padding: 10px 12px; margin: 6px 0; max-width: 900px; }
      .bubble.user { background: #eef6ff; border: 1px solid #cfe3ff; }
      .bubble.agent { background: #f6f6f6; border: 1px solid #dddddd; }
      .turnrole { font-weight: 600; margin-right: 8px; }
      .turntext { white-space: pre-wrap; }
      .meta { color: #666; margin-bottom: 8px; }
      .status-ok { color: #1a7f37; font-weight: 600; }
      .status-warn { color: #b54708; font-weight: 600; }
      .status-err { color: #b42318; font-weight: 600; }
    "))
  ),
  titlePanel("Politeness Profile Annotation"),
  sidebarLayout(
    sidebarPanel(
      textInput("annotator", "Your name (for the log)", value = ""),
      selectInput("profile_pick", "Choose the profile:",
                  choices = c("Select…" = "", PROFILE_LEVELS),
                  selected = ""),
      sliderInput("confidence", "Confidence (%)", min = 0, max = 100, value = 80, step = 5),
      textAreaInput("notes", "Notes (optional)",
                    placeholder = "Any brief reasoning or comments...", width = "100%"),
      hr(),
      actionButton("prev", "◀ Prev"),
      actionButton("next_btn", "Next ▶"),
      actionButton("submit", "Save Annotation", class = "btn-primary"),
      br(), br(),
      htmlOutput("status"),
      hr(),
      helpText(if (gs_ok)
        paste0("✅ Google Sheets connected. ", auth_msg, " Annotations will be written to the sheet.")
        else if (nzchar(SHEET_URL))
          "⚠️ Google Sheets not connected. Check service account sharing/JSON path. Will save to local CSV fallback."
        else
          "ℹ️ No Google Sheet configured. Annotations will be saved to a local CSV fallback.")
    ),
    mainPanel(
      uiOutput("conv_meta"),
      uiOutput("turns_panel")
    )
  )
)

# ========= Server =========
server <- function(input, output, session) {
  # index into convs
  idx <- reactiveVal(1L)
  n_all <- nrow(convs)
  
  cur <- reactive({
    i <- idx()
    req(is.finite(i), i >= 1, i <= n_all)
    convs[i, ]
  })
  
  # Header/meta
  output$conv_meta <- renderUI({
    cdat <- cur()
    div(class = "meta",
        strong("Conversation: "), span(cdat$blind_id),
        HTML("&nbsp;&nbsp;•&nbsp;&nbsp;"),
        strong("Recipe: "), span(cdat$recipe_title),
        HTML("&nbsp;&nbsp;•&nbsp;&nbsp;"),
        span(sprintf("(%d of %d total)", idx(), n_all))
    )
  })
  
  # Turns panel (robust parsing)
  output$turns_panel <- renderUI({
    cdat <- cur()
    turns_raw <- cdat$json_turns[[1]]
    
    turns <- parse_turns(turns_raw)
    
    if (is.null(turns) || !all(c("role","text") %in% names(turns))) {
      preview <- NA_character_
      if (is.character(turns_raw)) preview <- substr(turns_raw, 1, 180)
      return(div(class = "status-err",
                 "Could not parse conversation turns for this row.",
                 if (!is.na(preview)) tags$div(style="margin-top:6px;",
                                               tags$code(preview), "...")))
    }
    
    n_show <- min(14L, nrow(turns))
    tagList(lapply(seq_len(n_show), function(i) {
      format_turn_bubble(turns$role[i], turns$text[i])
    }))
  })
  
  # Navigation
  observeEvent(input$prev, {
    idx(max(1L, idx() - 1L))
    output$status <- renderText("")
  })
  observeEvent(input$next_btn, {
    idx(min(n_all, idx() + 1L))
    output$status <- renderText("")
  })
  
  # Submit
  observeEvent(input$submit, {
    cdat <- cur()
    if (!nzchar(input$annotator)) {
      output$status <- renderUI(div(class = "status-warn", "Please enter your name first."))
      return()
    }
    if (!nzchar(input$profile_pick)) {
      output$status <- renderUI(div(class = "status-warn", "Please choose a profile."))
      return()
    }
    
    
    

    row <- tibble::tibble(
      annotator             = input$annotator,
      blind_conversation_id = cdat$blind_id,
      conversation_id       = cdat$conversation_id,
      chosen_profile        = input$profile_pick,
      confidence            = as.integer(input$confidence),
      notes                 = input$notes,
      timestamp_utc         = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
    
    ok <- FALSE
    if (nzchar(SHEET_URL) && gs_ok) {
      ok <- append_annotation(SHEET_URL, TAB_NAME, row)
    } else {
      # local fallback
      if (!file.exists(FALLBACK_CSV)) {
        readr::write_csv(row, FALLBACK_CSV)
      } else {
        readr::write_csv(
          bind_rows(readr::read_csv(FALLBACK_CSV, show_col_types = FALSE), row),
          FALLBACK_CSV
        )
      }
    }
    
    if (ok) {
      output$status <- renderUI(div(class = "status-ok", "Saved to Google Sheets."))
    } else if (nzchar(SHEET_URL)) {
      output$status <- renderUI(div(class = "status-warn",
                                    "Sheets write failed; saved to local fallback CSV instead."))
    } else {
      output$status <- renderUI(div(class = "status-ok",
                                    "Saved to local CSV (no Google Sheet configured)."))
    }
    
    # Reset form controls (keep annotator name)
    updateSelectInput(session, "profile_pick", selected = "")
    updateSliderInput(session, "confidence", value = 80)
    updateTextAreaInput(session, "notes", value = "")
    
    # auto-advance + clear notes
    idx(min(n_all, idx() + 1L))
    updateTextAreaInput(session, "notes", value = "")
  })
}

shinyApp(ui, server)
