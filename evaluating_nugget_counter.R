#!/usr/bin/env Rscript
# ============================================================
# Clean Nugget Counter + Evaluation (few-shot, repeats, progress)
# - Uses FAKE few-shot examples (paraphrased) so real data stays a fair test
# - Set OLLAMA_SERVER and MODEL_NAME below
# - Input: an in-memory `test.set` or CSV "nugget_counter_test_set.csv"
#   Required columns: conversation_id, recipe_title, step_id, combined_agent_text
#   Optional: annotator, nugget_count, nuggets_marked
# - Output: eval_clean_out/*.csv + quick summaries
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(stringr); library(jsonlite); library(httr)
})

# ---------- CONFIG ----------
OLLAMA_SERVER <- "http://hcai.ur.de:11434"  # <— set me
MODEL_NAME    <- "deepseek-r1:8b"
TEMPERATURE   <- 0.0
NUM_CTX       <- 8192
NUM_PREDICT   <- 512
REPEATS       <- 3L

INPUT_CSV     <- "nugget_counter_test_set.csv"    # used if `test.set` not in memory
OUTDIR        <- "eval_clean_out"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

# ---------- Utils ----------
`%||%` <- function(x, y) if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) y else x
as_chr1 <- function(x, default="") if (is.null(x) || length(x)==0 || (length(x)==1 && is.na(x))) default else as.character(x[[1]])
rtrim   <- function(x, ch="/") sub(paste0(ch, "+$"), "", x)
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_
}
safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (sum(!is.na(x)) >= 2) stats::sd(x, na.rm = TRUE) else 0
}

shots <- list(
  # 0-nugget (apology/meta)
  list(
    step_description = "General help",
    agent_text = "Sorry, I can’t provide details for that request. As an assistant, I have limitations.",
    answer_json = '{"ok":true,"nugget_count":0,"nuggets":[],"notes":""}'
  ),
  
  # 1-nugget (simple, discourage splitting)
  list(
    step_description = "Bread baking",
    agent_text = "Bake the loaf at 220°C until the crust turns golden brown.",
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Bake at 220°C until crust is golden"],"notes":""}'
  ),
  
  # 2-nugget (two distinct facts, avoid collapsing)
  list(
    step_description = "Pasta boiling",
    agent_text = "Add salt to the water to season the pasta. Stir occasionally to prevent noodles from sticking together.",
    answer_json = '{"ok":true,"nugget_count":2,"nuggets":["Salt in water seasons pasta","Stirring prevents noodles from sticking"],"notes":""}'
  ),

  # 1-nugget example (normalized nuggets)
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = "Folding beaten egg whites into the base serves a crucial role in creating a light and airy texture, which is essential for soufflés. This process gently incorporates air from the mixing, making the mixture fluffy and helping it rise during baking.",
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Folding egg whites adds air for lift"],"notes":"1 nugget example; normalized nuggets"}'
  ),
  
  # 2-nugget example (normalized nuggets)
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = paste(
      "I'm sorry, I don't have access to images or the ability to display pictures. However, I can describe the correct way to fold egg whites into the base for your recipe.",
      "To fold egg whites into the base correctly, gently mix them using a spatula in a zigzag motion until they are just incorporated without deflating the whites. This ensures the mixture remains light and airy."
    ),
    answer_json = '{"ok":true,"nugget_count":2,"nuggets":["Fold whites gently with a spatula","Keeps mixture light and airy"],"notes":"2 nugget example; normalized nuggets"}'
  ),
  
  # 3-nugget example (normalized nuggets)
  list(
    step_description = "Duck à l’Orange",
    agent_text = paste(
      "Pricking the duck's skin with a fork or skewer creates tiny punctures that help render the fat more effectively.",
      "When you dip it in boiling water, the heat helps to melt the duck fat from deeper within the meat and forces it out through the pricks.",
      "Drying thoroughly afterward prevents steam from forming during cooking, which can make the skin soggy instead of crisp."
    ),
    answer_json = '{"ok":true,"nugget_count":3,"nuggets":["Prick skin to render fat","Brief boil mobilizes fat outward","Dry thoroughly to keep skin crisp"],"notes":"3 nugget example; normalized nuggets"}'
  ),
  
  # 3-nugget example (normalized nuggets)
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = paste(
      "The science behind preheating the oven is crucial for achieving a perfectly risen soufflé.",
      "When baking, an oven that starts at the correct temperature ensures even cooking and proper rising of the egg whites.",
      "Egg whites in a soufflé rely on air incorporation during beating to create bubbles that expand with heat."
    ),
    answer_json = '{"ok":true,"nugget_count":3,"nuggets":["Preheat oven for proper rise","Correct temperature ensures even bake","Beaten egg bubbles expand with heat"],"notes":"3 nugget example; normalized nuggets"}'
  ),
  
  # 3-nugget (originally with duplication; normalized nuggets)
  list(
    step_description = "Buttermilk-Brined Southern Fried Chicken",
    agent_text = paste(
      "The optional double fry technique is a Southern U.S. method that enhances crispiness by allowing the chicken to rest after the first deep fry, which helps render out moisture and sets the crust.",
      "I apologize for any confusion or frustration with this step. The optional double fry process has historical roots in Southern American cooking traditions, where it was developed as a technique to achieve exceptionally crispy fried chicken by allowing time for excess moisture to render out during the chilling period."
    ),
    answer_json = '{"ok":true,"nugget_count":3,"nuggets":["Double-fry method from Southern cooking","Rest between fries reduces moisture and sets crust","Chilling period improves crispiness"],"notes":"3 nuggets (was: with duplication); normalized nuggets"}'
  ),
  
  # 4-nugget example (normalized nuggets)
  list(
    step_description = "Duck à l’Orange",
    agent_text = paste(
      "I'm sorry, I don't know how to show pictures. However, I can provide you with a detailed explanation on the correct way to perform the step mentioned in Duck à l’Orange: Optional: spatchcock and trim duck; reserve trimmings for stock.",
      "Spatchcocking is a cooking technique where you butterfly the duck by cutting it horizontally through the breast area, then flatten it. After trimming any excess fat or skin (as specified), you should reserve those trimmings to make stock later on.",
      "To spatchcock and trim: First, remove the duck's backbone to separate the body from the legs. Then, cut along the backbone to detach the wings and legs, leaving the breast intact but spread out. Trim any excess fat or skin if desired, then flatten the breast by cutting horizontally through it once you've removed the backbone.",
      "Reserving trimmings for stock: Keep those trimmed parts (like necks, backs, wing tips) along with vegetables or bones to make a flavorful stock later in the recipe."
    ),
    answer_json = '{"ok":true,"nugget_count":4,"nuggets":["Spatchcock to butterfly and flatten duck","Trim excess fat and skin","Remove backbone; detach wings/legs to flatten","Reserve trimmings to make stock"],"notes":"procedural knowledge with multiple steps; normalized nuggets"}'
  ),
  
  # 5-nugget example (normalized nuggets)
  list(
    step_description = "Pesto alla Genovese",
    agent_text = paste(
      "Pine nuts, also known as pignoli, have a long history and are traditionally used in pesto sauce from Genoa.",
      "They originate from the wild hazelnut tree (Corylus avellana) varieties that produce smaller nuts suitable for toasting and grinding into paste.",
      "The use of pine nuts dates back centuries in Liguria's cuisine.",
      "The ideal texture for the pine nut paste in Pesto alla Genovese is a smooth, creamy consistency similar to pesto itself, allowing it to blend well with other ingredients."
    ),
    answer_json = '{"ok":true,"nugget_count":5,"nuggets":["Pine nuts (pignoli) are traditional","Used in Genoa-style pesto","Said to come from small wild hazelnut varieties","Use dates back centuries in Liguria","Texture should be smooth and creamy"],"notes":"5 nugget example; normalized nuggets"}'
  ),
  
  # 7-nugget high-density example (normalized nuggets)
  list(
    step_description = "Buttermilk-Brined Southern Fried Chicken",
    agent_text = paste(
      "Certainly! The use of buttermilk and egg in a Southern-style fried chicken marinade is rooted in science.",
      "Buttermilk, which has lactic acid from fermentation, helps tenderize the chicken by breaking down proteins through enzymatic action and acidity.",
      "This process makes the meat more succulent.",
      "Eggs act as an emulsifier, binding ingredients together to create a cohesive coating that adheres better during frying.",
      "Additionally, eggs contribute to browning via the Maillard reaction when heated in oil.",
      "The next step is to combine all the marinade ingredients and let the chicken soak for at least four hours or up to overnight in the refrigerator."
    ),
    answer_json = '{"ok":true,"nugget_count":7,"nuggets":["Buttermilk and eggs have functional roles","Buttermilk contains lactic acid","Acid tenderizes chicken proteins","Tenderizing yields juicier meat","Eggs emulsify for better coating adhesion","Eggs aid browning via Maillard reaction","Marinate 4+ hours or overnight"],"notes":"high-nugget example; normalized nuggets"}'
  ),
  
  # 1-nugget combined instructions (normalized nuggets)
  list(
    step_description = "Old-Fashioned Apple Pie",
    agent_text = "To add tapioca, sprinkle it evenly over the bottom of the pie crust as a thickening agent. Then, to assemble the pie with a top crust, place the filling into the bottom crust, seal the edges by crimping them together, and lay the top crust on top before trimming and fluting if necessary. Finally, chill the assembled pie in the refrigerator for at least one hour to set the filling and make it easier to slice.",
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Add tapioca, assemble and seal top crust, chill before slicing"],"notes":"combined instructions; normalized nuggets"}'
  ),
  # 1-NUGGET (many sentences but really ONE factual unit)
  # Goal: reduce over-splitting in the 1-bin
  list(
    step_description = "Seasoning a soup base",
    agent_text = paste(
      "Use 1% salt by weight for the soup base (10 g per 1 kg of soup).",
      "Think of it as a simple rule-of-thumb you can remember.",
      "This keeps the seasoning in a balanced range without guesswork."
    ),
    answer_json = '{"ok":true,"nugget_count":1,"nuggets":["Season soup at ~1% salt by weight (≈10 g per 1 kg)"],"notes":""}'
  ),
  # 4-nugget example
  list(
    step_description = "Savory Cheese Soufflé",
    agent_text = paste(
      "The primary reason for serving a savory cheese soufflé immediately is that it contains egg whites, which act as a leavening agent.",
      "If you wait too long, the egg whites will deflate due to the breakdown of proteins and loss of air bubbles, causing your soufflé to collapse.",
      "Egg whites in the recipe rely on their protein content to form stable foam and provide lift through leavening.",
      "If you wait too long, these proteins break down due to enzymatic action or acidification from other ingredients like lemon juice or wine vinegar commonly used alongside cheese for flavor enhancement."
    ),
    answer_json = '{"ok":true,"nugget_count":4,"nuggets":["Serve soufflé immediately because of egg whites","Waiting causes deflation from protein breakdown","Proteins form foam and provide lift","Proteins break down with enzymes or acid"],"notes":"4 nugget example"}'
  ),
  
  # 4-nugget example
  list(
    step_description = "Pesto alla Genovese",
    agent_text = paste(
      "The process of emulsifying sauce involves combining ingredients that are typically immiscible, such as oils and water-based components.",
      "In this case, for Pesto alla Genovese, you're mixing cheeses (like Parmesan or Pecorino) which contain fats and proteins with olive oil.",
      "The use of Parmesan in emulsifying sauce is due to its high fat content and protein structure.",
      "These components help stabilize the mixture by reducing surface tension between the oil and water phases, allowing them to blend smoothly without separating."
    ),
    answer_json = '{"ok":true,"nugget_count":4,"nuggets":["Emulsification combines oil and water components","Cheese with fats and proteins mixes with oil","Parmesan aids emulsion with fat and protein","Components reduce surface tension to stabilize"],"notes":"4 nugget example"}'
  ),
  
  # 7-nugget example
  list(
    step_description = "Parisian Gnocchi",
    agent_text = paste(
      "The process of mixing ingredients one at a time in this step for Parisian Gnocchi helps ensure even incorporation and prevents premature thickening or clumping.",
      "Adding mustard, cheese, and eggs gradually allows the mixture to emulsify properly, which is crucial for creating smooth dough without breaking down too quickly.",
      "This method also aids in controlling the texture as each ingredient has different properties—eggs can act as a binder, cheese adds moisture and richness, while mustard contributes flavor and helps with consistency.",
      "Finally, adding herbs last ensures they retain their aromatic oils and don’t get diluted early on.",
      "The eggs in this mixture for Parisian Gnocchi primarily serve as an emulsifier and binder.",
      "They help to bind the ingredients together, contributing to a cohesive dough texture.",
      "Additionally, eggs provide moisture and richness, enhancing the overall flavor and mouthfeel of the gnocchi."
    ),
    answer_json = '{"ok":true,"nugget_count":7,"nuggets":["Add ingredients one at a time for even mixing","Gradual addition prevents clumping and helps emulsify","Method controls texture via different ingredient roles","Eggs act as binder","Cheese adds moisture and richness","Mustard adds flavor and consistency","Herbs added last preserve oils and aroma"],"notes":"7 nugget example"}'
  ),
  
  # 6-nugget example
  list(
    step_description = "Roux and Béchamel Sauce",
    agent_text = paste(
      "The science behind making roux and béchamel sauce involves emulsification, thermal properties of fats and proteins, and starch gelatinization.",
      "Roux is a mixture of flour and butter that creates a base for sauces by forming a stable emulsion with liquids like milk or stock.",
      "The flour contains gluten which, when cooked with fat (butter), helps to thicken the sauce without clumping.",
      "Béchamel sauce relies on creating a smooth emulsion between milk and a thickening agent, typically flour slurry.",
      "The science behind making roux and béchamel sauce involves understanding how fats and proteins interact with liquids to create stable emulsions.",
      "Roux uses butter and flour to thicken sauces effectively without lumps by leveraging the thermal properties that cause starch gelatinization, while béchamel relies on a smooth mixture of milk and a thickening agent like flour slurry."
    ),
    answer_json = '{"ok":true,"nugget_count":6,"nuggets":["Roux and béchamel involve emulsification and starch gelatinization","Roux is flour and butter forming stable base with liquid","Gluten with fat thickens without clumping","Béchamel is milk plus flour slurry for smooth emulsion","Fats and proteins interact with liquid to stabilize","Thermal properties and starch gelatinization ensure smoothness"],"notes":"6 nugget example"}'
  ),
  
  # 5-nugget example
  list(
    step_description = "Pesto alla Genovese",
    agent_text = paste(
      "Pounding basil with salt is a key step in making pesto alla Genovese.",
      "This traditional Italian sauce hails from Liguria, and the practice likely dates back to ancient times when preserving food was essential.",
      "By grinding the basil finely with salt, you release its aromatic oils and create a vibrant green paste that forms the base of the sauce.",
      "The salt not only seasons but also helps in emulsifying the olive oil later on, ensuring a cohesive texture.",
      "Additionally, the salt acts as a preservative by drawing out moisture and inhibiting bacterial growth, which helps in preserving the sauce traditionally made without refrigeration."
    ),
    answer_json = '{"ok":true,"nugget_count":5,"nuggets":["Pound basil with salt for pesto base","Originates from Liguria with ancient roots","Grinding basil releases oils for green paste","Salt seasons and helps emulsify oil","Salt preserves by drawing moisture and inhibiting bacteria"],"notes":"5 nugget example"}'
  )
  
  
  # 9-NUGGET (very dense facts to encourage higher counts)
  # Goal: improve recall in 4+ bin and reduce undercounting on dense text
#  list(
#    step_description = "Whole roast chicken workflow",
#    agent_text = paste(
#      "Pat the chicken dry so the skin browns instead of steaming.",
#      "Salt the bird all over at least 1 hour ahead to dry-brine.",
#      "Tie the legs loosely to promote even cooking without compressing the cavity.",
#      "Start roasting at high heat to jump-start browning.",
#      "Reduce to moderate heat to cook the interior through without burning the skin.",
#      "Roast on a rack so hot air can circulate under the bird.",
#      "Probe the thickest part of the breast to 63–65°C and thigh to ~74°C.",
#      "Rest 10–15 minutes so juices redistribute and the skin stays crisp.",
#      "Carve against the grain and return resting juices to the platter."
 #   ),
#    answer_json = '{"ok":true,"nugget_count":9,"nuggets":["Pat chicken dry to enable browning","Salt in advance to dry-brine","Tie legs loosely for even cooking","Begin at high heat to start browning","Lower to moderate heat to finish without burning skin","Roast on a rack for air circulation","Target ~63–65°C breast and ~74°C thigh","Rest 10–15 minutes to redistribute juices","Carve against grain and add resting juices"],"notes":""}'
#  )
)



# ---------- Model I/O ----------
clean_model_output <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  x <- gsub("```[jJ][sS][oO][nN]\\s*|```", "", x)
  x <- gsub("<think>[\\s\\S]*?</think>", "", x, perl = TRUE)
  trimws(x)
}
parse_first_json <- function(x) {
  x <- clean_model_output(x)
  i <- regexpr("\\{", x)[1]; if (i == -1) return(NULL)
  open <- 0; end <- nchar(x)
  for (k in seq(i, nchar(x))) {
    ch <- substr(x, k, k)
    if (ch == "{") open <- open + 1
    if (ch == "}") { open <- open - 1; if (open == 0) { end <- k; break } }
  }
  tryCatch(jsonlite::fromJSON(substr(x, i, end)), error = function(e) NULL)
}
query_ollama_json <- function(prompt_text,
                              model = MODEL_NAME,
                              server = OLLAMA_SERVER,
                              temperature = TEMPERATURE,
                              num_ctx = NUM_CTX,
                              num_predict = NUM_PREDICT,
                              timeout_sec = 120) {
  url <- paste0(rtrim(server, "/"), "/api/generate")
  body <- list(
    model   = model,
    prompt  = prompt_text,
    stream  = FALSE,
    format  = "json",
    options = list(
      temperature = temperature,
      num_ctx     = num_ctx,
      num_predict = num_predict
    )
  )
  resp <- httr::POST(url,
                     body = jsonlite::toJSON(body, auto_unbox = TRUE),
                     encode = "json",
                     httr::timeout(timeout_sec))
  if (httr::http_error(resp)) {
    stop("Ollama HTTP error ", httr::status_code(resp), " at ", url, "\n",
         httr::content(resp, as = "text", encoding = "UTF-8"))
  }
  httr::content(resp)$response
}

# ---------- Filtering ----------
is_bad_nugget <- function(s) {
  s <- tolower(trimws(s))
  if (!nzchar(s)) return(TRUE)
  meta <- c("\\buser\\b","\\bassistant\\b","\\bagent\\b","\\bconfus(ed|ion)\\b",
            "\\bunsure\\b","\\bapolog(y|ise|ize)\\b","\\bsorry\\b",
            "\\brequest(ed|ing)?\\b","\\bask(ed|ing)?\\b",
            "\\bsummar(y|ise|ize)\\b","\\bsummary\\b",
            "\\bthis step\\b","\\bthe step\\b","\\bthe recipe\\b")
  if (any(grepl(paste(meta, collapse="|"), s))) return(TRUE)
  if (nchar(s) < 8) return(TRUE)
  if (grepl("^(mix|slice|do|go|next|continue)$", s)) return(TRUE)
  FALSE
}
filter_nuggets <- function(vec) {
  vec <- unique(trimws(vec))
  vec <- vec[nzchar(vec)]
  vec[!vapply(vec, is_bad_nugget, logical(1))]
}

# ---------- Prompt (lean, strict, shots) ----------
build_nugget_prompt <- function(agent_text, step_description = "", shots = NULL) {
  step_block <- if (nzchar(step_description)) paste0("STEP DESCRIPTION:\n", step_description, "\n\n") else ""
  shots_block <- ""
  if (!is.null(shots) && length(shots)) {
    fmt <- function(s) {
      sd <- s$step_description %||% ""
      sd_block <- if (nzchar(sd)) paste0("STEP DESCRIPTION (EXAMPLE):\n", sd, "\n") else ""
      paste0("EXAMPLE:\n", sd_block,
             "AGENT TEXT (EXAMPLE):\n", s$agent_text, "\n",
             "EXPECTED OUTPUT (single-line JSON):\n", s$answer_json, "\n\n")
    }
    shots_block <- paste0("FEW-SHOT EXAMPLES (match this style):\n",
                          paste(vapply(shots, fmt, character(1)), collapse = ""),
                          "END EXAMPLES.\n\n")
  }
  policy <- paste(
    "You are an expert annotator of INFORMATION NUGGETS.",
    "A nugget is a minimal, self-contained, factual unit drawn ONLY from the AGENT TEXT for THIS STEP.",
    "",
    "RETURN ALL DISTINCT FACTS:",
    "- Some steps have 0 nuggets; others may have 6–8 or more. List EVERY distinct fact you find.",
    "- Do NOT stop early or cap the number. If there are many facts, include them all.",
    "",
    "SEPARATE VS. MERGE:",
    "- Separate distinct facts even if they are short or appear in the same sentence.",
    "- Do NOT split a single fact into paraphrases. Paraphrases of the SAME fact must be merged as ONE nugget.",
    "",
    "EXCLUSIONS:",
    "- Drop meta/affect (apologies, capability comments, requests, uncertainty).",
    "- Drop vague statements that add no concrete information.",
    "",
    "OUTPUT FORMAT:",
    "- Output exactly ONE JSON object on ONE line (no code fences, no <think>, no commentary).",
    "- Schema: { \"ok\": true, \"nugget_count\": <int>, \"nuggets\": [\"<n1>\", ...], \"notes\": \"<optional>\" }",
    "- nugget_count MUST equal length(nuggets). nuggets MAY be empty [].",
    sep = "\n"
  )
  
  
  
  schema <- paste(
    "Output exactly ONE JSON object on ONE line (no code fences, no <think>, no extra text).",
    "Schema:",
    "{ \"ok\": true, \"nugget_count\": <int>, \"nuggets\": [\"<n1>\", ...], \"notes\": \"<optional>\" }",
    "- nugget_count MUST equal length(nuggets)",
    "- nuggets MAY be empty []",
    sep = "\n"
  )
  paste0(policy, "\n\n",
         "GOOD EXAMPLES:\n",
         "- \"Bake until the center reaches 195°F.\"\n",
         "- \"Add oil gradually to stabilize the emulsion.\"\n",
         "- \"Eggs act as a binder in this dough.\"\n\n",
         "BAD EXAMPLES:\n",
         "- Apologies/meta about capabilities.\n",
         "- Vague restatements without concrete new info.\n",
         "- Duplicate paraphrases of the same fact.\n\n",
         schema, "\n\n",
         shots_block,
         step_block,
         "AGENT TEXT:\n", agent_text, "\n\n",
         "OUTPUT: (JSON on one line)")
}

# ---------- Single & repeated extraction ----------
extract_once <- function(agent_text, step_description = "") {
  prompt <- build_nugget_prompt(agent_text, step_description, shots = shots)
  raw <- tryCatch(query_ollama_json(prompt), error = function(e) NA_character_)
  if (is.na(raw)) return(list(ok=FALSE, nuggets=character(0), notes="ollama_error"))
  parsed <- parse_first_json(raw)
  if (is.null(parsed)) return(list(ok=FALSE, nuggets=character(0), notes="invalid_json"))
  nlist <- parsed$nuggets %||% character(0)
  nlist <- filter_nuggets(nlist)
  list(ok = isTRUE(parsed$ok), nuggets = nlist, notes = as_chr1(parsed$notes, ""))
}
extract_repeats <- function(agent_text, step_description = "", repeats = REPEATS) {
  if (!nzchar(trimws(as_chr1(agent_text, "")))) {
    return(tibble::tibble(
      run1_count=0L, run2_count=0L, run3_count=0L,
      run1_nuggets="", run2_nuggets="", run3_nuggets="",
      mean_count=0, sd_count=0, min_count=0L, max_count=0L
    ))
  }
  reps <- vector("list", repeats)
  for (i in seq_len(repeats)) reps[[i]] <- extract_once(agent_text, step_description)
  counts <- vapply(reps, function(r) length(r$nuggets), integer(1))
  nugstr <- vapply(reps, function(r) paste(r$nuggets, collapse = " | "), character(1))
  counts3 <- c(counts, rep(NA_integer_, max(0, 3L - length(counts))))
  nugstr3 <- c(nugstr, rep("",            max(0, 3L - length(nugstr))))
  tibble::tibble(
    run1_count = counts3[1], run2_count = counts3[2], run3_count = counts3[3],
    run1_nuggets = nugstr3[1], run2_nuggets = nugstr3[2], run3_nuggets = nugstr3[3],
    mean_count = safe_mean(counts), sd_count = safe_sd(counts),
    min_count = min(counts, na.rm = TRUE), max_count = max(counts, na.rm = TRUE)
  )
}

# ---------- Load test set ----------
if (!file.exists(INPUT_CSV)) stop("Input CSV not found: ", INPUT_CSV)
message("Loading: ", INPUT_CSV)
test.set <- readr::read_csv(INPUT_CSV, show_col_types = FALSE)

need <- c("conversation_id","recipe_title","step_id","combined_agent_text")
miss <- setdiff(need, names(test.set))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))
test.set <- test.set %>% mutate(row_id = dplyr::row_number())

message("Calling Ollama at ", OLLAMA_SERVER, " | model = ", MODEL_NAME)
message("Repeats per annotation: ", REPEATS)

# ---------- Base-R progress bar ----------
total_rows <- nrow(test.set)
pb <- utils::txtProgressBar(min = 0, max = total_rows, style = 3)
on.exit(try(close(pb), silent = TRUE), add = TRUE)

# ---------- Run model (per-annotation) ----------
pred_list <- vector("list", total_rows)
for (i in seq_len(total_rows)) {
  r <- test.set[i, ]
  pred_list[[i]] <- dplyr::bind_cols(
    r %>% dplyr::select(row_id, conversation_id, recipe_title, step_id, dplyr::any_of("annotator")),
    extract_repeats(agent_text = r$combined_agent_text, step_description = "", repeats = REPEATS)
  )
  if (i == 1L || i %% 10L == 0L || i == total_rows) {
    cat(sprintf("\n[progress] %d/%d\n", i, total_rows))
  }
  utils::setTxtProgressBar(pb, i)
}
pred <- dplyr::bind_rows(pred_list)
readr::write_csv(pred, file.path(OUTDIR, "predictions.csv"))
message("Wrote: ", file.path(OUTDIR, "predictions.csv"))

# ---------- Evaluation (only if human labels present) ----------
if ("nugget_count" %in% names(test.set)) {
  
  # join only on row_id and only bring prediction columns to avoid .x/.y
  pred_cols <- c("row_id","run1_count","run2_count","run3_count",
                 "run1_nuggets","run2_nuggets","run3_nuggets",
                 "mean_count","sd_count","min_count","max_count")
  pred_trim <- pred %>% dplyr::select(dplyr::any_of(pred_cols))
  
  df <- test.set %>%
    dplyr::select(
      row_id, conversation_id, recipe_title, step_id,
      dplyr::any_of(c("annotator","nuggets_marked","combined_agent_text")),
      human_count = nugget_count
    ) %>%
    dplyr::left_join(pred_trim, by = "row_id")
  
  # Quick diagnostics
  cat("\n[eval] rows test.set:", nrow(test.set), " | rows pred:", nrow(pred),
      " | rows joined:", nrow(df), "\n")
  if (anyDuplicated(df$row_id)) {
    warn_dups <- df %>% dplyr::count(row_id) %>% dplyr::filter(n > 1)
    message("[eval] WARNING: duplicated row_id after join: ", nrow(warn_dups), " rows")
  }
  
  # Ensure numeric
  num_fix <- function(x) suppressWarnings(as.numeric(x))
  df <- df %>%
    mutate(
      mean_count = num_fix(mean_count),
      sd_count   = num_fix(sd_count),
      run1_count = num_fix(run1_count),
      run2_count = num_fix(run2_count),
      run3_count = num_fix(run3_count)
    ) %>%
    tidyr::replace_na(list(mean_count = 0, sd_count = 0))
  
  # Sanity distribution
  dist_tbl <- df %>% count(human_count) %>% arrange(human_count)
  print(dist_tbl, n = Inf)
  readr::write_csv(df, file.path(OUTDIR, "eval_joined.csv"))
  
  # Overall metrics
  df <- df %>% mutate(error = mean_count - human_count)
  overall <- df %>%
    summarise(
      n        = dplyr::n(),
      pearson  = suppressWarnings(cor(human_count, mean_count, use="complete.obs")),
      spearman = suppressWarnings(cor(human_count, mean_count, method="spearman", use="complete.obs")),
      mae      = mean(abs(error), na.rm=TRUE),
      bias     = mean(error, na.rm=TRUE),
      stability_sd = mean(sd_count, na.rm=TRUE)
    )
  print(overall)
  readr::write_csv(overall, file.path(OUTDIR, "eval_overall.csv"))
  
  # Bins: 0, 1, 2–3, 4+
  bin_of <- function(x){
    dplyr::case_when(
      x == 0 ~ "0",
      x == 1 ~ "1",
      x %in% 2:3 ~ "2-3",
      x >= 4 ~ "4+"
    )
  }
  by_bins <- df %>%
    mutate(bin = bin_of(human_count)) %>%
    group_by(bin) %>%
    summarise(
      n        = dplyr::n(),
      pearson  = suppressWarnings(cor(human_count, mean_count, use="complete.obs")),
      spearman = suppressWarnings(cor(human_count, mean_count, method="spearman", use="complete.obs")),
      mae      = mean(abs(error), na.rm=TRUE),
      bias     = mean(error, na.rm=TRUE),
      stability_sd = mean(sd_count, na.rm=TRUE),
      .groups  = "drop"
    ) %>%
    arrange(factor(bin, levels=c("0","1","2-3","4+")))
  print(by_bins)
  readr::write_csv(by_bins, file.path(OUTDIR, "eval_by_bins.csv"))
  
  # Exact strata
  by_exact <- df %>%
    group_by(human_count) %>%
    summarise(
      n        = dplyr::n(),
      mae      = mean(abs(mean_count - human_count), na.rm=TRUE),
      bias     = mean(mean_count - human_count, na.rm=TRUE),
      stability_sd = mean(sd_count, na.rm=TRUE),
      .groups  = "drop"
    ) %>% arrange(human_count)
  print(by_exact, n = Inf)
  readr::write_csv(by_exact, file.path(OUTDIR, "eval_by_exact.csv"))
  
  # Classification: has-any & high-density (>=4)
  cls_any  <- df %>% mutate(gold = human_count > 0, pred = mean_count > 0)
  tab_any  <- table(gold=cls_any$gold, pred=cls_any$pred)
  prec_any <- if ("TRUE" %in% colnames(tab_any)) tab_any["TRUE","TRUE"]/sum(tab_any[,"TRUE"]) else NA_real_
  rec_any  <- if ("TRUE" %in% rownames(tab_any)) tab_any["TRUE","TRUE"]/sum(tab_any["TRUE",]) else NA_real_
  
  cls_high  <- df %>% mutate(gold = human_count >= 4, pred = mean_count >= 4)
  tab_high  <- table(gold=cls_high$gold, pred=cls_high$pred)
  prec_high <- if ("TRUE" %in% colnames(tab_high)) tab_high["TRUE","TRUE"]/sum(tab_high[,"TRUE"]) else NA_real_
  rec_high  <- if ("TRUE" %in% rownames(tab_high)) tab_high["TRUE","TRUE"]/sum(tab_high["TRUE",]) else NA_real_
  
  misc <- tibble::tibble(
    has_any_precision = prec_any, has_any_recall = rec_any,
    high_precision    = prec_high, high_recall   = rec_high
  )
  print(misc)
  readr::write_csv(misc, file.path(OUTDIR, "eval_classification.csv"))
  
  # Top disagreements
  top_abs <- df %>%
    mutate(abs_err = abs(error)) %>%
    arrange(desc(abs_err)) %>%
    dplyr::select(conversation_id, recipe_title, step_id, dplyr::any_of("annotator"),
                  human_count, mean_count, abs_err,
                  dplyr::any_of(c("nuggets_marked","run1_nuggets","run2_nuggets","run3_nuggets","combined_agent_text"))) %>%
    slice(1:30)
  readr::write_csv(top_abs, file.path(OUTDIR, "top_disagreements.csv"))
  
  # --- Duplicate / near-duplicate nuggets (vectorized + strict) ---
  
  normalize_nug <- function(s) {
    s <- tolower(s)
    s <- stringr::str_replace_all(s, "[^a-z0-9\\s]", " ")
    stringr::str_squish(s)
  }
  
  # scalar: handle one cell (string) -> NA if <2 nuggets; else unique/total
  dedup_ratio_one <- function(nug_str) {
    s <- as.character(nug_str)[1]
    if (!nzchar(s) || is.na(s)) return(NA_real_)
    items <- strsplit(s, "\\s*\\|\\s*")[[1]]
    items <- trimws(items)
    items <- items[nzchar(items)]
    if (length(items) < 2) return(NA_real_)
    norm <- vapply(items, normalize_nug, character(1))
    length(unique(norm)) / length(norm)
  }
  
  # vectorized wrapper for a whole column
  dedup_ratio_vec <- function(col) vapply(as.character(col), dedup_ratio_one, numeric(1))
  
  dup_eval <- df %>%
    mutate(
      run1_ratio = dedup_ratio_vec(run1_nuggets),
      run2_ratio = dedup_ratio_vec(run2_nuggets),
      run3_ratio = dedup_ratio_vec(run3_nuggets)
    ) %>%
    mutate(
      any_multi = rowSums(cbind(is.finite(run1_ratio),
                                is.finite(run2_ratio),
                                is.finite(run3_ratio))) > 0,
      any_dup   = ( (run1_ratio < 1) | (run2_ratio < 1) | (run3_ratio < 1) ),
      mean_ratio = rowMeans(cbind(run1_ratio, run2_ratio, run3_ratio), na.rm = TRUE)
    ) %>%
    dplyr::select(
      dplyr::any_of(c("conversation_id","recipe_title","step_id","annotator")),
      run1_ratio, run2_ratio, run3_ratio, mean_ratio, any_multi, any_dup
    )
  
  dup_summary <- dup_eval %>%
    summarise(
      n = dplyr::n(),
      n_with_multi = sum(any_multi, na.rm = TRUE),
      p_any_dup_among_multi = ifelse(n_with_multi > 0,
                                     mean(any_dup[any_multi], na.rm = TRUE),
                                     NA_real_),
      mean_ratio_among_multi = ifelse(n_with_multi > 0,
                                      mean(mean_ratio[any_multi], na.rm = TRUE),
                                      NA_real_)
    )
  
  cat("\n=== DUPLICATE RATIO SUMMARY (runs with ≥2 nuggets) ===\n")
  print(dup_summary)
  readr::write_csv(dup_eval,    file.path(OUTDIR, "duplicate_ratio_per_row.csv"))
  readr::write_csv(dup_summary, file.path(OUTDIR, "duplicate_ratio_summary.csv"))
  
  library(irr)
  
  # --- 1. Stability summary ---
  stab_summary <- pred %>%
    summarise(
      mean_sd   = mean(sd_count, na.rm = TRUE),
      median_sd = median(sd_count, na.rm = TRUE),
      max_sd    = max(sd_count, na.rm = TRUE)
    )
  print(stab_summary)
  
  # --- 2. Pairwise run correlations ---
  runs <- pred %>% select(run1_count, run2_count, run3_count)
  cor_mat <- cor(runs, use = "pairwise.complete.obs")
  print(cor_mat)
  
  # --- 3. Intraclass correlation ---
  icc_res <- irr::icc(runs, model = "twoway", type = "consistency", unit = "average")
  print(icc_res)
}  


