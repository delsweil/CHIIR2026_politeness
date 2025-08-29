library(httr)
library(jsonlite)
library(dplyr)

url = "http://hcai.uni-regensburg.de:11434/api/generate"
url = "http://intern.schlaubox.de:11434/api/generate"
url = "http://localhost:11434/api/generate"

model = "deepseek-r1:7b"
model= "deepseek-r1:32b"
# Set your prompt
compliment <- "A compliment is a speech act which explicitly or implicitly attributes credit to someone other than the speaker, usually the person addressed, for some ‘good’ (possession, characteristic, skill, etc.) which is positively valued by the speaker and the hearer” (Holmes 1988: 446, qtd. in Leech 2014: 186)
Compliments/praise are considered synonymous in many speech act studies (Meierkord 2023: 3). Meierkord stresses the interactive nature of complimenting in her own study and points out that praise “lack[s] the interactional component attributed to compliments, that is, there is
no second pair part in the form of an uptake of the praise”. (Meierkord 2023: 4).
Compliment speech events may appear in the following patterns (adapted from Wolfson 1989, qtd. in Leech 2014: 187):
(a)	NP is/looks (intensifier) ADJ
-	E.g. You look really lovely.
-	E.g. This is lovely!
-	E.g. That’s beautiful on you, that’s the one to have.
(b)	I (intensifier) like/love NP
-	E.g. I really like your accent.
(c)	PRO is (intensifier) a ADJ NOUN
-	E.g. That’s a terrific idea.
-	E.g. that’s a good color.
(d)	Possible modifications (Leech 2014: 188)  Extra emotive emphasis may be added through exclamatory questions (e.g. Aren’t you good!), interjections (e.g. Oh), additional adverbials (e.g. That’s beautiful on you)
(e)	Praise formulae/expressions found in the data set: well done; congratulations

Given the following utterance, is the following an example of compliment or praise? End your response with \"Compliment:T\" if you think the utterance contains either an compliment or praise. End with \"Compliment:F\" if you think neither a compliment or praise is present"

##########

Request.indirect <- "A request is a directive speech act in which the speaker tries to get the hearer to do something. It is aimed at influencing the hearer's future actions, typically for the benefit of the speaker. A request must express that the speaker wants the hearer to perform an action — either explicitly or implicitly — and not merely make a suggestion, offer, or statement of preference.\n\nOn-record indirect strategies are directive speech acts that do not issue commands directly but instead rely on inference to convey the speaker’s intention. They are still \"on record\" (i.e., not hidden or implied) but avoid directly imposing the speaker’s will.\n\nThese strategies can appear as:\n\nA. Statements\nThese use modally loaded language from which the directive (requestive) meaning must be inferred.\n\nTypes (in order of increasing politeness):\n\n1. Prediction statements\n- Use: will\n- Example: You will just fill out that work ticket.\n\n2. Strong obligation statements\n- Use: must, have (got) to\n- Example: You must record testing times for all three tests.\n\n3. Weaker obligation statements\n- Use: should, need to\n- Example: You should give me all your old clothes, Lisa.\n\n4. Volitional statements\n- Speaker is subject\n- Use: I want (you to), I would like (you to)\n- Example: I want you to bend your knees.\n\n5. Ability/possibility statements\n- Use: can, could, may, might\n- Example: You can hang out with my mom for a while.\n\nB. Questions\nThese express a request by asking a question, which appears to give the addressee more control, thus making them more polite.\n\nTypes:\n\n1. Volitional questions\n- Ask about the hearer’s willingness\n- Use: Will you, Would you, Would you like to, Are you willing to, Would you be prepared to\n- Example: Would you deal with another matter please, your worships?\n\n2. Ability/possibility questions\n- Ask about the hearer’s ability or the feasibility of an action\n- Use: Can you, Could you, Are you able to, Is it possible (for you) to\n- Example: Is it possible, Larry, to send me a copy of that?\n\nGiven the following utterance, does it contain a request in the form of an on-record indirect strategy as defined above? Only mark the utterance as indirect if it clearly expresses a request for the hearer to do something, even if the request is phrased indirectly.\nEnd your response with \"Request - indirect:T\" if the utterance contains such a request.\nEnd with \"Request - indirect:F\" if it does not."
Apology.IFID <- "An apology is a speech act in which the speaker acknowledges a fault, social infraction, or offense, and attempts to redress it. The core of an apology is the apology itself — the 'head act' — which contains an illocutionary force indicating device (IFID).\n\nCommon IFID expressions include: (I'm) sorry; sorry about / to / that...; I beg your pardon; I apologize; pardon me; excuse me; forgive me; pardon; (my) apologies. These may be intensified (e.g., very sorry, really sorry) or accompanied by internal or external modifiers (e.g., Oh dear; John).\n\nSupporting elements like explanations, offers of repair, or promises to do better in the future may accompany the IFID, but **they do not in themselves constitute an apology** unless an explicit IFID is present.\n\nRoutine or formulaic apologies (e.g., saying 'sorry' after a minor social gaffe) also count as apologies as long as an IFID is used.\n\nImportant: Do not classify a statement as an apology unless it clearly contains an IFID — such as 'sorry', 'I apologize', 'excuse me', etc.\n\nGiven the following utterance, does it contain an **explicit apology** with a recognizable IFID, as described above?\nEnd your response with \"Apology - IFID:T\" if the utterance contains an IFID.\nEnd with \"Apology - IFID:F\" if it does not."
Request.direct <- "A request is a directive speech act in which the speaker tries to get the hearer to do something. It is aimed at influencing the hearer's future actions, typically for the benefit of the speaker. A request must express that the speaker wants the hearer to perform an action — either explicitly or implicitly — and not merely make a suggestion, offer, or statement of preference. Bald-on-record direct strategies are directive speech acts that “convey meaning directly, without any device to reduce the face threat” (Leech 2014: 147). Typically, there are therefore no or few mitigation devices such as hedging through conditional and hypothetical forms such as “would” and “could” or adverbs such as “maybe”. Direct requests can be realised through imperatives, where the imperative clause acts as illocutionary force indicating device. Example: “Bring me a cup of tea”. Direct requests can also be formally realised through performative verbs such as “ask” or “beg”. Examples include: “I beg you to help me”; “I am asking you to listen”. Given the following utterance, does it contain a request in the form of a bald-on-record direct strategy as defined above? End your response with \"Request - direct:T\" if the utterance contains such a request.\nEnd with \"Request - direct:F\" if it does not."
Request.non-sentential <- "A request is a directive speech act in which the speaker tries to get the hearer to do something. It is aimed at influencing the hearer's future actions, typically for the benefit of the speaker. A request must express that the speaker wants the hearer to perform an action — either explicitly or implicitly — and not merely make a suggestion, offer, or statement of preference. A non-sentential request is not a full sentential utterance, as it is clear from the context what kind of request is being made. It may include indicators of politeness such as “please” and honorific vocatives. Examples: “Passport?”; “Another cup, please”; “Two chocolate brownies and a multigrain loaf please”; “Next?”; “Tickets please”; “A return to Glasgow please”. Given the following utterance, does it contain a request in the form of a non-sentential strategy as defined above? End your response with \"Request - non-sentential:T\” if the utterance contains such a request.\nEnd with \"Request - non-sentential:F\” if it does not."
Absolving.acceptance <- "An apology is a speech act in which the speaker acknowledges a fault, social infraction, or offense, and attempts to redress it. Apologies typically evoke either rejection or acceptance by the hearer, though the polite option is usually preferred: “an absolving speech event by which S is excused, or pardoned, for the offense” (Leech 2014: 130). Such an absolving speech event may take of an acceptance, signalling the apology as sufficient recompense for the fault. Acceptance expressions include, e.g. That’s all right; It’s all right; OK. Given the following utterance, does it contain an absolving speech event in the form of an acceptance as defined above? Only mark the utterance as absolving acceptance if it is preceded by an apology.\nEnd your response with \”Absolving - acceptance:T\” if the utterance contains such an absolving event .\nEnd with \”Absolving - acceptance:F\” if it does not."
Compliment.praise.face-enhancing.feedback <- "A compliment is a speech act which explicitly or implicitly attributes credit to someone other than the speaker, usually the person addressed, for some ‘good’ (possession, characteristic, skill, etc.) which is positively valued by the speaker and the hearer’ (Holmes 1988: 446, qtd. in Leech 2014: 186) Compliments/praise are considered synonymous in many speech act studies (Meierkord 2023: 3). Meierkord stresses the interactive nature of complimenting in her own study and points out that praise ‘lack[s] the interactional component attributed to compliments, that is, there is no second pair part in the form of an uptake of the praise’. (Meierkord 2023: 4). In the Cooking_with_conversation dataset, we observe a continuum ranging from explicit praising and complimenting (e.g., Well done!) to utterances that serve to enhance the interlocutors face by, e.g., pointing out the usefulness of the information given or that they appreciate the information given (e.g., This is really useful!; I will remember this for the next time I make soup.). Face-enhancing feedback includes utterances positively commenting on the cooking process, e.g., okay great; it looks a lovely colour; Yum yum. Sounds and looks fantastic. Given the following utterance, does it contain a complimenting-praising-face-enhancing speech event as defined above? .\nEnd your response with \”Compliment-praise-face-enhancing feedback:T\” if the utterance contains such a face-enhancing speech event .\nEnd with \”Compliment-praise-face-enhancing feedback:F\” if it does not."
Acknowledge.response <- "In the cooking_with_conversation data, users regularly ask for information and the agent complies with these requests. The users may, in turn, verbally acknowledge the agent’s response. Examples include: “Ah, ok”; “I see”; “I understand”. Sometimes, users add face-enhancing utterances expressing the usefulness of the offered information (= positive politeness). Example: I see. I will remember this for the next time I make a sauce. If such a face-enhancing utterance is added, this is coded separately as “Compliment/praise/face-enhancing feedback” in addition to the utterance being coded as “Acknowledge response”. Given the following utterance, does it contain an acknowledgement of a response as defined above? .\nEnd your response with \”acknowledge response:T\” if the utterance contains such an acknowledgement .\nEnd with \”acknowledge response:F\” if it does not."
Hint.statement <- "A request is a directive speech act in which the speaker tries to get the hearer to do something. It is aimed at influencing the hearer's future actions, typically for the benefit of the speaker. A request must express that the speaker wants the hearer to perform an action — either explicitly or implicitly — and not merely make a suggestion, offer, or statement of preference. Requests may take the form of hints. The defining characteristic of hints is “that they do not actually mention the action A that S wants O to carry out, but leave it to be inferred; hence they have been termed hints” (Leech 2014: 158–159). Hints may be realised as statement hints. They “typically refer to the situation, usually in the present, that calls for the future action.“ (Leech 2014: 158) and “they can be obviously impolite, in accusing O of some misdemeanor, or they can more indirectly imply an accusation” (Leech 2014: 158). Moreover, “statement hints are not particularly polite, and to avoid face threat they often need to be embellished with some show of reluctance to bother the other person” (Leech 2014: 158). Example: “I’m sorry, I can’t see the screen” (said to someone standing in one’s line of vision). Given the following utterance, does it contain a request in the form of a statement hint as defined above? End your response with \”Hint - statement:T\” if the utterance contains such a request.\nEnd with \”Hint - statement:F\” if it does not."





# Wrapper function to send a single request to Ollama
query_ollama <- function(utterance, 
                         model = "deepseek-r1:32b", 
                         base_prompt = Request.direct) {
  
  prompt_text <- paste0(base_prompt, "\n\nUtterance: ", utterance)
  
  body <- list(
    model = model,
    prompt = prompt_text,
    stream = FALSE
  )
  
  res <- httr::POST(
    url = "http://localhost:11434/api/generate",  # ← or "http://localhost:11434"
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )
  
  if (res$status_code != 200) return(NA)
  
  result <- httr::content(res)
  return(result$response)
}




# load in the data 
anna.data.full <- read.csv2("Anna_full_annotated_data/Anna_annotation_01_08_25.csv")

# 2. Standardize column names
names(anna.data.full) <- tolower(names(anna.data.full))

# 3. Keep relevant columns and clean
anna.data.full.raw <- anna.data.full %>%
  select(code, segment, modified.by)

# 4. Extract meta-data from [key=value] format in Segment

anna.data.full <- anna.data.full.raw %>%
  mutate(
    meta_fields = str_extract_all(segment, "\\[(.*?)\\]"),
    meta_pairs = lapply(meta_fields, function(pairs) {
      kv <- str_match(pairs, "\\[(.*?)=(.*?)\\]")
      if (is.null(kv) || nrow(kv) == 0) return(list())
      setNames(kv[, 3], kv[, 2])
    }),
    Utterance = str_trim(str_remove_all(segment, "\\[.*?\\]"))
  )

# Get all possible keys from metadata
meta_keys <- unique(unlist(lapply(anna.data.full$meta_pairs, names)))

# Populate metadata columns manually
for (key in meta_keys) {
  anna.data.full[[key]] <- sapply(anna.data.full$meta_pairs, function(x) x[[key]])
}


# 5. Clean and prepare columns
anna.data.full <- anna.data.full %>%
  select(-segment, -meta_fields) %>%
  mutate(
    #    code = str_remove(code, "\\(user\\)") %>%
    #      str_remove("\\(agent\\)") %>%
    #      str_trim(),
    conversation_id = as.character(conversation_id),
    conversation.id = as.character(conversation.id),
    conversation.participant = as.character(conversation.participant),
    modified.by = as.character(modified.by),
    marker = TRUE
  ) #%>%
#  filter(conversation.participant=='user')

names(anna.data.full) <- tolower(names(anna.data.full))




# Time how long it takes to run through all utterances
elapsed_time <- system.time({
  anna.data.full$request.direct <- sapply(anna.data.full$utterance, query_ollama)
})
anna.data.full$response<- anna.data.full$request.direct
# Print total time
print(elapsed_time)

# create a complement field based on the responses
anna.data.full <- anna.data.full %>%
  mutate(
    cleaned_response = str_remove(request.direct, "[^a-zA-Z0-9]+$"),
    request.direct = case_when(
      is.na(response) ~ NA,
      str_sub(cleaned_response, -1) == "T" ~ TRUE,
      str_sub(cleaned_response, -1) == "F" ~ FALSE,
      TRUE ~ NA
    )
  )

cbind(Test_22_07_25$`c.Apology (IFID)`,Test_22_07_25$d.Apology.IFID)

