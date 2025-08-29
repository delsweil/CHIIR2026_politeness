install.packages("jsonlite")
install.packages("readr")      # for writing CSV
install.packages("writexl")    # for writing Excel files

library(jsonlite)
library(readr)
library(writexl)

# Loop through conversation_0.json to conversation_29.json
for (i in 0:29) {
  # Construct filename
  json_filename <- paste0("JASON_files/conversation_", i, ".json")
  txt_filename  <- paste0("conversation_", i, "_annotatable.txt")
  
  # Safely read JSON file
  if (file.exists(json_filename)) {
    json_data <- fromJSON(json_filename, flatten = TRUE)
    df <- as.data.frame(json_data)
    
    # Check necessary columns exist
    required_cols <- c("conversation_id", "conversation.id", "conversation.participant", "conversation.utterance")
    if (all(required_cols %in% colnames(df))) {
      
      # Format each utterance into text with inline metadata
      formatted_lines <- mapply(function(conversation_id, conversation.id, conversation.participant, conversation.utterance) {
        paste0(
          conversation.utterance, "\n",
          "[conversation_id=", conversation_id, "] ",
          "[conversation.id=", conversation.id, "] ",
          "[conversation.participant=", conversation.participant, "]\n"
        )
      }, df$conversation_id, df$conversation.id, df$conversation.participant, df$conversation.utterance)
      
      # Write to text file
      writeLines(formatted_lines, txt_filename)
      message("Written: ", txt_filename)
      
    } else {
      warning("Missing expected columns in: ", json_filename)
    }
  } else {
    warning("File not found: ", json_filename)
  }
}
 