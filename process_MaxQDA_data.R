########
# This script takes data outputted by MaxQDA, processes the meta-data highlighted in the annotations, and calculates an inter-rater reliability score.
########
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(irr)
library(tibble)
library(purrr)
library(ggplot2)
library(Rtsne)
library(ggrepel)
library(cluster)

# 1. Load clean CSVs
christine.data <- read.csv2("Annotated_conversations/1st_round_IRR_test/Cooking_with_conversation_random_coded_Christine.csv")
anna.data <- read.csv2("Annotated_conversations/1st_round_IRR_test/Cooking_with_conversation_random_coded_Anna.csv")

christine.data$Modified.by='C'
anna.data$Modified.by='A'

annotations <- rbind(anna.data,christine.data)
nrow(annotations)

# 2. Standardize column names
names(annotations) <- tolower(names(annotations))

# 3. Keep relevant columns and clean
annotations.raw <- annotations %>%
  select(code, segment, modified.by)

# 4. Extract meta-data from [key=value] format in Segment

annotations <- annotations.raw %>%
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
meta_keys <- unique(unlist(lapply(annotations$meta_pairs, names)))

# Populate metadata columns manually
for (key in meta_keys) {
  annotations[[key]] <- sapply(annotations$meta_pairs, function(x) x[[key]])
}


# 5. Clean and prepare columns
annotations <- annotations %>%
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

names(annotations) <- tolower(names(annotations))
n_before <- nrow(annotations.raw)
n_after <- nrow(annotations)  # After the full mutate + unnest_wider
n_before
n_after

#####
# Do sanity checks on the data and re-create the agreement table from MaxQDA
######
names(annotations) <- tolower(names(annotations))
# let's get an overview of the codes

annotations %>%
  count(code, modified.by) %>%
  arrange(desc(n))

# sanity check for 1 code
annotations %>%
  filter(code == "Acknowledge response (user)") %>%
  mutate(coder = ifelse(grepl("^a", tolower(modified.by)), "A", "C")) %>%
  mutate(utterance_id = paste(conversation_id, conversation.id, utterance, sep = "|")) %>%
  count(utterance_id, coder)

# How many unique utterance-code pairs per coder?
annotations %>%
  mutate(utterance_id = paste(conversation_id, conversation.id, utterance, sep = "|")) %>%
  distinct(modified.by, code, utterance_id) %>%
  count(modified.by, name = "n_utterances_coded")

#Check for duplicates or overwritten codes in the pipeline
annotations %>%
  mutate(utterance_id = paste(conversation_id, conversation.id, utterance, sep = "|")) %>%
  count(modified.by, utterance_id, code) %>%
  filter(n > 1)

# passed all the sanity checks 

##
# Kappa 
##

# Step 1: Extract utterance_id and code per coder
utterance_code <- annotations %>%
  mutate(
    utterance_id = paste(conversation_id, conversation.id, sep = "|"),
    coder = ifelse(tolower(modified.by) == "a", "A", "C")
  ) %>%
  distinct(utterance_id, code, coder)

# Step 2: Count annotations per code and coder
per_coder_counts <- utterance_code %>%
  count(code, coder, name = "n_coder")

# Step 3: Count how often both coders applied the same code to the same utterance
agreement_counts <- utterance_code %>%
  group_by(code, utterance_id) %>%
  summarise(n_coders = n_distinct(coder), .groups = "drop") %>%
  filter(n_coders == 2) %>%
  count(code, name = "n_both")

# Step 4: Merge and calculate agreement statistics
agreement_table <- per_coder_counts %>%
  tidyr::pivot_wider(names_from = coder, values_from = n_coder, values_fill = 0) %>%
  left_join(agreement_counts, by = "code") %>%
  mutate(
    n_both = replace_na(n_both, 0),
    agreement = 2 * n_both,
    disagreement = (A - n_both) + (C - n_both),
    total = A + C,
    percent_agreement = round(100 * agreement / total, 1)
  ) %>%
  select(code, agreement, disagreement, total, percent_agreement) %>%
  arrange(desc(percent_agreement))

agreement_table

##########
# Calculate the Krippendorf's alpha
#########

# Get all unique utterance_ids
all_utterances <- annotations %>%
  mutate(utterance_id = paste(conversation_id, conversation.id, sep = "|")) %>%
  distinct(utterance_id)

# Prepare alpha calculation across full set of utterances
unique_codes <- unique(annotations$code)

results <- lapply(unique_codes, function(code_name) {
  # Mark where each coder applied the code
  coded <- annotations %>%
    filter(code == code_name) %>%
    mutate(
      utterance_id = paste(conversation_id, conversation.id, sep = "|"),
      coder = ifelse(tolower(modified.by) == "a", "A", "C")
    ) %>%
    select(utterance_id, coder) %>%
    mutate(value = 1)
  
  # Join with full utterance set, fill in 0s where not coded
  full_data <- expand.grid(
    utterance_id = all_utterances$utterance_id,
    coder = c("A", "C")
  ) %>%
    left_join(coded, by = c("utterance_id", "coder")) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    pivot_wider(names_from = coder, values_from = value)
  
  mat <- t(as.matrix(full_data[, c("A", "C")]))
  
  # Only compute alpha if more than one utterance
  if (ncol(mat) < 2) {
    alpha <- NA_real_
  } else {
    alpha <- kripp.alpha(mat, method = "nominal")$value
  }
  
  tibble(
    code = code_name,
    alpha = alpha,
    n_utterances = nrow(full_data)
  )
}) %>%
  bind_rows() %>%
  arrange(desc(n_utterances))
print (results)
print ("We computed Krippendorff’s alpha using the full set of utterances to account for agreement on both application and non-application of codes. The resulting reliability estimates ranged from 0.39 to 1.0, with 15 of 20 codes exceeding 0.65.")
print (results %>%
  filter(!grepl("\\(agent\\)", code)))
print ("We further analyse the 10 user codes which had on average much higher inter-rater reliability. 9 of the codes reached Krippendorff’s alpha values above 0.6, indicating substantial to perfect agreement, with several codes such as *Compliment/praise/face-enhancing feedback (user)*, *Politeness marker please (user)*, and *Thanking (user)* achieving perfect agreement (α = 1). The only exception was *Request – direct (user)*, which showed comparatively lower reliability (α = 0.392), suggesting some ambiguity or variation in how this code was applied by different annotators.
  ")

################
# cluster analysis
################

# Anna's_full_annotated_data
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



user_annotations <- anna.data.full %>%
  filter(!grepl("\\(agent\\)", code))

user_code_matrix <- user_annotations %>%
  mutate(user = conversation_id) %>%
  count(user, code) %>%
  pivot_wider(
    names_from = code,
    values_from = n,
    values_fill = list(n = 0)
  ) %>%
  filter(user != "NULL")

# normalise the counts (think about this is sensible)
user_code_matrix_scaled <- user_code_matrix %>%
  column_to_rownames("user") %>%
  scale()




#Elbow Method to Choose Number of Clusters
wss <- map_dbl(1:10, function(k) {
  kmeans(user_code_matrix_scaled, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- tibble(k = 1:10, wss = wss)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Elbow Method for Determining Optimal k",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal()


# Compute silhouette width for k = 2 to 10
sil_width <- map_dbl(2:10, function(k) {
  km <- kmeans(user_code_matrix_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(user_code_matrix_scaled))
  mean(ss[, 3])  # average silhouette width
})

# Plot silhouette scores
tibble(k = 2:10, silhouette = sil_width) %>%
  ggplot(aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point() +
  labs(title = "Silhouette Method for Optimal k",
       x = "Number of clusters",
       y = "Average silhouette width") +
  theme_minimal()


# Final Clustering with k = 4
set.seed(1234)
kmeans_result <- kmeans(user_code_matrix_scaled, centers = 4)

user_ids <- rownames(user_code_matrix_scaled)

user_clusters <- tibble(
  user = user_ids,
  cluster = kmeans_result$cluster
)

# Create Human-Readable Labels
user_labels <- tibble(
  user = user_ids,
  label = paste0("u", str_pad(1:length(user_ids), 2, pad = "0"))
)

# Run t-SNE
tsne_input <- as.matrix(user_code_matrix_scaled)

set.seed(4321)
tsne_result <- Rtsne(tsne_input, dims = 2, perplexity = 5, verbose = TRUE, max_iter = 1000)

# Combine All Info
tsne_df <- tsne_result$Y %>%
  as.data.frame() %>%
  mutate(user = user_ids) %>%
  left_join(user_labels, by = "user") %>%
  left_join(user_clusters, by = "user")

# Plot t-SNE with Clusters and Labels
ggplot(tsne_df, aes(x = V1, y = V2, label = label, color = factor(cluster))) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    title = "t-SNE of Users Based on Politeness Code Usage",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Cluster"
  ) +
  theme_minimal()

write.csv(user_code_matrix_scaled,"Anna_full_annotated_data/user_code_matrix_scaled.csv")

## analysing clusters

# 1. Convert rownames (user IDs) to a column so we can join
user_code_scaled_df <- user_code_matrix_scaled %>%
  as.data.frame() %>%
  rownames_to_column("user")


# 2. Join with cluster assignments
user_code_with_cluster <- user_code_scaled_df %>%
  left_join(user_clusters, by = "user")

# 3. Pivot to long format for plotting and aggregation
user_code_long <- user_code_with_cluster %>%
  pivot_longer(-c(user, cluster), names_to = "code", values_to = "scaled_count")

# 4. Compute mean standardized count per code per cluster
cluster_code_means <- user_code_long %>%
  group_by(cluster, code) %>%
  summarise(mean_scaled = mean(scaled_count), .groups = "drop")

# 5. Plot: Which codes are most characteristic of which cluster?
ggplot(cluster_code_means, aes(x = reorder(code, mean_scaled), y = mean_scaled, fill = factor(cluster))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Mean Standardized Frequency of Politeness Codes per Cluster",
    x = "Politeness Code",
    y = "Mean (Standardized) Usage",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 12)

# Find codes with the greatest variance across clusters (i.e., distinctive codes)
top_discriminating_codes <- cluster_code_means %>%
  group_by(code) %>%
  summarise(variance = var(mean_scaled)) %>%
  arrange(desc(variance))

# Show top 10
head(top_discriminating_codes, 10)


