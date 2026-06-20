library(jsonlite)
library(dplyr)

json_path <- "F:/IDS/arxiv-metadata-oai-snapshot.json"
output_csv <- "F:/IDS/ids_final_dataset_sample_group_12.csv"

ai_list <- list()
non_ai_list <- list()

ai_pattern <- "cs.AI|cs.LG|cs.CV|cs.CL"

con <- file(json_path, "r")
while (length(ai_list) < 100 || length(non_ai_list) < 100) {
  line <- readLines(con, n = 1)
  if (length(line) == 0) break
  
  obj <- fromJSON(line)
  if (is.null(obj$abstract) || obj$abstract == "") next
  
  row <- data.frame(
    title = obj$title,
    abstract = obj$abstract,
    stringsAsFactors = FALSE
  )
  
  if (grepl(ai_pattern, obj$categories) && length(ai_list) < 100) {
    ai_list[[length(ai_list) + 1]] <- mutate(row, type = "AI")
  }
  
  if (!grepl(ai_pattern, obj$categories) && length(non_ai_list) < 100) {
    non_ai_list[[length(non_ai_list) + 1]] <- mutate(row, type = "Non-AI")
  }
}
close(con)

final_df <- bind_rows(
  bind_rows(ai_list),
  bind_rows(non_ai_list)
)

nrow(final_df)
table(final_df$type)

write.csv(final_df, output_csv, row.names = FALSE)




library(readr)
library(dplyr)
library(tm)
library(cluster)
library(wordcloud)
library(ggplot2)

set.seed(123)

data <- read_csv("F:/ids_final_dataset_sample_group_12.csv", show_col_types = FALSE)
data <- data %>% filter(!is.na(abstract))

ai_data    <- data %>% filter(type == "AI")     %>% sample_n(100)
nonai_data <- data %>% filter(type == "Non-AI") %>% sample_n(100)

all_data   <- rbind(ai_data, nonai_data)
all_labels <- all_data$type



clean_corpus <- function(text){
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus
}
all_corpus <- clean_corpus(all_data$abstract)



dtm <- DocumentTermMatrix(all_corpus)
dtm <- removeSparseTerms(dtm, 0.95)

tfidf <- as.matrix(weightTfIdf(dtm))

normalize <- function(x){
  if(sum(x^2) == 0) return(x)
  x / sqrt(sum(x^2))
}

tfidf_norm <- t(apply(tfidf, 1, normalize))




ai_idx    <- which(all_labels == "AI")
nonai_idx <- which(all_labels == "Non-AI")

ai_mean    <- colMeans(tfidf_norm[ai_idx, , drop = FALSE])
nonai_mean <- colMeans(tfidf_norm[nonai_idx, , drop = FALSE])

contrast_score <- ai_mean - nonai_mean

top_terms <- names(sort(abs(contrast_score), decreasing = TRUE))[1:100]
final_matrix <- tfidf_norm[, top_terms]

contrast_df <- data.frame(
  Term  = top_terms,
  Score = contrast_score[top_terms],
  Type  = ifelse(contrast_score[top_terms] > 0, "AI", "Non-AI")
)



kmeans_res <- kmeans(final_matrix, centers = 2, nstart = 25)
kmeans_clusters <- factor(kmeans_res$cluster)

hc <- hclust(dist(final_matrix), method = "ward.D2")
hc_clusters <- factor(cutree(hc, k = 2))




kmeans_top_terms <- lapply(1:2, function(cl){
  center_vals <- kmeans_res$centers[cl, ]
  terms <- names(sort(abs(center_vals), decreasing = TRUE))[1:10]
  data.frame(
    Cluster = factor(cl),
    Term    = terms,
    Score   = center_vals[terms]
  )
})

kmeans_top_terms_df <- do.call(rbind, kmeans_top_terms)

hc_top_terms <- lapply(1:2, function(cl){
  cluster_docs <- final_matrix[hc_clusters == cl, , drop = FALSE]
  mean_vals <- colMeans(cluster_docs)
  terms <- names(sort(abs(mean_vals), decreasing = TRUE))[1:10]
  data.frame(
    Cluster = factor(cl),
    Term    = terms,
    Score   = mean_vals[terms]
  )
})

hc_top_terms_df <- do.call(rbind, hc_top_terms)




par(mar = c(1,1,1,1))
suppressWarnings(
  wordcloud(
    words = contrast_df$Term,
    freq  = abs(contrast_df$Score),
    max.words = 100,
    scale = c(3.5, 0.5),
    random.order = FALSE,
    colors = ifelse(contrast_df$Type == "AI", "#2C3E50", "#7F8C8D")
  )
)




ggplot(kmeans_top_terms_df,
       aes(x = reorder(interaction(Term, Cluster), Score),
           y = Score, fill = Cluster)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("1" = "#2C3E50", "2" = "#7F8C8D")) +
  theme_minimal(base_size = 13) +
  labs(title = "Top 10 Words per K-means Cluster",
       x = "Term", y = "Score", fill = "Cluster")




ggplot(hc_top_terms_df,
       aes(x = reorder(interaction(Term, Cluster), Score),
           y = Score, fill = Cluster)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("1" = "#2C3E50", "2" = "#7F8C8D")) +
  theme_minimal(base_size = 13) +
  labs(title = "Top 10 Words per Hierarchical Cluster",
       x = "Term", y = "Score", fill = "Cluster")




pca <- prcomp(final_matrix, scale. = TRUE)
pca_df <- data.frame(
  PC1  = pca$x[,1],
  PC2  = pca$x[,2],
  Type = all_labels
)

ggplot(pca_df, aes(PC1, PC2, color = Type)) +
  geom_point(size = 3, alpha = 0.75) +
  scale_color_manual(values = c("AI" = "#2C3E50", "Non-AI" = "#7F8C8D")) +
  theme_bw(base_size = 14) +
  labs(title = "PCA of AI vs Non-AI Articles")




plot(hc, main = "Hierarchical Clustering Dendrogram", cex = 0.7)
rect.hclust(hc, k = 2, border = c("#2C3E50", "#7F8C8D"))




table(KMeans_Cluster = kmeans_clusters, Type = all_labels)
table(HC_Cluster     = hc_clusters,     Type = all_labels)

