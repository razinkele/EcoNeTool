# =============================================================================
# Feedback analyzer - TF-IDF duplicate detection for the Feedback Dashboard
# =============================================================================
# Ported from MarineSABRES (functions/feedback_analyzer.R). Pure functions, no
# Shiny or DB dependency: the dashboard passes feedback_list() rows in and
# writes any "mark duplicate" decision back through feedback_mark_duplicate().
# Rows are keyed by the store `id` (MarineSABRES used NDJSON line numbers).
# =============================================================================

#' Compact English stopword list used by the tokeniser
FEEDBACK_STOPWORDS <- c(
  "a", "an", "the", "and", "or", "but", "in", "on", "at", "to", "for",
  "of", "with", "by", "from", "is", "are", "was", "were", "be", "been",
  "being", "have", "has", "had", "do", "does", "did", "will", "would",
  "could", "should", "may", "might", "shall", "can", "not", "no", "nor",
  "so", "yet", "both", "either", "neither", "each", "few", "more",
  "most", "other", "some", "such", "than", "too", "very", "just",
  "that", "this", "these", "those", "i", "me", "my", "we", "our",
  "you", "your", "he", "she", "it", "its", "they", "them", "their",
  "what", "which", "who", "whom", "when", "where", "why", "how",
  "all", "any", "if", "then", "because", "as", "until",
  "while", "about", "up", "out", "into", "through", "during",
  "before", "after", "above", "below", "between", "own", "same",
  "only", "also", "here", "there", "s", "t"
)

#' Pairwise TF-IDF cosine similarity
#'
#' @param texts Character vector of documents.
#' @return An NxN numeric matrix of cosine similarities in [0, 1]. A single
#'   document, or documents with no non-stopword tokens, give zeros.
#' @export
compute_text_similarity <- function(texts) {
  n <- length(texts)
  if (n < 2L) return(matrix(0.0, nrow = n, ncol = n))

  tokenise <- function(text) {
    text <- gsub("[^a-z0-9 ]", " ", tolower(as.character(text)))
    tokens <- strsplit(trimws(text), "\\s+")[[1L]]
    tokens[nzchar(tokens) & !tokens %in% FEEDBACK_STOPWORDS]
  }
  token_lists <- lapply(texts, tokenise)
  vocab <- sort(unique(unlist(token_lists)))
  if (length(vocab) == 0L) return(matrix(0.0, nrow = n, ncol = n))

  # Term frequency (rows = docs, cols = terms)
  tf_mat <- matrix(0.0, nrow = n, ncol = length(vocab))
  for (i in seq_len(n)) {
    toks <- token_lists[[i]]
    if (length(toks) == 0L) next
    counts <- table(toks)
    tf_mat[i, match(names(counts), vocab)] <- as.numeric(counts) / length(toks)
  }

  # IDF with +1 smoothing on document frequency, +1 offset to stay positive
  idf <- log(n / (colSums(tf_mat > 0) + 1L)) + 1.0
  tfidf <- sweep(tf_mat, 2L, idf, `*`)

  # Cosine similarity; zero-norm documents stay at 0 (no NaN)
  norms <- sqrt(rowSums(tfidf^2))
  unit <- tfidf / ifelse(norms == 0, 1, norms)
  sim <- tcrossprod(unit)
  sim[norms == 0, ] <- 0.0
  sim[, norms == 0] <- 0.0
  pmin(pmax(sim, 0.0), 1.0)
}

#' Candidate duplicate pairs among feedback rows
#'
#' Rows already marked as a duplicate (non-empty duplicate_of) are skipped.
#' Text is title + description; legacy rows without a title still compare on
#' their description. Capped to the 500 most recent rows (O(n^2) memory).
#'
#' @param df data.frame with columns id, title, description, duplicate_of
#'   (as returned by feedback_list()).
#' @param threshold Numeric in [0, 1]; pairs with similarity >= threshold.
#' @return data.frame(id_a, id_b, similarity), most similar first; id_a is the
#'   older row of the pair when ids are chronological.
#' @export
find_duplicate_pairs <- function(df, threshold = 0.7) {
  empty_pairs <- data.frame(id_a = integer(0), id_b = integer(0),
                            similarity = numeric(0), stringsAsFactors = FALSE)
  if (is.null(df) || nrow(df) < 2L) return(empty_pairs)

  dup <- if ("duplicate_of" %in% names(df)) as.character(df$duplicate_of) else NA_character_
  df <- df[is.na(dup) | !nzchar(dup) | dup == "NA", , drop = FALSE]
  if (nrow(df) < 2L) return(empty_pairs)

  df <- df[order(df$id), , drop = FALSE]
  if (nrow(df) > 500L) {
    warning("[feedback_analyzer] find_duplicate_pairs: capping to the 500 most recent rows",
            call. = FALSE)
    df <- df[seq(nrow(df) - 499L, nrow(df)), , drop = FALSE]
  }

  title <- if ("title" %in% names(df)) as.character(df$title) else NA_character_
  desc <- as.character(df$description)
  texts <- paste(ifelse(is.na(title), "", title), ifelse(is.na(desc), "", desc))
  sim <- compute_text_similarity(texts)

  hits <- which(upper.tri(sim) & sim >= threshold, arr.ind = TRUE)
  if (nrow(hits) == 0L) return(empty_pairs)

  out <- data.frame(id_a = as.integer(df$id[hits[, "row"]]),
                    id_b = as.integer(df$id[hits[, "col"]]),
                    similarity = sim[hits],
                    stringsAsFactors = FALSE)
  out <- out[order(-out$similarity), , drop = FALSE]
  rownames(out) <- NULL
  out
}
