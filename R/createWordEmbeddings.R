#' @title Create Word Embeddings
#' @description
#' Generate comparison between the prompt and participants' responses in a latent word2vec space.
#'
#'
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' 
#' 
#' @rdname createWordEmbeddings
#' @seealso \code{\link{}}
#' @export

createWordEmbeddings <- function(df, prompts, responses, user_seed=5, prune_len=1){
  # Create iterator over tokens
  # first, put the prompts
  #, then the rest of the responses
  word_ls <- c(space_tokenizer(prepText(unique(df[, prompts]))), space_tokenizer(prepText(df[, responses])))
  
  # Create vocabulary. Terms will be unigrams (simple words).
  it <- itoken(word_ls, progressbar = F)
  vocab <- create_vocabulary(it)
  # words should not be too uncommon
  # can't calculate meaningful word vector for a word which we saw only once in the entire corpus
  # Default: Take only words which appear at least once
  limited_vocab <- prune_vocabulary(vocab, term_count_min = prune_len)
  
  # now have 377 terms in vocab
  # ready to construct term-co-occurence matrix (TCM)
  # Use our filtered vocabulary
  vectorizer <- vocab_vectorizer(limited_vocab)
  # create normal dtm
  dtm <- create_dtm(it, vectorizer)
  # use window of 5 for context words
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
  # with TCM matrix can factorize it via GloVe algorithm
  # text2vec uses a parallel stochastic gradient descent algorithm
  set.seed(user_seed)
  
  glove_model <- GloVe$new(word_vectors_size = 50, vocabulary = limited_vocab, x_max = 10)
  # get the word vectors:
  word_embeddings <- glove_model$fit_transform(tcm, n_iter = 10000, convergence_tol = 0.00001)
  
  # get average of main and context vectors as proposed in GloVe paper
  word_vectors <- word_embeddings + t(glove_model$components)  
  # execute Relaxed Word Movers Distance
  rwmd_model <- RWMD$new(word_vectors, method = "cosine", normalize = T)
  
  rwmd_dist <- 1-dist2(dtm[1:length(unique(df[, prompts])), ], 
        dtm[(length(unique(df[, prompts]))+1):dim(dtm)[1],], 
        method = rwmd_model, norm = 'none')
  #browser()
  rwmd_dist <- ifelse(rwmd_dist<0, 0, rwmd_dist)
  df$embedding_score <- NULL
  for(obs in 1:dim(df)[1]){
    #browser()
    if(df[obs, prompts]==unique(df[, prompts])[1]){
      df[obs, "embedding_score"] <- rwmd_dist[1, obs]
    } 
    if(df[obs, prompts]==unique(df[, prompts])[2]){
      df[obs, "embedding_score"] <- rwmd_dist[2, obs]
    }
    if(df[obs, prompts]==unique(df[, prompts])[3]){
      df[obs, "embedding_score"] <- rwmd_dist[3, obs]
    }
  }
  
  return(df)
}
