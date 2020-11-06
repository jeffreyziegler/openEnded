#' @title Create Word Embeddings
#' @description
#' Generate comparison between the prompt and participants' responses in a latent word2vec space.
#'
#' @param dataframe Dataframe from which we will estimate our regression model.
#' @param prompts Vector/column in dataframe that contain the prompt that each respondnet saw.
#' @param responses Vector/column in dataframe that records participants' recollection of prompt.
#' @param user_seed Set seed for reproducibility. Default = 5.
#' @param prune_len Cut words that are not frequently mentioned. Default = 2 (must be mentioned at least twice in the corpus).
#' @param language What language are the prompts and responses? Default is English ('en'), Spanish ('es') and Brazilian-Portuguese ('br-pt') will hopefully be available soon.
#' 
#' @author Jeffrey Ziegler (<jeffrey.ziegler[at]emory.edu>)
#' @examples
#' 
#' 
#' @rdname createWordEmbeddings
#' @seealso \code{\link{similarityMeasures}}
#' @export

createWordEmbeddings <- function(dataframe, prompts, responses, user_seed=5, prune_len=2, language="en"){
  # create iterator over tokens
  # first, put the prompts
  # then the rest of the responses
  word_ls <- c(space_tokenizer(prepText(unique(dataframe[, prompts]))), 
               space_tokenizer(prepText(dataframe[, responses])))
  
  # create vocabulary
  # terms will be unigrams (simple words)
  it <- itoken(word_ls, progressbar = F)
  vocab <- create_vocabulary(it)
  
  # words should not be too uncommon
  # can't calculate meaningful word vector for 
  # a word which we saw only once in the entire corpus
  # Default: Take only words which appear at least once
  limited_vocab <- prune_vocabulary(vocab, term_count_min = prune_len)
  
  # ready to construct term-co-occurence matrix (TCM)
  # use our filtered vocabulary
  vectorizer <- vocab_vectorizer(limited_vocab)
  
  # create normal dtm
  dtm <- create_dtm(it, vectorizer)
  
  # use window of 5 for context words
  tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
  
  # set seed first for reproducibility
  set.seed(user_seed)
  
  if(language!="en"){
    error("Currently this language is unsupported, this feature is only available for prompts and responses in English.")
  }
  if(language=="en"){
    # w/ TCM matrix can factorize it via GloVe algorithm
    # text2vec uses a parallel stochastic gradient descent algorithm
    glove_model <- GloVe$new(rank = 50, x_max = 10)
    # retrieve the word vectors
    word_embeddings <- glove_model$fit_transform(tcm, n_iter = 10000, convergence_tol = 0.00001)
  }
  
  # get average of main and context vectors as proposed in GloVe paper
  word_vectors <- word_embeddings + t(glove_model$components)  
  
  # execute Relaxed Word Movers Distance

  # old way under text2vec version 0.5.0
  # rwmd_model <- RWMD$new(x=dtm, embeddings=word_vectors, method = "cosine", normalize = T)
  # rwmd_dist <- 1-dist2(dtm[1:length(unique(dataframe[, prompts])), ], 
  #                      dtm[(length(unique(dataframe[, prompts]))+1):dim(dtm)[1],], 
  #                      # don't need to tell norm here since we normalized above
  #                      method = rwmd_model, norm = 'none')
  # now under 0.6
  # should automatically now use cosine distance instead of euclidean
  rwmd_model <- RWMD$new(x=dtm, embeddings=word_vectors) 
  rwmd_dist <- 1-rwmd_model$dist2(dtm)
  rwmd_dist <- ifelse(rwmd_dist<0, 0, rwmd_dist)
  
  # now, put those embedding scores back into original dataframe
  dataframe$embedding_score <- NULL
  for(obs in 1:dim(dataframe)[1]){
    # based on which prompt they saw
    # since we have comparisons of all prompts to all responses
    #browser()
    if(dataframe[obs, prompts]==unique(dataframe[, prompts])[1]){
      dataframe[obs, "embedding_score"] <- rwmd_dist[1, obs+3]
    } 
    if(dataframe[obs, prompts]==unique(dataframe[, prompts])[2]){
      dataframe[obs, "embedding_score"] <- rwmd_dist[2, obs+3]
    }
    if(dataframe[obs, prompts]==unique(dataframe[, prompts])[3]){
      dataframe[obs, "embedding_score"] <- rwmd_dist[3, obs+3]
    }
  }
  
  return(dataframe)
}
