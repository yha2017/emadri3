#' df_to_matrix Function
#'
#' This function allows you to convert a dataframe to a matrix.
#' @param data Input dataframe
#' @keywords convert
#' @export

#Requirement: Input data fram has only two columns: id and text
#Output: matrix of single words with count.

df_to_matrix <- function(data){
  #Make a VCorpus from a data frame
  names(data) <- c("doc_id", "text")
  # Create a DataframeSource: df_source
  df_source <- DataframeSource(data)
  # Convert df_source to a corpus: df_corpus
  df_corpus <- VCorpus(df_source)

  clean_corp <- clean_corpus(df_corpus)

  # Create the dtm from the corpus: df_corpus
  dtm <- DocumentTermMatrix(clean_corp)
  # Convert category_dtm to a matrix: category_m
  words_matrix <- as.matrix(dtm)

  return(words_matrix)
}
