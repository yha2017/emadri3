#' clean_corpus Function
#'
#' This function allows you to clean corpus.
#' @param corpus input corpus
#' @keywords corpus
#' @export
#' @examples
#' clean-corpus(corpus)


clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  return(corpus)
}
