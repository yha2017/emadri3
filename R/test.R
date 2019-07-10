get_product_id2 <- function(user_input_text, full_data){
  user_id <- 1
  sentiment_user <- exploratory::get_sentiment(user_input_text)
  df <- data.frame(user_id, user_input_text)
  input_matrix <- df_to_matrix(df)
  input_df <- data.frame(input_matrix)
  input_df <- setDT(input_df, keep.rownames = TRUE)[]
  names(input_df)[1] <- "user_id"
  input_df %>%
    gather(words, count, -user_id) -> tidy_input

  adj_df <- merge(x = tidy_input, y = full_data,
                  by.x = "words", by.y = "value", all.x = TRUE)
  adj_df <- na.omit(adj_df)
  category_Df <- merge(x = tidy_input, y = full_data,
                       by.x = "words", by.y = "item_name", all.x = TRUE)
  
  if(is.na(merge(x = adj_df, y = category_Df, by = "product_id")) == TRUE){
    if(is.na(adj_df) == TRUE && is.na(category_Df) == FALSE){
      results <- category_Df
    }else if(is.na(adj_df) == FALSE && is.na(category_Df) == TRUE){
      results <- adj_df
    }else{
      results <- catefory_Df
    }
  }else if(is.na(merge(x = adj_df, y = category_Df, by = "product_id")) == FALSE){
    results <- merge(x = adj_df, y = category_Df, by = "product_id")
  }
  
  if(sentiment_user >= 0){
    results <- results[which(results$star_rating.x >= 3),]
    results <- results[order(results$weight.x, decreasing = TRUE),]
    results <- results$product_id
    results <- na.omit(results)
    results <- results[!duplicated(results)]
    results <- as.character(results)
    results = results[which(nchar(results) == 10)]
  }else{
    results <- results[which(results$star_rating.x < 3),]
    results <- results[order(results$weight.x, decreasing = TRUE),]
    results <- results$product_id
    results <- na.omit(results)
    results <- results[!duplicated(results)]
    results <- as.character(results)
    results = results[which(nchar(results) == 10)]
  }
  return(head(results,3))
}
