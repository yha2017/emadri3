#' get_product_id Function
#'
#' This function allows you to get a top three product_id according to user's input text.
#' @param user_input_text User's input text
#' @param full_data Product database
#' @keywords userinput
#' @export
#' @examples
#' get_product_id("I need a comfortable dress", read.csv("sentiment_df_v1"), 3 )

get_product_id <- function(user_input_text, full_data){
  user_id <- 1
  sentiment_user <- exploratory::get_sentiment(user_input_text)
  df <- data.frame(user_id, user_input_text)
  input_matrix <- df_to_matrix(df)
  input_df <- as.data.frame(input_matrix)
  input_df <- setDT(input_df, keep.rownames = TRUE)[]
  names(input_df)[1] <- "user_id"
  input_df %>%
    gather(words, count, -user_id) -> tidy_input
  adj_df <- merge(x = full_data, y = tidy_input,
                  by.x = "value", by.y = "words")
  adj_df <- na.omit(adj_df)
  category_Df <- merge(x = full_data, y = tidy_input,
                       by.x = "item_name", by.y = "words")
if(nrow(merge(x = adj_df, y = category_Df, by = "product_id")) == 0){
    if(nrow(adj_df) == 0 && nrow(category_Df) != 0){
      results <- category_Df
      
      if(sentiment_user >= 0){
        results <- results[which(results$star_rating >= 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
        results <- results[order(results$weight, decreasing = TRUE),]
        results <- results$product_id
        results <- na.omit(results)
        results <- results[!duplicated(results)]
        results <- as.character(results)
        results = results[which(nchar(results) == 10)]
      }else{
        results <- results[which(results$star_rating < 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
        results <- results[order(results$weight, decreasing = TRUE),]
        results <- results$product_id
        results <- na.omit(results)
        results <- results[!duplicated(results)]
        results <- as.character(results)
        results = results[which(nchar(results) == 10)]
      }
    }else if(nrow(adj_df) != 0 && nrow(category_Df) == 0){
      results <- adj_df
      if(sentiment_user >= 0){
        results <- results[which(results$star_rating >= 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
        results <- results[order(results$weight, decreasing = TRUE),]
        results <- results$product_id
        results <- na.omit(results)
        results <- results[!duplicated(results)]
        results <- as.character(results)
        results = results[which(nchar(results) == 10)]
      }else{
        results <- results[which(results$star_rating < 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
        results <- results[order(results$weight, decreasing = TRUE),]
        results <- results$product_id
        results <- na.omit(results)
        results <- results[!duplicated(results)]
        results <- as.character(results)
        results = results[which(nchar(results) == 10)]
      }
    }else if(nrow(adj_df) == 0 && nrow(category_Df) == 0){
        results = "There is no match according to your input, sorry."
      }
     else{
        results <- category_Df
        
        if(sentiment_user >= 0){
          results <- results[which(results$star_rating >= 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
          results <- results[order(results$weight, decreasing = TRUE),]
          results <- results$product_id
          results <- na.omit(results)
          results <- results[!duplicated(results)]
          results <- as.character(results)
          results = results[which(nchar(results) == 10)]
      }else{
          results <- results[which(results$star_rating < 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
          results <- results[order(results$weight, decreasing = TRUE),]
          results <- results$product_id
          results <- na.omit(results)
          results <- results[!duplicated(results)]
          results <- as.character(results)
          results = results[which(nchar(results) == 10)]
      }
    }
}else if(nrow(merge(x = adj_df, y = category_Df, by = "product_id")) != 0){
    results <- merge(x = adj_df, y = category_Df, by = "product_id")
    if(sentiment_user >= 0){
      results <- results[which(results$star_rating.x >= 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
      results <- results[order(results$weight.x, decreasing = TRUE),]
      results <- results$product_id
      results <- na.omit(results)
      results <- results[!duplicated(results)]
      results <- as.character(results)
      results = results[which(nchar(results) == 10)]
    }else{
      results <- results[which(results$star_rating.x < 3),]
      results <- results[order(order(match(c("color", "comfort", "adj"), results$attributes))), ]
      results <- results[order(results$weight.x, decreasing = TRUE),]
      results <- results$product_id
      results <- na.omit(results)
      results <- results[!duplicated(results)]
      results <- as.character(results)
      results = results[which(nchar(results) == 10)]
  }
  }
 results <- as.data.frame(results)
 results <- head(results, 3)
 results_final <- merge(x = results, y = basic_info, by.x = "results", by.y = "product_id") 
  return(results_final)
}
