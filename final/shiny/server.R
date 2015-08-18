library(shiny)
library(tm)
#library(slam) 
#library(ggplot2)
#setwd("~/JHDataScience/capstone/final/shiny")
load("bigram_index_small.RData")
load("trigram_index_small.RData")
load("fourgram_index.RData")

bigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

trigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

fourgramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

clean.input <- function(phrase){
    split <- strsplit(phrase, " ")
    result_split <- tolower(matrix(unlist(split)))
    result_split <- gsub("[^[:alnum:][:space:]']", "", result_split)
    result_split <- gsub('[[:digit:]]+', '', result_split)
    num_words <- nrow(result_split)
    
    cleaned_list <- character(0)
    if(num_words > 2) {    
        bigram_word <- result_split[num_words - 2]
        cleaned_list[1] <- bigram_word 
        trigram_word <- result_split[(num_words - 1)]
        cleaned_list[2] <- trigram_word
        fourgram_word <- result_split[(num_words)]
        cleaned_list[3] <- fourgram_word
    } else if(num_words == 2) {    
        bigram_word <- result_split[num_words - 1]
        cleaned_list[1] <- bigram_word 
        trigram_word <- result_split[(num_words)]
        cleaned_list[2] <- trigram_word
    } else if (num_words == 1) {    
        bigram_word <- result_split[num_words]
        cleaned_list[1] <- bigram_word 
    }
    cleaned_list
    #bigram_index <- do.call(rbind, strsplit(paste(rownames(tdm_bigram.matrix), tdm_bigram.matrix), " "))
    # works
    #phrase <- "test word"
}

predict.next.word.backoff <- function(word){
    ngram_df <- data.frame(predicted=character(), count = numeric(), stringsAsFactors=FALSE)
    word_match_one <- word[1]
    word_match_two <- word[2]
    word_match_three <- word[3]
    #print (is.na(word_match_three))
    #print (word_match_three)
    if(!is.na(word_match_three)){
        print ("word 3 not null")
        col_ng_matrix <- nrow(fourgram_index)
        cond_three <- c(word_match_one == fourgram_index[,1] & word_match_two == fourgram_index[,2] 
                        & word_match_three == fourgram_index[,3])
        for (i in 1:col_ng_matrix){
            if (cond_three[i]){
                fourth_word <- fourgram_index[,4][i]
                count_word <- fourgram_index[,5][i]
                matched_factor <- structure(c(fourth_word, count_word), .Names = c("predicted", "count"))
                ngram_df[i,] <- as.list(matched_factor)
            } } 
        if(nrow(ngram_df)<1){  print ("df fourgram is null")
                               predict.next.word.backoff(c(word_match_two, word_match_three))}else{
                                   ngram_df <- transform(ngram_df, count = as.numeric(count))
                                   return (ngram_df[order(ngram_df$count, decreasing = TRUE),])  
                               }}  
    else if(!is.na(word_match_two)){
        print ("word 2 not null")
        col_ng_matrix <- nrow(trigram_index)
        cond_two <- c(word_match_one == trigram_index[,1] & word_match_two == trigram_index[,2])
        for (i in 1:col_ng_matrix){
            if (cond_two[i]){
                third_word <- trigram_index[,3][i]
                count_word <- trigram_index[,4][i]
                matched_factor <- structure(c(third_word, count_word), .Names = c("predicted", "count"))
                ngram_df[i,] <- as.list(matched_factor)
            } } 
        if(nrow(ngram_df)<1){  print ("df trigram is null")
                               predict.next.word.backoff(c(word_match_two))}else{
                                   ngram_df <- transform(ngram_df, count = as.numeric(count))
                                   return (ngram_df[order(ngram_df$count, decreasing = TRUE),])  
                               }}  
    else {
        print ("word 1 not null")
        col_ng_matrix <- nrow(bigram_index)      
        cond_one <- c(word_match_one == bigram_index[,1])
        for (i in 1:col_ng_matrix){
            if (cond_one[i] && !is.na(cond_one[i])){
                second_word <- bigram_index[,2][i]
                count_word <- bigram_index[,3][i]
                matched_factor <- structure(c(second_word, count_word), .Names = c("predicted", "count"))
                ngram_df[i,] <- as.list(matched_factor)
            } } 
        if(nrow(ngram_df)<1){  print ("df bigram is null")   }else {
            ngram_df <- transform(ngram_df, count = as.numeric(count))
            return (ngram_df[order(ngram_df$count, decreasing = TRUE),])  
        }}   
    #ngram_df <- transform(ngram_df, count = as.numeric(count))
    #return (ngram_df[order(ngram_df$count, decreasing = TRUE),])  
}

shinyServer(
    function(input, output) {
        
        output$text_entered <- renderText({
        input$text_input
        })
        
        output$text_prediction_output <- renderText({
            #if(input$predictButton) {
                text_box <- input$text_input
                text_cleaned <- clean.input(text_box)
                result_df <- predict.next.word.backoff(text_cleaned)
                
                if (!is.null(dim(result_df))){
                as.character(result_df[1,][1])
                }
            #}
        })
        
    }   
)

