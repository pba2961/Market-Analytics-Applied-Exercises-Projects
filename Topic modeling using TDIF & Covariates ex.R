library(tidyverse) 
library(tidytext) 
library(topicmodels) 
library(tm)
library(SnowballC) 
library(stringr)
library(NLP)



reviews <- read_csv(file.choose()) 

#clean the review data, our reviews are in the 'text' column of the dataset 
reviews$text <- str_replace_all(reviews$text,"[^[:graph:]]", " ")

top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot
                                     = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enough stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>%
    ungroup()
  # get the number of words per text
  total_words <- words %>%
    group_by(!!group_column) %>%
    summarize(total = sum(n))
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  # get the tf_idf & order the words by degree of relevance
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to idiosyncrasies with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    # plot the 10 most informative terms per topic
    tf_idf %>%
      group_by(!!group_column) %>%
      top_n(10) %>%
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}

# here we are using the listing_id as the grouping variable.
# if you use large datasets be aware of how this will affect your readability

reviews_tfidf_byListing <- top_terms_by_topic_tfidf(text_df = reviews, 
                                                    text_column = text, group = listing_id, plot = F)
reviews_tfidf_byListing %>%
  group_by(listing_id) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = listing_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~listing_id, ncol = 4, scales = "free", ) +
  coord_flip()
