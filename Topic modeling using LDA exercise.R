library(tidyverse) 
library(tidytext) 
library(topicmodels) 
library(tm)
library(SnowballC) 
library(stringr)
library(NLP)

reviews <- read_csv(file.choose()) #load the ChicagoReviews2kAirBnB.csv file 

#clean the review data, our reviews are in the 'text' column of the dataset
reviews$text <- str_replace_all(reviews$text,"[^[:graph:]]", " ") 

top_terms_by_topic_LDA <- function(input_text, # should be a column from a data frame
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic, 
  # yes I made up the word informativeness
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a separate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
} 


#get just two topics to see how things pan out, carefully check words to see 
#what needs to be added to stop words.
top_terms_by_topic_LDA(reviews$text, number_of_topics = 2)


# pay attention to column names
reviewsCorpus <- Corpus(VectorSource(reviews$text)) 
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)

# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

# I'm going to add my own custom stop words that I don't think will be
# very informative in these reviews. My first test topic model indicated that # I should add room and Chicago to the list. Case is not relevant 

custom_stop_words <- tibble(word = c("room", "chicago"))

# remove stopwords from the dataset of reviews
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
  anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
  anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords


# reconstruct cleaned documents (so that each word shows up the correct number of times)
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# check out what the cleaned documents look like (should just be a bunch of content words)
# in alphabetic order
head(cleaned_documents)

  
#try out two topics, expand to 3 or 4 or more. I found three topics to be 
# ideal in this specific 2k review set but in the larger set I needed 4.


top_terms_by_topic_LDA(cleaned_documents, number_of_topics = 2)  

top_terms_by_topic_LDA(cleaned_documents, number_of_topics = 3) 

top_terms_by_topic_LDA(cleaned_documents, number_of_topics = 4)  



# stem the words (e.g. convert each word to its stem, where applicable)
reviewsDTM_tidy_cleaned <- reviewsDTM_tidy_cleaned %>% 
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(stem, count))) %>%
  select(document, terms) %>%
  unique()


#try out the lower end of what was acceptable from step 6. This was 3 for me
#I then tried 4 for the larger set which seemed to work well enough. 
# now let's look at the new most informative terms

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 3)  

top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)  




