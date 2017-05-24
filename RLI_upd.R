# Competition
# House Prices: Two Sigma Connect: Rental Listing Inquiries
# Goal: How much interest will a new rental listing on RentHop receive?
# Link: https://www.kaggle.com/c/two-sigma-connect-rental-listing-inquiries
## My goal was to practice Text Mining basics


#### Data preparation ####

### Load and preprocess the data----

library(jsonlite)
library(dplyr)

## Reading the JSON data

train <- fromJSON("C:/Users/user/Documents/R projects/Kaggle/Two Sigma_Rental Listing Inquiries/train.json")

## unlist every variable except `photos` and `features` and convert to tibble

vars <- setdiff(names(train), c("photos", "features"))
train <- map_at(train, vars, unlist) %>% tibble::as_tibble(.)

## W|O features and photos

train <- train %>% 
           select(-features, -photos)

### Check whether there are any NA values

library(reshape2)

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(train)


#### Ploting Price distribution by street address(number of streets) and interest level ####

### Prepearing data set

d <- train %>%
       group_by(street_address, price, interest_level) %>%
       summarise(n = sum(!is.na(street_address)))

### Plotting price vs street_address(number of streets) +interest_level----

library(ggplot2)
library(scales)

d %>% 
  ggplot(aes(x = n,
             y = price)) + 
  theme(legend.position="top",
        axis.text=element_text(size = 6)) +
  geom_point(aes(color = as.character(interest_level)), alpha = 0.5, size = 1.5, 
             position = position_jitter(width = 0.25, height = 0)) + 
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.5) + 
  scale_y_continuous(labels = comma)

## Deepper look on previous plot

d %>% 
  filter(price < 25000) %>% 
      ggplot(aes(x = n, 
                 y = price)) + 
      theme(legend.position="top",
            axis.text=element_text(size = 6)) +
      geom_point(aes(color = interest_level), alpha = 0.5, size = 1.5, 
                     position = position_jitter(width = 0.25, height = 0)) + 
      stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                   geom = "crossbar", width = 0.5) + 
      scale_y_continuous(labels = comma)


#### Text processing - "description feature" using tm and NLP ####

library(tm)
library(NLP)

### Prepare the data----

dat <- train %>% 
         mutate(listing_id = as.character(listing_id)) %>% 
         mutate(description = as.character(description)) %>% 
         tbl_df()

### Create corpus----

my_corpus <- Corpus(VectorSource(dat$description))
my_corpus <- tm_map(my_corpus, content_transformer(tolower)) # Convert words to lowercase
my_corpus <- tm_map(my_corpus, removeWords, c(stopwords("english"), "br")) # Remove stopwords and br (an artifact from html)
my_corpus <- tm_map(my_corpus, removePunctuation) # Remove punctiation
my_corpus <- tm_map(my_corpus, stripWhitespace) # Strip whitespace
my_corpus <- tm_map(my_corpus, removeNumbers) # RemoveNumbers
my_corpus <- tm_map(my_corpus, stemDocument) # Stem words

## Cretae the Document matrix

dtm <- DocumentTermMatrix(my_corpus, 
                          control = list(weighing=weightTfIdf,minWordLength=2, 
                                         minDocFreq=10))

## Reduce sparsity to lower number of features

dtm <- removeSparseTerms(dtm, 0.90)
dtm <- as.matrix(dtm)

## Converting to data.frame and summarising the data

dtm <- melt(as.data.frame(dtm))
dtm <- dtm %>%
        group_by(variable) %>%
        summarise(n = sum(value))

### Removing rows which contain----

dtm <- dtm[ grep("open", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("see", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("just", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("can", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("will", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("throughout", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("kagglemanagerrenthopcom", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("one", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("two", dtm$variable, invert = TRUE) , ]
dtm <- dtm[ grep("websiteredact", dtm$variable, invert = TRUE) , ]

### Renaming the variables----

dtm$variable <- gsub("apart", "apartment", dtm$variable)
dtm$variable <- gsub("build", "building", dtm$variable)
dtm$variable <- gsub("locat", "location", dtm$variable)
dtm$variable <- gsub("featur", "features", dtm$variable)
dtm$variable <- gsub("renov", "renovated", dtm$variable)
dtm$variable <- gsub("applianc", "appliances", dtm$variable)
dtm$variable <- gsub("larg", "large", dtm$variable)
dtm$variable <- gsub("includ", "include", dtm$variable)
dtm$variable <- gsub("laundri", "laundry", dtm$variable)
dtm$variable <- gsub("ceil", "ceiling", dtm$variable)
dtm$variable <- gsub("beati", "beatiful", dtm$variable)
dtm$variable <- gsub("restaur", "restaurants", dtm$variable)
dtm$variable <- gsub("marbl", "marble", dtm$variable)
dtm$variable <- gsub("granit", "granite", dtm$variable)
dtm$variable <- gsub("fit", "fitness", dtm$variable)
dtm$variable <- gsub("luxuri", "luxury", dtm$variable)
dtm$variable <- gsub("privat", "private", dtm$variable)
dtm$variable <- gsub("dishwash", "dishwasher", dtm$variable)
dtm$variable <- gsub("inform", "information", dtm$variable)
dtm$variable <- gsub("elev", "elevator", dtm$variable)
dtm$variable <- gsub("bath", "bathroom", dtm$variable)
dtm$variable <- gsub("bathroomroom", "bathroom", dtm$variable)
dtm$variable <- gsub("amen", "amenities", dtm$variable)
dtm$variable <- gsub("avail", "available", dtm$variable)
dtm$variable <- gsub("servic", "service", dtm$variable)
dtm$variable <- gsub("estat", "estate", dtm$variable)
dtm$variable <- gsub("amaz", "amazing", dtm$variable)
dtm$variable <- gsub("schedul", "shedule", dtm$variable)
dtm$variable <- gsub("storag", "storage", dtm$variable)
dtm$variable <- gsub("hous", "housing", dtm$variable)
dtm$variable <- gsub("pleas", "please", dtm$variable)
dtm$variable <- gsub("closett", "closet", dtm$variable)
dtm$variable <- gsub("close", "closet", dtm$variable)
dtm$variable <- gsub("show", "shower", dtm$variable)
dtm$variable <- gsub("loung", "lounge", dtm$variable)
dtm$variable <- gsub("conveni", "convenient", dtm$variable)
dtm$variable <- gsub("natur", "natural", dtm$variable)
dtm$variable <- gsub("side", "resident", dtm$variable)
dtm$variable <- gsub("concierg", "concierge", dtm$variable)
dtm$variable <- gsub("opportunityp", "opportunity", dtm$variable)

### Also variables "bathroom" and "closet" were doubled----

dtm <- dtm %>%
        group_by(variable) %>%
        summarise(n = sum(n))


#### Setiment analysis for "description feature" using tidytext ####

library(tidytext)
library(stringr)
library(tidyr)

### Prepare the data----

text_df <- melt(as.data.frame(dtm))

data_ta <- train %>% 
             select(listing_id, description) %>%
             mutate(listing_id = as.character(listing_id)) %>% 
             mutate(description = as.character(description)) %>% 
             tbl_df() %>% 

text_df <- data_frame(text = data_ta$description)

tidys <- text_df %>%
           mutate(linenumber = row_number()) %>%
           ungroup() %>%
           unnest_tokens(word, text) %>%
           anti_join(stop_words) # remove stop words

### Comparing the three sentiment dictionaries (AFINN, Bing, NRC)----

## Setiment analysis with AFINN

afinn <- tidys %>% 
           inner_join(get_sentiments("afinn")) %>% 
           group_by(index = linenumber %/% 80) %>% 
           summarise(sentiment = sum(score)) %>% 
           mutate(method = "AFINN")

## Setiment analysis with Bing and NRC

bing_and_nrc <- bind_rows(tidys %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidys %>% 
                            inner_join(get_sentiments("nrc") %>% 
                            filter(sentiment %in% c("positive", 
                                                    "negative"))) %>%
                            mutate(method = "NRC")) %>%
                            count(method, index = linenumber %/% 80, sentiment) %>%
                            spread(sentiment, n, fill = 0) %>%
                            mutate(sentiment = positive - negative)

### Plotting the results (AFINN vs Bing vs NRC)----

bind_rows(afinn, bing_and_nrc) %>%
                        ggplot(aes(index, sentiment, fill = method)) +
                        geom_bar(stat = "identity", show.legend = FALSE) +
                        facet_wrap(~method, ncol = 1, scales = "free_y")


### Word frequencies----

## Word frequencies with Bing

bing_word_counts <- tidys %>%
                      inner_join(get_sentiments("bing")) %>%
                      count(word, sentiment, sort = TRUE) %>%
                      ungroup()

## Plotting contribution to sentiment using Bing----

bing_word_counts %>%
           group_by(sentiment) %>%
           top_n(10) %>%
           mutate(word = reorder(word, n)) %>%
           ggplot(aes(word, n, fill = sentiment)) +
             geom_bar(stat = "identity", show.legend = FALSE) +
             facet_wrap(~sentiment, scales = "free_y") +
             labs(y = "Contribution to sentiment using Bing",
                  x = NULL) +
             coord_flip()

## Word frequencies with NRC

nrc_word_counts <- tidys %>%
                     inner_join(get_sentiments("nrc")) %>%
                     count(word, sentiment, sort = TRUE) %>%
                     filter(sentiment %in% c("positive", 
                                             "negative")) %>%
                     ungroup()


## Plotting contribution to sentiment using NRC----

nrc_word_counts %>%
          group_by(sentiment) %>%
          top_n(10) %>%
          mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n, fill = sentiment)) +
            geom_bar(stat = "identity", show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment using NRC",
                 x = NULL) +
            coord_flip()


#### Building Wordcloud ####

library(wordcloud)
library(RColorBrewer)

### Prepare the data----

tidys <- tidys[ grep("br", tidys$word, invert = TRUE) , ]

tidys %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 75))


### Plotting the World Cloud----

library(reshape2)

tidys %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),max.words = 100)


#### Relationships between words: n-grams ####

### Counting and filtering n-grams----

tidy_bigrams <- text_df %>%
                  unnest_tokens(bigram, text, token = "ngrams", n = 2)

tidy_bigrams %>%
     count(bigram, sort = TRUE)

bigrams_separated <- tidy_bigrams %>%
                        separate(bigram, c("word1", "word2"), sep = " ")

## Removing cases with stop-words

bigrams_filtered <- bigrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word)

## New bigram counts

bigram_counts <- bigrams_filtered %>% 
                    count(word1, word2, sort = TRUE)

### Preparing data for plotting----

bigram_graph <- bigram_counts %>%
                    filter(n > 1000) %>%
                    graph_from_data_frame()

### Ploting relatively common words combinations----

library(ggraph)

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


## Plot_update (including some polishing operations to make a better looking graph)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
