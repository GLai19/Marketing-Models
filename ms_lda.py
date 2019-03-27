# -*- coding: utf-8 -*-
"""
Created on Wed Nov 14 19:39:51 2018

@author: mab2343
"""

#### Import Packages ####
import gensim
from gensim import corpora,models
from gensim.models import LdaModel
from gensim.parsing.preprocessing import remove_stopwords,strip_punctuation, strip_numeric,strip_short
import pandas as pd
import unidecode

#### Import data ####
data=pd.read_csv("C:/Users/mbensliman20/Downloads/Product Reviews.csv") # Import the data
data=data[["Review_ID","Item_ID","Review_date","Content"]] # Select a subset of the columns
reviews=[review for review in data["Content"]] # Create a list where each element is a single review

#### Text Preprocessing ####
reviews # Print the list "reviews"
reviews[2] # Print the 3rd review. Remember Python starts the numbering at 0
## Convert to lower case
reviews[0].lower() 
## Remove punctuation
strip_punctuation(reviews[2])
## Remove numbers
strip_numeric(strip_punctuation(reviews[2].lower())) # Here we do three operations: 1) we convert to lower case, 2) then we remove punctuation and 3) finally we remove numbers
## Remove stopwords
remove_stopwords(strip_numeric(strip_punctuation(reviews[2].lower()))) # We do 4 operations: in addition to the previous operations, we also remove stopwords
## Remove short words
strip_short(remove_stopwords(strip_numeric(strip_punctuation(reviews[2].lower()))),2) # This remove words that have less than 2 characters. We perform this on top of the previous operations

strip_short(remove_stopwords(strip_numeric(strip_punctuation(reviews[2].lower()))),2).split() # Finally, we split the text in a list of words

## Preprocessing function
def preprocess(doc):
    return(strip_short(remove_stopwords(strip_numeric(strip_punctuation(doc.lower()))),3).split()) # This function performs all the previous steps
    
#### Corpus preparation ####
## Tokenization
corpus=[preprocess(review) for review in reviews] # We create a new list where each element is the list of word in a given review. In other words, it is a list of list of words
## Creation of Dictionary
dictionary = corpora.Dictionary(corpus) #this function creates a dictionary based on the previous corpus. It contains all the unique words in the reviews with  a unique ID attached to each of them
## Word-ID mapping
print(dictionary.token2id) # This will print the list of tokens and their ID
## Map corpus to ID -- Vectorization
corpus = [dictionary.doc2bow(preprocess(review)) for review in reviews] #doc2bow converts a document into a bag of word, that is a list of tuples containg the id of a word (from the dictionary) and its frequency in the document. The procedure creates a list of bag of words.

### Recover words
print([[dictionary[id], freq] for id, freq in corpus[0]]) # This simply represents the bag of word with the real word instead of the ID

### tfidf
import numpy as np
tfidf = models.TfidfModel(corpus, smartirs='ntc') # This creates a corpus transformed into tfidf. Note that we do not use tf-idf later
print([[dictionary[id], np.around(freq, decimals=2)] for id, freq in tfidf[corpus[0]]])

## Remove unfrequent words
dictionary.filter_extremes(no_below=5, no_above=0.75) # we use this function to filter words that appear not often (as an integer, here 5) and too often (as a percentage, here in more than 75% [pe])
corpus = [dictionary.doc2bow(preprocess(review)) for review in reviews]

#### LDA ####
lda_model = LdaModel(corpus=corpus,  # This code runs your lda
                         id2word=dictionary, 
                         random_state=100, 
                         num_topics=10,
                         passes=5,
                         chunksize=10000,
                         alpha='asymmetric',
                         decay=0.5,
                         offset=64,
                         eta=None,
                         eval_every=0,
                         iterations=100,
                         gamma_threshold=0.001,
                         per_word_topics=True)

## See the topics
lda_model.print_topics(-1) #this allows to observe the topics
lda_model.get_topic_terms(0, topn=10) # this provides the top 10 words in topic 0
lda_model.log_perplexity(corpus) # this compute the log perplexity
lda_model.get_document_topics(corpus[0]) # This provide the document topic distribution. Note that by default, when a document has a low probability on a topic, it is not displayed
lda_model.get_document_topics(corpus[0],minimum_probability=0) # This provide the document topic distribution. Here, every topics and associated probabilities are printed.
### Document topic
####
# Plotting tools
import pyLDAvis
import pyLDAvis.gensim  # don't skip this
import matplotlib.pyplot as plt
vis = pyLDAvis.gensim.prepare(lda_model, corpus, dictionary)
pyLDAvis.show(vis)
8
