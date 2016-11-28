
import pandas as pd
import re

# import libraries used for nlp
from __future__ import division
from nltk import word_tokenize
from nltk import FreqDist
from search_map import spell_check_dict

# load train, test, description and attributes files
train = pd.read_csv('./data/train.csv', index_col='id')
test = pd.read_csv('./data/test.csv', index_col='id')

description = pd.read_csv('./data/product_descriptions.csv')
attributes = pd.read_csv('./data/attributes.csv')


## Frequency Analysis
def default_tokenizer(sentence):
    return sentence.split(' ')

def tokenize(sentence, tokenizer_type='word'):
    if tokenizer_type == 'word':
        return word_tokenize(sentence)
    else:
        return default_tokenizer(sentence)

def tokenize_sentences(sentences, n):
    tokens = []
    
    for i in range(0, n):
        tokens.extend(tokenize(sentences[i]))
    
    return tokens

def frequency_analysis(search_terms, n=50, num_terms=5):
    tokens_list = tokenize_sentences(search_terms, n=n)
    fdist = FreqDist(tokens_list)
    
    return fdist.most_common(n=num_terms)
    

## Relevance scores based on different patterns on training corpus
def relevance_scores_by_pattern(train, pattern):
    query_list = [(idx, w) for (idx, w) in enumerate(train.search_term.values) if re.search(pattern, w)]
    relevance_scores = [train.iloc[idx]['relevance'] for (idx, w) in query_list]
    
    return relevance_scores


# Do spelling mistakes have an effect on relevance scores?
def spelling_mistakes_effect(train):
    train = train.copy()
    boolean_indicator = [1 if q in spell_check_dict else 0 for q in train.search_term]
    train['is_incorrect'] = boolean_indicator
    
    mean_relevance_score_correct = train[train.is_incorrect == 0].relevance.mean()    
    mean_relevance_score_incorrect = train[train.is_incorrect == 1].relevance.mean()    
        
    return mean_relevance_score_correct, mean_relevance_score_incorrect




