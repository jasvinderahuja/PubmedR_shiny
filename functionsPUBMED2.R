#functions = Paperstally(searchfor, from, to)
#jasvinderahuja@gmail.com

library(tm)
library(tidytext)
library(wordcloud)
library(tidyverse)

authorcomp <- function(authorlisteutils) {
  authors <- authorlisteutils %>% 
                arrange(order) %>% 
                unite(Authors, LastName, Initials, sep = '_') %>% 
                select(Authors) %>% 
                str_c(collapse=NULL)              
  return(authors)
}
repairAuthors <- function(authrs) {
  a <- str_replace (authrs, '[c(]' , "")
  b <- str_replace_all (a, '[/"]' , "")
  c <- str_replace (b, c('\\)') , "")
  d <- str_replace (c, c('\\(') , "")
  return(d)
}

PaperID.yr <- function(searchfor="Lichten MJ", from=2000, to=2018, maxresults=1000) {
  require(RISmed) 
  require(dplyr)
  r <- EUtilsSummary(searchfor, 
                       type='esearch', 
                       db='pubmed', 
                       retmax = maxresults,
                       mindate=from, 
                       maxdate=to)
    fetch <- EUtilsGet(r, type = "efetch", db = "pubmed")
    PMIDtitleyr <- tibble(title = fetch@ArticleTitle,
                              Authors = sapply(fetch@Author, authorcomp),
                              abstract = fetch@AbstractText,
                              journal = fetch@Title,
                              PMID = fetch@PMID, 
                              year = fetch@YearPubmed)
  abstracts <- PMIDtitleyr %>% rowwise() %>% mutate(abstract = as.character(abstract), PMID = as.character(PMID), Authors = repairAuthors(Authors))
  
  return(abstracts)
}
fetchwordcloud <- function(abstracts) {
  from <- min(abstracts$year)
  to <- max(abstracts$year)
  cloud <- abstracts %>% 
    unnest_tokens(word, abstract) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
  cloud %>% 
    with(wordcloud(word, n, min.freq = 10, max.words = 1000, random.order = FALSE,
                   colors = brewer.pal(8, "Dark2")), scale = c(3.5,0.25), per.rot = 0.4)
  return(cloud)
}
fetchbigrams <- function(abstracts){
  from <- min(abstracts$year)
  to <- max(abstracts$year)
  ds_bigrams <- abstracts %>%
    unnest_tokens(ngram, abstract, token = "ngrams", n=2) %>%
    count(ngram, sort = TRUE)
  
  bigrams_separated <- ds_bigrams %>%
    separate(ngram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) 
  
  bigrams_filtered %>% 
    filter(word1 != "0")  %>%
    count(word1, word2) %>%
    arrange(-nn)
  
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ") 
  
  bigrams_united %>%
    filter(n > 250) %>%
    arrange(-n) %>%
    ggplot(aes(reorder(bigram, n), n)) +
    geom_point() +
    coord_flip() +
    theme(axis.text.x = element_text(size  = 10)) +
    labs(title = "Bigrams with frequency count > 250", x = "")
  
  bigrams_united %>%
    with(wordcloud(bigram, n, max.words = 200, random.order = FALSE, colors =      
                     brewer.pal(10, "Dark2"), scale = c(3.5,0.25)), per.rot = 0.4)  
  return(bigrams_united)
}
fetchjournal <- function(abstracts){
  from <- min(abstracts$year)
  to <- max(abstracts$year)
  cloud3 <- abstracts %>% 
    select(journal) %>%
    group_by(journal) %>%
    count(sort = TRUE)
  
  cloud3 %>%
    with(wordcloud(journal, n, min.freq = 1, random.order = FALSE, 
                   max.words = 80, colors = brewer.pal(10, "Dark2"), scale = c(3.5,0.25)), rot.per = 0.4)
  return(cloud3)
}
fetchauthorcloud <- function(abstracts) {
  cloud <- abstracts %>% select(PMID, Authors) %>%
    unnest_tokens(authorship, Authors, token = "regex", pattern=",") %>% 
    mutate(authorship = str_trim(authorship)) %>%
    count(authorship, sort = TRUE)
  cloud %>% 
    with(wordcloud(authorship, n, min.freq = 1, random.order = FALSE, max.words = 200,
                   colors = brewer.pal(4, "Dark2"), scale = c(3,1)), per.rot = 0.6)
  return(cloud)
}
first.word <- function(my.string, separator){
  return(str_trim(unlist(strsplit(my.string, separator))[1]))
}
plotPubmed <- function(pubmedResults, srch){
  pubmedResults %>%
    group_by(year) %>%
    count() %>%
    ggplot(aes(year, n)) +
    geom_bar(stat = 'identity') +
    labs(title = 
           paste0("Pubmed articles with search for ", 
                  srch, " \n", min(pubmedResults$year), "-", max(pubmedResults$year)), 
         hjust = 0.5,
         y = "Articles") 
}