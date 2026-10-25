# libraries
install.packages('pdftools')
install.packages('tidytext')
install.packages('tidyverse')
install.packages('quanteda')
install.packages('wordcloud')
install.packages('widyr')
install.packages('ggraph')
install.packages('igraph')
install.packages('tm')

library(pdftools)
library(tidytext)
library(tidyverse)
library(quanteda)
library(wordcloud)
library(widyr)
library(ggraph)
library(igraph)

# get data
my.data <- data.frame(pdf_text("https://github.com/matthew-danna/justice-research/raw/main/article.pdf"))

# clean data
my.data$ID <- seq.int(nrow(my.data))
names(my.data) <- c("text", "page")
my.data$text <- as.character(my.data$text)

my.words <- my.data %>% unnest_tokens(word, text)
my.words$word.ID <- seq.int(nrow(my.words))

no.stops <- my.words %>% 
  anti_join(stop_words, by = "word")

tmp.clean.words <- no.stops[grep("^[[:digit:]]", 
                                 no.stops$word), ]

my.clean.words <- no.stops %>% 
  anti_join(tmp.clean.words, by = "word")

# sentiment
my.afinn <- my.clean.words %>% 
  inner_join(get_sentiments("afinn"))
my.bing <- my.clean.words %>%
  inner_join(get_sentiments("bing"))
my.nrc <- my.clean.words %>%
  inner_join(get_sentiments("nrc"))
my.sentiment <- my.bing

# word counts
counts.word <- my.clean.words %>% count(word, sort = TRUE)
counts.sentiment <- my.sentiment %>% 
  count(word, sentiment, sort = TRUE)
counts.word.top <- subset(counts.word, 
                          n >= quantile(counts.word$n, 0.95))

# bar graphs
ggplot(counts.word.top, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(counts.sentiment, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip()

# wordpairs
wordpairs.my.words <- my.clean.words %>%
  pairwise_count(word, page, sort = TRUE)

# network chart
set.seed(611)
pairs.plot <- wordpairs.my.words %>%
  filter(n >= 3 & n < 6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue") +
  ggtitle("Word Pair Chart") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
pairs.plot

