#######################
# DATA PRE-PROCESSING #
#######################

# split data into paragraphs
for (i in (1:nrow(sotu_database))){
  splitted_text <- str_split(iconv(sotu_database$address[i],"WINDOWS-1252","UTF-8"), pattern = '\n')[[1]]
  temp_paragraph_data <- as.data.frame(splitted_text) %>%
    rename(paragraph = splitted_text) %>%
    filter(nchar(paragraph)>0) %>%
    mutate(par_id = row_number(),
           president = sotu_database$president[i],
           year = sotu_database$year[i],
           years_active = sotu_database$years_active[i],
           party = sotu_database$party[i],
           sotu_type = sotu_database$sotu_type[i]) %>%
    select(president, year, years_active, party, sotu_type, par_id, paragraph)
  
  if (i==1){
    sotu_paragraph <- temp_paragraph_data
  } else {
    sotu_paragraph <- sotu_paragraph %>%
      full_join(temp_paragraph_data)
  }
}
sotu_paragraph <- sotu_paragraph %>%
  mutate(doc_id = row_number())

# manipulate the corpus
corpus <- sotu_paragraph %>%
  mutate(text = paragraph) %>%
  select(doc_id, text, president) %>%
  as.data.frame() %>%
  DataframeSource() %>%
  Corpus()
processedCorpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, as.character(stop_words$word)) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>%
  tm_map(removeNumbers) %>%
  #  tm_map(stemDocument, language = "en") %>%
  tm_map(lemmatize_strings) %>%
  tm_map(stripWhitespace)

#######################
# CONSTRUCT THE MODEL #
#######################

# Prepare Model
minimumFrequency <- 3
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
sotu_paragraph <- sotu_paragraph[sel_idx, ]


# LDA

# number of topics
K <- 7
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.5))

# get the topics
#terms(topicModel, 15)
topic_table <- as.data.frame(terms(topicModel, 15))

###################
# VIEWING RESULTS #
###################

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms
rowSums(beta)            # rows in beta sum to 1
nDocs(DTM)               # size of collection

# for every document we have a probaility distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics
rowSums(theta)[1:10]     # rows in theta sum to 1

# get 15 terms with the highest probability from each topic
exampleTermData <- terms(topicModel, 15)
exampleTermData[, 1:K]


top6termsPerTopic <- terms(topicModel, 6) # get the top 6 terms from each topic
topicNames <- apply(top6termsPerTopic, 2, paste, collapse=" ") # get topic names
topicNames

# function to generate a wordcloud for the selected topic
f_wordCloud <- function(topicNum){
  library(pals)
  library(wordcloud)
  # visualize topics as word cloud
  topicToViz <- topicNum # change for your own topic of interest
  #  topicToViz <- grep(keyWord, topicNames)[1] # Or select a topic by a term contained in its name
  # select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
  top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
  words <- names(top40terms)
  # extract the probabilites of each of the 40 terms
  probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
  # visualize the terms as wordcloud
  mycolors <- brewer.pal(8, "Dark2")
  return(wordcloud(words, probabilities, random.order = FALSE, color = mycolors))
}

f_wordCloud(1)
f_wordCloud(2)
f_wordCloud(3)
f_wordCloud(4)
f_wordCloud(5)
f_wordCloud(6)
f_wordCloud(7)


########################
# COMPILE OUTPUT TABLE #
########################

library(data.table)

# get character numbers by paragraph
sotu_topicmodel_output <- sotu_paragraph %>%
  as.data.frame() %>%
  mutate(charNum = nchar(paragraph)) %>%
  select(-paragraph)

#exampleIds <- sotu_filtered$doc_id

# get topic proportion table
topicProportionTable <- theta
colnames(topicProportionTable) <- topicNames

# pivot topicProportionTable
topicProportionTable <- melt(
  as.data.table(cbind(data.frame(topicProportionTable), doc_id = sotu_topicmodel_output$doc_id)),
  variable.name = "topic", id.vars = "doc_id")

# compile paragraph level table
sotu_topicmodel_output <- sotu_topicmodel_output %>%
  left_join(topicProportionTable, by = c('doc_id'))

# compile speech level table
sotu_topicmodel_output_aggregated <- sotu_topicmodel_output %>%
  mutate(weighted_value = charNum*value) %>%
  group_by(president, year, years_active, party, sotu_type, topic) %>%
  summarise(weighted_value = sum(weighted_value), charNum=sum(charNum)) %>%
  ungroup()
sotu_topicmodel_output_aggregated <- sotu_topicmodel_output_aggregated %>%
  mutate(value = weighted_value/charNum) %>%
  select(-weighted_value)


write.csv(sotu_topicmodel_output, 
          file = file.path(dir_data, "sotu_topicmodel.csv"), row.names = FALSE)
write.csv(sotu_topicmodel_output_aggregated, 
          file = file.path(dir_data, "sotu_topicmodel_aggregated.csv"), row.names = FALSE)
