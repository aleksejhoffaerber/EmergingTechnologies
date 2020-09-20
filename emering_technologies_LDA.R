# 19-11-19

library(dplyr)
library(magrittr)
library(wordcloud)
library(wordcloud2)
library(tibble)
library(tm)
library(tidyr)
library(topicmodels)
library(slam)
library(RColorBrewer)
library(rowr)
library(ggplot2)
library(gridExtra)
library(reshape)

# asd
p1 <- "full"
p2 <- "nouns_adj"

# CREATING OVERVIEW TABLE
files  <- list.files(path = p1, recursive = T, full.names=F) %>% 
  as_tibble(.) %>% 
  mutate(p1 = paste0("/full/",value),
         p2 = paste0("/nouns_adj/",value)) %>% 
  separate(value, c("cik", "year", "month", "day"), "-") # separating column into multiple variables

length(unique(as.numeric(files$cik)))-length(unique(files$cik))
# checking uniqueness of given numeric ciks vs. uniqueness of given character ciks
# no difference indicates no duplicates

files$day <- gsub(".txt", "", files$day)
files$date <- paste0(files$year, "-", files$month, "-", files$day) # create a date column 

files %<>% 
  mutate_at(c("month", "day"), as.numeric) %>% # transformation of columns using a compound assignment pipe operator (%<>%)
  mutate_at(c("year"), as.integer) %>% 
  mutate_at(c("date"), as.Date) %>% 
  arrange(date, cik) 


h1 <- hist(files$year) # analysis should only include 2008 and 2009

corpus <- DocumentTermMatrix(Corpus(VectorSource(c(1)))) # creating a dummy dtm at first to assure function operation

# CREATING CORPUSES

corpus.creation <- function () {
  nouns.adj <- vector()
  
  for (i in 1:nrow(files)) { # loading full data 
    nouns.adj[i] <- toString(readLines(paste0(getwd(),files[i,"p1"])))
  }
  
  nouns.adj %<>% gsub(".*Summary(.+?)Proceeds.*", "\\1",.) %>% 
    gsub("(\t.+?,)", "",.) %>% # only interested in letters
    gsub("[^A-Za-z/// ]+", "", .) %>% # only including letters from english alphabet
    gsub("\\s+", " ",.) %>% # widespaces
    gsub("\\t+", " ",.) # to many tab-breaks 
  
  corpus <<- DocumentTermMatrix(Corpus(VectorSource(nouns.adj)),
                                     control = list(removePunctuation = T, # to not consider noise made by punctuation
                                                    stopwords = T, # stopwords, as those do not have any specific connection to the industry
                                                    tolower = T, # to not differentiate between specific captions, important because of differenet use of language
                                                    removeNumbers = T, # noise 
                                                    stemming = F, # to keep technology-specific contexts of keywords 
                                                    # weighting = weightBin, 
                                                    wordLengths = c(3,25), # to be in norm with the lecture --> going up to 25 would also be a good alternative
                                                    bounds = list(global = c(15,100))))
  

}

corpus.creation()

# CREATING VECTOR OF COMPANIES AND CIKS THAT NEED TO BE DELETED IN MAIN FILE:

a <- c(seq(1:nrow(corpus))) # storing the rows to be deleted
b <- c(rep(NA,nrow(corpus))) # storing amount of terms per company

to.del <- data.frame(a,b)

for (i in 1:nrow(corpus)) {
  to.del[i,2] <- row_sums(corpus[i,]) # calculating amount of words
}

to.del <- to.del[to.del[,2] <= 10,] # deleting entries that do not fulfill minimum term amount requirement

# ADJUSTING THE CORPUS
corpus <- corpus[row_sums(corpus) > 10,] # exclude everything from the corpus with less than 10 words 

# TRAINING TOPIC MODEL 
topic <- LDA(corpus,  # document term matrix
             k = 75, # specifify number of topics
             method = "Gibbs", # staying with Gibbs instead of VEM to reduce possible local minima
             control = list(
               seed = 1234, # eases replication
               burnin = 25,  # how often sampled before estimation recorded
               iter = 50,  # number of iterations
               keep = 1,    # saves additional data per iteration (such as logLik)
               save = F,     # saves logLiklihood of all iterations
               verbose = 10  # report progress
             ))

# TOPIC INVESTIGATION
topic@loglikelihood             
plot(topic@logLiks, type = "l") 

# TOPIC WORDS AND WORDCLOUD CREATION PER TOPIC 

topic.words <- function () {
  
  beta <- exp(topic@beta) # log of probability reported
  dim(beta)
  
  topic.terms <<- list() # empty list for topic words
  prob.top <<- list() # empty list for respective probabilities
  
  for (i in 1:topic@k) {
    
    topic.terms[[i]] <<- head(topic@terms[order(beta[i,], decreasing = T)], 30) # generating top words per topic
    prob.top[[i]] <<- head(sort(beta[i,], decreasing = T), 30)

  }
}

topic.words() # run wordcloud function


# ASSIGNING DOMINATING TOPICS PER COMPANY/CIK
gamma <- topic@gamma # creating the gamma overview for every company and the k topics
gamma %<>%  as.data.frame() %>%
  mutate(mak = do.call(pmax,.)) # maximizing the gamma values

colnames(gamma) <- c(seq(1:topic@k),"mak") 

gamma$topic <- as.numeric(colnames(gamma)[apply(gamma,1,which.max)]) # and deviating the topic for which gamma is maximized


# DELETING UNECESSARY ENTRIES
to.del <<- to.del[,-2] # delete columns counting the words per company in corpus
files <<- files[-to.del,] # deleting documents that had less than 10 terms from main file

files$topic <- gamma$topic # appending topic values
files$mak <- gamma$mak
files$id <- c(seq(1:nrow(files))) # creating ID for recreation 

files %<>% filter(mak > 0.25) 
h2 <- hist(files$year) # analysis should only include 2008 and 2009

# EMERGING TOPIC PLOT CREATION

topic.distribution <- files %>% # creating a sub-df i.o. to summarise per topic and year
  group_by(topic,year) %>% 
  summarise(sum = count(topic)) %>% 
  as.data.frame() %>% 
  mutate_at(c("year"),as.integer)

w <- c(4,23,29,38,59,65,70) # LDA, 15-100 bound, k = 75 with mak

create.plots <- function() 
  {
  plot <- list() # storing all plots in one list to call them into the do.call
  
  for (i in 1:topic@k) {
    if (i %in% w) {
      plot[[i]] <- filter(topic.distribution, topic == i) %>% # filter per current topic
        ggplot(aes(x= year, y = sum)) +
        geom_line(aes(colour = "royalblue4")) +
        geom_point(colour = "indianred4", size = 1) +
        ggtitle(paste0("Topic Nr.",i)) + 
        xlim(2008,2019) + # set x-axis to show years in focus
        theme(legend.position = "none", # deleting legends and axis titles
              plot.title = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
    }
    else {
      plot[[i]] <- filter(topic.distribution, topic == i) %>% # filter per current topic
        ggplot(aes(x= year, y = sum)) +
        geom_line(aes(colour = topic)) +
        geom_point(colour = "royalblue4", size = 1) +
        ggtitle(paste0("Topic Nr.",i)) + 
        xlim(2008,2019) + # set x-axis to show years in focus
        theme(legend.position = "none", # deleting legends and axis titles
              plot.title = element_text(size = 12),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank())
    }
  }
  do.call(grid.arrange, plot)
}

create.plots()

emerging.topics <- function(v) {
  par(mfrow=c(1,1), oma = c(0,0,0,0), mai = c(0,0,0,0)) # reset plotting space 
  for (i in w) {
    
    x <- topic.terms[[i]]
    y <- prob.top[[i]]
    
    wordcloud(words = x,
              freq = y,
              scale = c(4, .5),
              random.order = F, 
              rot.per = 0.5, # 90? degree rotation amount
              colors = brewer.pal(8, "Dark2")) # colour palette taking frequency into account
    
    dev.copy2pdf(file=paste0("7_Emerging Topics",i,".pdf"), width = 7, height = 5)
  }
  par(mfrow=c(1,1), oma = c(1,1,1,1), mai = c(1,1,1,1)) # reset plotting space 
}
emerging.topics(w)

a <- c(topic.terms[[4]], topic.terms[[31]]) # creating vector with key words for most apparent topics

# DEFINING TOPIC-COMPANY CORPUSAE FOR TRAINING AND TEST DATA SET -----

# IMPLEMENTING LOGLIKEHOOD MEASURE FOR KEYWORD VALIDAITON FOR RISING TOPICS -----
# CREATING WORDCOUNT TABLES FOR ALL TERMS FROM THE DTM

create.emerging.topics.rising <- function(y) {
  r <<- vector() 
  s <<- vector()
  
  for (i in w) {
    q <- unname(files %>% filter(topic == i & year < y) %>% select(id))[[1]] # filtering companies for train set
    r <<- c(r,q)}
  
  for (i in w) {
    u <- unname(files %>% filter(topic == i & year >= y) %>% select(id))[[1]] # filtering companies for test set
    s <<- c(u,s)}
  
  # CREATING TOPIC BASED VECTOR
  p <- vector() 
  
  for (i in w) {
    x <- topic.terms[[i]] # appending all terms that are topic specific
    p <- c(x,p)
  }
  
  corpus.train <- corpus[r,] # only include ciks according to filter above
  corpus.test <- corpus[s,] # 

  occ.train <- function () { 
    count.training <- sort(colSums(as.matrix(corpus.train)), decreasing = T) # create a corpus subset that sums all terms 
    names.training <- names(count.training) # split into naming 
    count.training <- unname(count.training) # and word occurence part
    word.occ.training <<- data.frame(names.training, count.training) # append as df and save as global
  }
  
  occ.test <- function () {
    count.test <- sort(colSums(as.matrix(corpus.test)), decreasing = T)
    names.test <- names(count.test)
    count.test <- unname(count.test)
    word.occ.test <<- data.frame(names.test, count.test)
  }
  
  occ.train()
  occ.test()
  
  # CREATING DATA FRAME FOR THE DIFFERENT CALCULATIONS
  
  # 1) create matrix with terms per emerging topic
  a1 <- matrix(ncol = length(w), nrow = length(topic.terms[[1]]))
  
  for (i in 1:length(w)) {
    for (j in 1:length(topic.terms[[i]])) {
      a1[j,i] <- topic.terms[[w[i]]][j] # for loop structure
    }
  }  
  a1 <- melt(a1)
  
  # 2) create a for data frame 
  a2 <- matrix(ncol = length(w), nrow = length(topic.terms[[1]]))
  
  for (i in 1:length(w)) {
    for (j in 1:length(topic.terms[[i]])) {
      a2[j,i] <- unname(word.occ.training %>% # extract only the occurence for j term in i emerging topic
                          filter(names.training == topic.terms[[w[i]]][j]) %>% 
                          select(count.training))[[1]]
    }
  }
  a2 <- melt(a2) # melt into long df
  
  # 3) create b for data frame
  a3 <- matrix(ncol = length(w), nrow = length(topic.terms[[1]]))
  
  for (i in 1:length(w)) {
    for (j in 1:length(topic.terms[[i]])) {
      if (length(unname(word.occ.test %>% # if resulting term is not found, place a zero
                        filter(names.test == topic.terms[[w[i]]][j]) %>% 
                        select(count.test))[[1]]) == 0)
        a3[j,i] <- 0
      else {
        a3[j,i] <- unname(word.occ.test %>% # extract only the occurence for j term in i emerging topic
                            filter(names.test == topic.terms[[w[i]]][j]) %>% 
                            select(count.test))[[1]]  
      }  
    }
  }
  a3 <- melt(a3)
  
  # 4) create c for data frame
  a4 <- unname(word.occ.training %>% mutate(sum = sum(count.training)) %>% select(sum))[[1,1]]
  
  # 5) create d for data frame
  a5 <- unname(word.occ.test %>% mutate(sum = sum(count.test)) %>% select(sum))[[1,1]]
  

  # COMBINING 1) + 5) INTO ONE DATA FRAME
  z <<- data.frame(a1,a2[,3],a3[,3],c(rep(a4,length(a1))),c(rep(a5,length(a1))))
  colnames(z) <<- c("term.in.topic", "topic", "term", "a", "b", "c", "d")
  
  z <<- z %>% mutate(E1 = c*(a+b)/(c+d),
                     E2 = d*(a+b)/(c+d),
                     LL = case_when(a == 0 ~ ((E2-E1)/E1),
                                    a < b ~ 2 *((a*log(a/E1))+(b*log(b/E2))),
                                    a > b ~ NaN))
}

# CREATING WORDCOUNT TABLES FOR ALL TERMS REGARDLESS OF TOPIC BEHAVIOR -----

create.emerging.topics.all <- function(y) {
  t <- unname(files %>% filter(year <= y) %>% select(id))[[1]] # topic unspecific filter
  u <- unname(files %>% filter(year > y) %>% select(id))[[1]]
  
  
  corpus.train.2 <<- corpus[t,] # exclude everything with less than 10 words 
  # corpus.train.2 <- corpus.train.2[, colnames(corpus.train.2) %in% p]
  corpus.test.2 <<- corpus[u,] # exclude everything with less than 10 words 
  # corpus.test.2 <- corpus.test.2[, colnames(corpus.test.2) %in% p]
  
  occ.train.2 <- function () {
    count.training <- sort(colSums(as.matrix(corpus.train.2)), decreasing = T) # create a corpus subset that sums all terms 
    names.training <- names(count.training) # split into naming 
    count.training <- unname(count.training) # and word occurence part
    word.occ.training.2 <<- data.frame(names.training, count.training) # append as df and save as global
  }
  
  occ.test.2 <- function () {
    count.test <- sort(colSums(as.matrix(corpus.test.2)), decreasing = T)
    names.test <- names(count.test)
    count.test <- unname(count.test)
    word.occ.test.2 <<- data.frame(names.test, count.test)
  }
  
  occ.train.2()
  occ.test.2()
  
  # CREATING DATA FRAME FOR THE DIFFERENT CALCULATIONS

  # 1) create matrix with terms per emerging topic
  a1 <- matrix(ncol = length(w), nrow = length(topic.terms[[1]]))
  
  for (i in 1:length(w)) {
    for (j in 1:length(topic.terms[[i]])) {
      a1[j,i] <- topic.terms[[w[i]]][j] # for loop structure
    }
  }  
  a1 <- melt(a1)
  
  # 2) create a for data frame 
  a2 <- matrix(ncol = length(w), nrow = length(topic.terms[[1]]))
  
  for (i in 1:length(w)) {
    for (j in 1:length(topic.terms[[i]])) {
      a2[j,i] <- unname(word.occ.training.2 %>% # extract only the occurence for j term in i emerging topic
                          filter(names.training == topic.terms[[w[i]]][j]) %>% 
                          select(count.training))[[1]]
    }
  }
  a2 <- melt(a2)
  
  # 3) create b for data frame
  a3 <- matrix(ncol = length(w), nrow = length(topic.terms[[1]]))
  
  for (i in 1:length(w)) {
    for (j in 1:length(topic.terms[[i]])) {
      if (length(unname(word.occ.test.2 %>% # if resulting term is not found, place a zero
                        filter(names.test == topic.terms[[w[i]]][j]) %>% 
                        select(count.test))[[1]]) == 0)
        a3[j,i] <- 0
      else {
        a3[j,i] <- unname(word.occ.test.2 %>% # extract only the occurence for j term in i emerging topic
                            filter(names.test == topic.terms[[w[i]]][j]) %>% 
                            select(count.test))[[1]]  
      }  
    }
  }
  a3 <- melt(a3)
  
  # 4) create c for data frame
  a4 <- unname(word.occ.training.2 %>% mutate(sum = sum(count.training)) %>% select(sum))[[1,1]]
  
  # 5) create d for data frame
  a5 <- unname(word.occ.test.2 %>% mutate(sum = sum(count.test)) %>% select(sum))[[1,1]]

  # COMBINING INTO ONE DATA FRAME
  z.2 <<- data.frame(a1,a2[,3],a3[,3],c(rep(a4,length(a1))),c(rep(a5,length(a1))))
  colnames(z.2) <<- c("term.in.topic", "topic", "term", "a", "b", "c", "d")
  
  z.2 <<- z.2 %>% mutate(E1 = c*(a+b)/(c+d),
                         E2 = d*(a+b)/(c+d),
                         LL = case_when(a == 0 ~ ((E2-E1)/E1),
                                        a < b ~ 2 *((a*log(a/E1))+(b*log(b/E2))),
                                        a > b ~ NaN))
}

# INVESTIGATING LOGLIKELIHOOD OF TOPICS -----
# filtering on topic 1 = biotech & topic 5 = information technology & systems

create.emerging.topics.rising(2010)
z %>% 
  filter(topic %in% c(1,5)) %>% 
  arrange(-LL)

create.emerging.topics.all(2010)
z.2 %>% 
  filter(topic %in% c(1,5)) %>% 
  arrange(-LL)

# CREATING A KWIC FOR EMERGING TECHNOLOGY EXTRACTION

y <- c(r,s) # vector containing documents classified as being within "emerging technologies"
nouns.adj.train <- vector() # creating corpus for KWIC
nouns.adj.test <- vector() # creating corpus for KWIC

for (i in 1:length(r)) { 
  nouns.adj.train[i] <- toString(readLines(paste0(getwd(),filter(files, id == r[i])[,"p2"]))) # filtering only on specific documents
}

for (i in 1:length(s)) { 
  nouns.adj.test[i] <- toString(readLines(paste0(getwd(),filter(files, id == s[i])[,"p2"]))) # filtering only on specific documents
}


nouns.adj.train %<>% gsub("(\t.+?,)", "",.) %>% # only interested in letters
    gsub("[^A-Za-z/// ]+", "", .) %>% # only including letters from english alphabet
    gsub("\\s+", " ",.) %>% # widespaces
    gsub("\\t+", " ",.) # to many tab-breaks 

nouns.adj.test %<>% gsub("(\t.+?,)", "",.) %>% # only interested in letters
    gsub("[^A-Za-z/// ]+", "", .) %>% # only including letters from english alphabet
    gsub("\\s+", " ",.) %>% # widespaces
    gsub("\\t+", " ",.) # to many tab-breaks 



kw.train <- list()
kw.test <- list()
# using kwic function from quanteda package in order to facilitate the function construction
kw.train <- kwic(nouns.adj.train, pattern = a, window = 1, valuetype = "glob", case_insensitive = T)
kw.test <- kwic(nouns.adj.test, pattern = a, window = 1, valuetype = "glob", case_insensitive = T)
# pattern = keywords indicating possible emerging technologies we want to validate

kw.train %<>% mutate(aggr = paste(pre, pattern, post, sep = " "))
kw.train %<>% mutate(left = paste(pre, pattern, sep = " "))
kw.train %<>% mutate(right = paste(pattern, post, sep = " "))
aggr.kwic.train <- kw.train %>% group_by(aggr) %>% summarise(sum = count(aggr)) %>% arrange(-sum)
left.kwic.train <- kw.train %>% group_by(left) %>% summarise(sum = count(left)) %>% arrange(-sum)
right.kwic.train <- kw.train %>% group_by(right) %>% summarise(sum = count(right)) %>% arrange(-sum)


kw.test %<>% mutate(aggr = paste(pre, pattern, post, sep = " "))
kw.test %<>% mutate(left = paste(pre, pattern, sep = " "))
kw.test %<>% mutate(right = paste(pattern, post, sep = " "))
aggr.kwic.test <- kw.test %>% group_by(aggr) %>% summarise(sum = count(aggr)) %>% arrange(-sum)
left.kwic.test <- kw.test %>% group_by(left) %>% summarise(sum = count(left)) %>% arrange(-sum)
right.kwic.test <- kw.test %>% group_by(right) %>% summarise(sum = count(right)) %>% arrange(-sum)


right.kw <- left_join(right.kwic.train, right.kwic.test, by = c("right")) %>% replace_na(list(sum.x = 0, sum.y = 0))
colnames(right.kw) <- c("term", "a", "b")  
right.kw %<>% 
  mutate(c = sum(a),
         d = sum(b),
         E1 = c*(a+b)/(c+d),
         E2 = d*(a+b)/(c+d),
         LL = case_when(a < b ~ 2 *((a*log(a/E1))+(b*log(b/E2))), # adjustment of loglik calculation i.o. to not look  
                        a > b ~ NaN)) %>% # at cases which are reduced in absolute occurence (a > b)
  arrange(-LL)

left.kw <- left_join(left.kwic.train, left.kwic.test, by = c("left")) %>% replace_na(list(sum.x = 0, sum.y = 0))
colnames(left.kw) <- c("term", "a", "b")  
left.kw %<>% 
  mutate(c = sum(a),
         d = sum(b),
         E1 = c*(a+b)/(c+d),
         E2 = d*(a+b)/(c+d),
         LL = case_when(a < b ~ 2 *((a*log(a/E1))+(b*log(b/E2))), # adjustment of loglik calculation i.o. to not look  
                        a > b ~ NaN)) %>% # at cases which are reduced in absolute occurence (a > b)
  arrange(-LL)

aggr.kw <- left_join(aggr.kwic.train, aggr.kwic.test, by = c("aggr")) %>% replace_na(list(sum.x = 0, sum.y = 0))
colnames(aggr.kw) <- c("term", "a", "b")  
aggr.kw %<>% 
  mutate(c = sum(a),
         d = sum(b),
         E1 = c*(a+b)/(c+d),
         E2 = d*(a+b)/(c+d),
         LL = case_when(a < b ~ 2 *((a*log(a/E1))+(b*log(b/E2))), # adjustment of loglik calculation i.o. to not look  
                        a > b ~ NaN)) %>% # at cases which are reduced in absolute occurence (a > b)
  arrange(-LL)

rm(aggr.kwic.test, left.kwic.test, right.kwic.test,
   aggr.kwic.train, left.kwic.train, right.kwic.train,
   kw.train, kw.test)
