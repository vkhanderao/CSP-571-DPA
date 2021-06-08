##Load neccessary packages
#library(rlang)
#install.packages("scales")
#install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.7.tar.gz", repo=NULL, type="source")
#library(rlang)
#library(scales)
library(tidyverse) # metapackage with lots of helpful functions
library(reshape2)
#install.packages("ggrepel") 
library(ggrepel)
library(plotly)
library(lubridate)
library(data.table)
library(recommenderlab)
library(reshape2)

options(warn=-1)

NetflixDF <-  read.csv(file = "C:/Users/Soumy/Desktop/DPA/netflix/netflix_titles.csv", na.strings = c("NA", ""), stringsAsFactors=F)

##Fetch the weighted average votes/ratings of the movies from imdb for netflix

RatingsIMDB=fread("C:/Users/Soumy/Desktop/DPA/netflix/IMDb ratings.csv", select = c("weighted_average_vote"))
TitlesIMDB=fread("C:/Users/Soumy/Desktop/DPA/netflix/IMDb movies.csv", select = c('title','year','genre','reviews_from_users'))


IMDBdf = data.frame(RatingsIMDB, TitlesIMDB)
head(IMDBdf,5)

#drop duplicated rows based on the title, Release Year, Rating
IMDBdf=distinct(IMDBdf,title,year,weighted_average_vote,genre, .keep_all= TRUE)


IMDBdf %>% drop_na("weighted_average_vote")

library(dplyr)
###Join netflix and imdb dataset
jointDF=inner_join(NetflixDF, IMDBdf, by = c("title"))
head(jointDF,1)


(dim(jointDF))

#number of missing values for each variable for NetflixDF
data.frame("variable"=c(colnames(NetflixDF)), "missing values"=sapply(NetflixDF, function(x) sum(is.na(x))), row.names=NULL)


NetflixDF$date_added <- as.Date(NetflixDF$date_added, format = "%B %d, %Y")


#function to find a mode
getmode <- function(v) {
   unique_mode <- unique(v)
   unique_mode[which.max(tabulate(match(v, unique_mode)))]
}

###Fill by mode
NetflixDF$rating[is.na(NetflixDF$rating)] <- getmode(NetflixDF$rating)

#drop duplicated rows based on the title, country, type and release_year
NetflixDF=distinct(NetflixDF,title,country,type,release_year, .keep_all= TRUE)


Type_amount <- NetflixDF %>% group_by(type) %>% summarise(
  count = n()
)

fig1 <- plot_ly(Type_amount, labels = ~type, values = ~count, type = 'pie', marker = list(colors = c("#9467bd", "#1f77b4")))
fig1 <- fig1 %>% layout(title = 'Amount Of Netflix Content By Type')
fig1

s <- strsplit(NetflixDF$country, split = ", ")
Total_titles_countries <- data.frame(type = rep(NetflixDF$type, sapply(s, length)), country = unlist(s))
Total_titles_countries$country <- as.character(gsub(",","",Total_titles_countries$country))


country_amount <- na.omit(Total_titles_countries) %>%
  group_by(country, type) %>%
  summarise(count = n())

w <- reshape(data=data.frame(country_amount),idvar="country",
                          v.names = "count",
                          timevar = "type",
                          direction="wide") %>% arrange(desc(count.Movie)) %>%
                          top_n(10)
names(w)[2] <- "count_movie"
names(w)[3] <- "count_tv_show"
w <- w[order(desc(w$count_movie+w$count_tv_show)),] 

fig <- plot_ly(w, x = w$country, y = ~count_movie, type = 'bar', name = 'Movie', marker = list(color = '#9467bd'))
fig <- fig %>% add_trace(y = ~count_tv_show, name = 'TV Show',marker = list(color = '#1f77b4'))
fig <- fig %>% layout(xaxis=list(categoryorder = "array", categoryarray = w$country, title="Country"), yaxis = list(title = 'Amount of content'), barmode = 'stack', title = 'Top Countries distribution of the produced content')
fig

unique_vals <- apply(NetflixDF, MARGIN = 2, FUN = function(x) length(unique(x)))
unique_vals <- data.frame(Columns = names(unique_vals), UniqueCounts = unique_vals, stringsAsFactors = F)

options(repr.plot.width=10, repr.plot.height=8)
unique_vals %>% ggplot(aes(x = Columns, y = UniqueCounts)) + 
                       geom_bar(stat = 'identity',fill="#56B4E9") + 
                       scale_x_discrete(limits = colnames(NetflixDF)) + 
                       geom_hline(yintercept = nrow(NetflixDF)) + 
                       geom_label(aes(x = 4, y = nrow(NetflixDF), label = 'Number of rows in the input'), size = 5) + theme_bw()

DF_bydate <- NetflixDF %>% group_by(date_added,type) %>% summarise(addedToday = n()) %>% 
            ungroup() %>% group_by(type) %>% mutate(Total_Number_of_Shows = cumsum(addedToday), label = if_else(date_added == max(date_added,na.rm = T), as.character(type), NA_character_))

DF_bydateTotal <- NetflixDF %>% group_by(date_added) %>% summarise(added_today = n()) %>% 
  mutate(total_number_of_content = cumsum(added_today), type = "Total")

DF_bydate <- NetflixDF %>% group_by(date_added,type) %>% summarise(added_today = n()) %>% 
            ungroup() %>% group_by(type) %>% mutate(total_number_of_content = cumsum(added_today))

TotalDF<- rbind(as.data.frame(DF_bydateTotal), as.data.frame(DF_bydate))



fig4 <- plot_ly(TotalDF, x = ~date_added, y = ~total_number_of_content, color = ~type, type = 'scatter', mode = 'lines', colors=c("#9467bd","#ff7f0e","#1f77b4")) 
fig4 <- fig4 %>% layout(yaxis = list(title = 'Total number of Shows or Movies'), xaxis = list(title = 'Date content was added'), title="Amout Of Content As A Function Of Time")
fig4

Total_Rating_DF <- NetflixDF %>% group_by(rating) %>% summarise(count = n())

fig6 <- plot_ly(Total_Rating_DF, labels = ~rating, values = ~count, type = 'pie')
fig6 <- fig6 %>% layout(title = 'Amount Of Content By Rating')

fig6

topGenr <- strsplit(NetflixDF$listed_in, split = ",")
listedTitles <- data.frame(type = rep(NetflixDF$type, sapply(topGenr, length)), listed_in = unlist(topGenr))
listedTitles$listed_in <- as.character(gsub(",","",listedTitles$listed_in))

ListedtotalDF <- listedTitles %>% group_by(listed_in) %>% summarise(count = n()) %>%
  arrange(desc(count)) %>% top_n(20)

fig7 <- plot_ly(ListedtotalDF, x = ~listed_in, y = ~count, type = 'bar', marker = list(color = 'palegreen'))
fig7 <- fig7 %>% layout(xaxis=list(categoryorder = "array", categoryarray = ListedtotalDF$listed_in, title="Genre"), yaxis = list(title = 'Count'), title="20 Top Genres On Netflix")

fig7

Country_moviesbyduration<-na.omit(NetflixDF[NetflixDF$type=="Movie",][,c("country", "duration")])
countrydur_s <- strsplit(Country_moviesbyduration$country, split = ", ")
Country_moviesbydurationTotal <- data.frame(duration = rep(Country_moviesbyduration$duration, sapply(countrydur_s, length)), country = unlist(countrydur_s))
Country_moviesbydurationTotal$duration <- as.numeric(gsub(" min","", Country_moviesbydurationTotal$duration))


Subset_Country_moviesbydurationTotal<-Country_moviesbydurationTotal[Country_moviesbydurationTotal$country %in% c("United States", "India", "United Kingdom", "Canada", "France", "Japan", "Spain", "South Korea", "Mexico", "Australia", "Taiwan"),]


fig8 <- plot_ly(Subset_Country_moviesbydurationTotal, y = ~duration, color = ~country, type = "box")
fig8 <- fig8 %>% layout(xaxis=list(title="Country"), yaxis = list(title = 'Duration (in min)'), 
        title="Box-Plots Of Movie Duration In Top 11 Countries")

fig8

ShowCategoriesDF <- NetflixDF %>% 
                        select(c('show_id','type','listed_in')) %>% 
                        separate_rows(listed_in, sep = ',') %>%
                        rename(Show_Category = listed_in)
ShowCategoriesDF$Show_Category <- trimws(ShowCategoriesDF$Show_Category)
head(ShowCategoriesDF)

ShowCategoriesDF %>% mutate(Show_Category = fct_infreq(Show_Category)) %>% 
        ggplot(aes(x = Show_Category)) + 
            geom_bar() + scale_x_discrete() + facet_wrap(~type, scales = 'free_x') + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme() + coord_cartesian(xlim = c(1,20))


UniqCategoriesDF <- ShowCategoriesDF %>% group_by(type,Show_Category) %>%  summarise()

MoviesCategories_CorrDF <- data.frame(expand_grid(type = 'Movie', 
                                             Category1 = subset(UniqCategoriesDF, type == 'Movie')$Show_Category,
                                             Category2 = subset(UniqCategoriesDF, type == 'Movie')$Show_Category))

         

TVCategories_CorrDF <-      data.frame(expand_grid(type = 'TV Show', 
                                             Category1 = subset(UniqCategoriesDF, type == 'TV Show')$Show_Category,
                                             Category2 = subset(UniqCategoriesDF, type == 'TV Show')$Show_Category))


Categories_CorrDF <- rbind(MoviesCategories_CorrDF,TVCategories_CorrDF)
Categories_CorrDF$matched_count <- apply(Categories_CorrDF, MARGIN = 1,FUN = function(x) {
                                            length(intersect(subset(ShowCategoriesDF, type == x['type'] & Show_Category == x['Category1'])$show_id,
                                            subset(ShowCategoriesDF, type == x['type'] & Show_Category == x['Category2'])$show_id))})


Categories_CorrDF <- subset(Categories_CorrDF, (as.character(Category1) < as.character(Category2)) & (matched_count > 0))


options(repr.plot.width=20, repr.plot.height=10)           

ggplot(subset(Categories_CorrDF, type == 'Movie'), aes(x = Category1, y = Category2, fill = matched_count)) + 
        geom_tile() + facet_wrap( ~type, scales = 'free') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_distiller(palette = "Spectral") + 
           theme(legend.text = element_text(size = 14), legend.title = element_text(size = 20))

ggplot(subset(Categories_CorrDF, type == 'TV Show'), aes(x = Category1, y = Category2, fill = matched_count)) + 
        geom_tile() + facet_wrap( ~type, scales = 'free') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_distiller(palette = "Spectral") + 
            theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

###Separating the list of comma separated actor lists

NetflixDF %>% select(c('show_id','cast','director')) %>% 
        gather(key = 'role', value = 'person', cast, director) %>% 
             filter(person != "") %>% separate_rows(person, sep = ',') -> Show_personDF

Show_personDF$person <- trimws(Show_personDF$person)
head(Show_personDF)

FreqPersonDF<- Show_personDF %>% group_by(person,role) %>% 
                    summarise(count = n()) %>% arrange(desc(count))

FreqPersonDF %>% group_by(role) %>% top_n(10,count) %>% ungroup() %>% ggplot(aes(x = fct_reorder(person,count,.desc = T), y = count, fill = role)) + 
            geom_bar(stat = 'identity') + scale_x_discrete() + facet_wrap(~role, scales = 'free_x') + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position = 'none') + labs(x = 'Name of the actor / director')

topDir <- strsplit(NetflixDF$director, split = ", ")
DirectorTitles <- data.frame(type = rep(NetflixDF$type, sapply(topDir, length)), director = unlist(topDir))
DirectorTitles$director <- as.character(gsub(",","",DirectorTitles$director))
DirectorTitles<-na.omit(DirectorTitles) %>%  group_by(director)  %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(10)
DirectorTitles<-as.data.frame(DirectorTitles)
DirectorTitles

dir_<-ggplot(data=DirectorTitles, aes(x=director, y=count)) +
  geom_bar(stat="identity", fill="rosybrown")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=5.5)+
  theme_classic()
dir_

topActr <- strsplit(NetflixDF[NetflixDF$country=="United States",]$cast, split = ", ")
ActorTitles <- data.frame(type = rep(NetflixDF[NetflixDF$country=="United States",]$type, sapply(topActr, length)), actor = unlist(topActr))
ActorTitles$actor <- as.character(gsub(",","",ActorTitles$actor))
ActorTitles<-na.omit(ActorTitles) %>%  group_by(actor)  %>% summarise(count = n()) %>% arrange(desc(count)) %>% top_n(10)
ActorTitles<-as.data.frame(ActorTitles)
ActorTitles


actr_<-ggplot(data=ActorTitles, aes(x=actor, y=count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=5.5)+
  theme_classic()
actr_

##Convert rating to factor level
NetflixDF$rating <- as.numeric(factor(NetflixDF$rating))

## "type" and "Listed_in" should be categorical variable
NetflixDF$listed_in <- as.numeric(factor(NetflixDF$listed_in))
NetflixDF$type <- as.numeric(factor(NetflixDF$type))


NetflixDF$rating[is.na(NetflixDF$rating)] <- getmode(NetflixDF$rating)

summary(NetflixDF)

#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Load the data as a corpus
Netflixdocs <- Corpus(VectorSource(jointDF$genre))

inspect(Netflixdocs)

###Text Transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
Netflixdocs <- tm_map(Netflixdocs, toSpace, "/")
Netflixdocs <- tm_map(Netflixdocs, toSpace, "@")
Netflixdocs <- tm_map(Netflixdocs, toSpace, "\\|")

###Clean text
# Convert the text to lower case
Netflixdocs <- tm_map(Netflixdocs, content_transformer(tolower))
# Remove numbers
Netflixdocs <- tm_map(Netflixdocs, removeNumbers)
# Remove english common stopwords
Netflixdocs <- tm_map(Netflixdocs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
Netflixdocs <- tm_map(Netflixdocs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
Netflixdocs <- tm_map(Netflixdocs, removePunctuation)
# Eliminate extra white spaces
Netflixdocs <- tm_map(Netflixdocs, stripWhitespace)

###create Term document
dtm <- TermDocumentMatrix(Netflixdocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
dir_ <- data.frame(word = names(v),freq=v)
head(dir_, 10)

set.seed(05)
wordcloud(words = dir_$word, freq = dir_$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(10, "Dark2"))

##Explore frequent terms and their associations
findFreqTerms(dtm, lowfreq = 4)

## Analyze the association between frequent terms (i.e., terms which correlate) using findAssocs() function
findAssocs(dtm, terms = "thriller", corlimit = 0.1)

library(dplyr)
library(janeaustenr)
library(tidytext)

Netflix_words <- jointDF %>%
  unnest_tokens(word, description) %>%
  count(title, word, sort = TRUE)
##netflix-title,description-text

total_words <- Netflix_words %>% group_by(title) %>% summarize(total = sum(n))
Netflix_words <- left_join(Netflix_words, total_words)
Netflix_words

 ###Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common.
###https://cran.r-project.org/web/packages/tidytext/vignettes/tf_idf.html
Netflix_words <- Netflix_words %>%
  bind_tf_idf(word, title, n)
head(Netflix_words,10)

Netflix_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

Netflix_words %>%
  filter(title == "One Day") %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#############################################################################################################################

###Columns in the joined data of Netflix and Imdb
colnames(jointDF)

Netflix_rating_data<-subset(jointDF, select = c("reviews_from_users","show_id","weighted_average_vote"))
Netflix_movie_data<-subset(jointDF, select = c("title","description","show_id","genre"))



movie_genre <- as.data.frame(Netflix_movie_data$genre, stringsAsFactors=FALSE)


movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[,]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE) 


colnames(movie_genre2) <- c(1:3)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_DF1 <- matrix(0,2332,18)
genre_DF1[1,] <- list_genre
colnames(genre_DF1) <- list_genre
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_DF1[1,] == movie_genre2[index,col]) 
    genre_DF1[index+1,gen_col] <- 1
}
}

genre_DF2 <- as.data.frame(genre_DF1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_DF2)) {
  genre_DF2[,col] <- as.integer(genre_DF2[,col]) #convert from characters to integers
} 
str(genre_DF2)

Mtrx_Search <- cbind(Netflix_movie_data[,1:2], genre_DF2[])
head(Mtrx_Search)

Mtrx_Rating <- dcast(Netflix_rating_data, reviews_from_users~show_id, value.var = "weighted_average_vote", na.rm=FALSE,fun=mean)
##Netflix_rating_data<-subset(jointDF, select = c("reviews_from_users","show_id","weighted_average_vote"


Mtrx_Rating <- as.matrix(Mtrx_Rating[,-1]) #remove userIds/reviews from users

Mtrx_Rating <- as(Mtrx_Rating, "realRatingMatrix")#Convert rating matrix into a recommenderlab sparse matrix
Mtrx_Rating

recommnd_ml <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommnd_ml)


recommnd_ml$IBCF_realRatingMatrix$parameters

mtrx_similarity <- similarity(Mtrx_Rating[1:4, ],
                               method = "cosine",
                               which = "users")
as.matrix(mtrx_similarity)
image(as.matrix(mtrx_similarity), main = "User's Similarities")

SImilarityMovie <- similarity(Mtrx_Rating[, 30:40], method =
                                 "cosine", which = "items")
as.matrix(SImilarityMovie)
image(as.matrix(SImilarityMovie), main = "Movies similarity")

ratingVals <- as.vector(Mtrx_Rating@data)
unique(ratingVals) # extracting unique ratings

Rating_Tbl <- table(ratingVals) # creating a count of movie ratings
Rating_Tbl

RatingMovie <- Mtrx_Rating[rowCounts(Mtrx_Rating) > 1,
                             colCounts(Mtrx_Rating) > 1]
RatingMovie

DFSampled<- sample(x = c(TRUE, FALSE),
                      size = nrow(RatingMovie),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
trn_DF <- RatingMovie[DFSampled, ]
tst_DF <- RatingMovie[!DFSampled, ]

recommnd_SM <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommnd_SM$IBCF_realRatingMatrix$parameters

recommnd_model <- Recommender(data = trn_DF,
                          method = "IBCF",
                          parameter = list(k = 30))
recommnd_model
class(recommnd_model)

infoModel <- getModel(recommnd_model)
class(infoModel$sim)
dim(infoModel$sim)
top_items <- 20

#image(infoModel$sim[1:top_items, 1:top_items],main = "Heatmap of the first rows and columns")

sum_rows <- rowSums(infoModel$sim > 0)
table(sum_rows)
sum_cols <- colSums(infoModel$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

TopRecomndtns <- 10 # the number of items to recommend to each user
PredRecomndtns <- predict(object = recommnd_model,
                          newdata = tst_DF,
                          n = TopRecomndtns)
PredRecomndtns

user1 <- PredRecomndtns@items[[1]] # recommendation for the first user
movies_user1 <- PredRecomndtns@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(Netflix_movie_data,
                                         Netflix_movie_data$show_id== movies_user1[index])$title)
}
movies_user2

user10 <- PredRecomndtns@items[[10]] # recommendation for the tenth user
movies_user1 <- PredRecomndtns@itemLabels[user10]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(Netflix_movie_data,
                                         Netflix_movie_data$show_id== movies_user1[index])$title)
}
movies_user2

recommendation_matrix <- sapply(PredRecomndtns@items,
                      function(x){ as.integer(colnames(RatingMovie)[x]) }) # matrix with the recommendations for each user

recommendation_matrix[1:10]

DF<-na.omit(jointDF)
str(DF)

DF <- as.data.frame(unclass(jointDF))  ###convert to factor levels
str(DF)

#number of missing values for each variable for NetflixDF
data.frame("variable"=c(colnames(DF)), "missing values"=sapply(DF, function(x) sum(is.na(x))), row.names=NULL)


## when the whole dataset is used for fitting a linear model
movies_model1 = lm(weighted_average_vote ~ rating + genre + director + reviews_from_users + country + listed_in + duration, data = DF,drop.unused.levels = TRUE)
                     
summary(movies_model1)

library (caret) 
inTrain = createDataPartition (DF[,'weighted_average_vote'], p=0.8, list= FALSE ) 
training = DF[inTrain,] 
validation = DF[-inTrain,] 

### When the training set is used for  running the model                
movie.linear = lm(weighted_average_vote ~ rating + genre  + reviews_from_users + country + listed_in + duration,data = training,drop.unused.levels = TRUE)

summary(movie.linear )

#predicted.linear = predict(movie.linear, validation)*10


#install.packages("gbm")
library(gbm)

movie.boost=gbm(weighted_average_vote ~ rating + genre  + reviews_from_users + country + listed_in + duration,data = training,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(movie.boost)


predicted.boost=predict(movie.boost,newdata=validation,n.trees=5000)


# calculate the sum of squared error
sum((validation$weighted_average_vote - predicted.boost)^2, na.rm=T)
