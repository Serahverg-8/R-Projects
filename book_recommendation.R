

#####loading the packages required
library(recommenderlab)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)
library(tidyr)

#### loading the data sets.

read.csv("C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Book Recommendation\\books.csv") -> books

read.csv("C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Book Recommendation\\ratings.csv") -> ratings
read.csv("C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Book Recommendation\\book_tags.csv") -> book_tags
read.csv("C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Book Recommendation\\tags.csv") -> tags

### taking look at the data
head(books)
View(ratings)
View(book_tags)
View(tags)

# data cleansing

# to find and remove duplicate ratings
#new column
ratings %>%group_by(user_id, book_id) %>% mutate(N=n()) -> ratings
head(ratings)
View(ratings)
table(ratings$N)

##filter duplicate ratings
ratings %>% filter(N>1) -> duplicate_ratings
View(duplicate_ratings)
nrow(duplicate_ratings)


##filter ratings where N=1
ratings %>% filter(N==1) -> ratings


## remove the users rated fewer than 3 books

ratings %>% group_by(user_id) %>% mutate(Ratings_given = n()) -> ratings
View(ratings)

ratings %>% filter(Ratings_given > 2) -> ratings
range(ratings$Ratings_given)


#### DAta Exploration 
# select the sample data from the data set
set.seed(1)
user_fraction <- 0.02
users <- unique(ratings$user_id)
length(users)
sample_users <- sample(users,round(user_fraction * length(users)))
length(sample_users)

ratings %>% filter(user_id %in% sample_users) -> ratings 
nrow(ratings)

## plot the distribution of ratings

ratings%>%
  ggplot(aes(x = rating, fill = factor(rating))) + geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE)

### number of ratings of each book

ratings %>% 
  group_by(book_id) %>% 
  summarize(number_of_ratings_per_book = n()) -> count_ratings

count_ratings %>%ggplot(aes(number_of_ratings_per_book)) + geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,40))


##for any book the number of ratings per book is less than ten

### find the number of genres

genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Cookbooks", "Crime", "Fantasy", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))
length(genres)
available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
length(available_genres)
#out of 36 27 we have in the data set
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]

##plotting the number of genres
#genres are identifies bytag id's

#count of genres
tmp <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>%
  summarize(n = n()) %>%
  ungroup()

#find percentage



tmp <- tmp %>% mutate(sumN = sum(n), percentage = n / sumN) %>%
  arrange(-percentage) %>%
  left_join(tags, by = "tag_id")  

tmp %>% 
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = 'YlOrRd') + labs(y = 'Percentage', x = 'Genre')

#Find the top 10 books with highest ratings

books %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(title, ratings_count, average_rating) -> top_10

#Find the 10 most popular books

books  %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(title, ratings_count, average_rating) -> top_10_popular

#### restructure the data for recomendation 

dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
ratingmat[,-1] -> ratingmat
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)

######converting the rating matrix into a real rating matrix

ratingmat0 <- ratingmat  
dim(ratingmat0)
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

##########Splitting the data into train & test

sample(x=c(T,F),size=nrow(real_ratings),replace = T, prob = c(0.9,0.2)) ->split_book
real_ratings[split_book,]->recc_train
real_ratings[!split_book,]->recc_test

#Building the ubcf model
Recommender(data = recc_train,method="IBCF")->recc_model_ibcf
n_recommended_ibcf<-6

#Recommending books
predict(object=recc_model_ibcf,newdata=recc_test,n=n_recommended_ibcf)->recc_predicted_ibcf

############Recommeding books for user number-1
recc_predicted_ibcf@items[[1]]->user1_book_numbers
recc_predicted_ibcf@itemLabels[user1_book_numbers]

books %>% filter(id==3419) %>% select(original_title,authors)


#####################Recommending books for user number-5

recc_predicted_ibcf@items[[6]]->user5_book_numbers
recc_predicted_ibcf@itemLabels[user5_book_numbers]

books %>% filter(id==4624) %>% select(original_title,authors)
books %>% filter(id==6867) %>% select(original_title,authors)
books %>% filter(id==7326) %>% select(original_title,authors)




















