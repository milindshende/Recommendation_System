
#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("tidyverse")
install.packages("Matrix")

library(caTools)
library(tidyverse)
library(Matrix)
library(recommenderlab)
library(kableExtra)
library(gridExtra)
library(ggplot2)

book <- read.csv(file.choose())  # consider csv file
book<- book[,-1]  # delete first column (Sr.No.)
object.size(book) # 1125816 bytes

#metadata about the variable
str(book)
table(book$Book.Title)

#rating distribution
hist(book$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
# consider small data size for assignment purpose
book_new<-book[1:1000, ]
book_new_matrix <- as(book_new, 'realRatingMatrix')
dim(book_new_matrix@data) # 250 x 981

sim1 <- similarity(book_new_matrix[1:25, ], method = "cosine", which = "users")
image(as.matrix(sim1), main = "User Similarity")

sim2 <- similarity(book_new_matrix[ ,1:25], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")

# users who rated at least 2 books and books rated at least 1 times
bmatrix <- book_new_matrix[rowCounts(book_new_matrix) > 2, colCounts(book_new_matrix) > 1]
bmatrix # 70 x 19 rating matrix with 31 ratings

tbl_ratings <- as.data.frame(table(as.vector(bmatrix@data)))
tbl_ratings
tbl_ratings <- tbl_ratings[-1,] #0 means missing values so remove missing values
ggplot(tbl_ratings, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity")
+ ggtitle("Distribution of Book Ratings")

############################## POPULARITY method #########################################
#Popularity based 

book_model <- Recommender(book_new_matrix, method="POPULAR")

#Predictions for xx users 
recommended_books <- predict(book_model, book_new_matrix, n=3)
as(recommended_books, "list")

############### USER BASED COLABORATIVE FILTERING (UBCF) #######################
# non-normalized
UBCF_N_C <- Recommender(book_new_matrix, "UBCF", 
                        param=list(normalize = NULL, method="Cosine"))

recommended_books1 <- predict(UBCF_N_C, book_new_matrix, n=3)
as(recommended_books1, "list")

############### ITEM BASED COLABORATIVE FILTERING (IBCF) #######################
# non-normalized
IBCF_N_C <- Recommender(book_new_matrix, "IBCF", 
                        param=list(normalize = NULL, method="Cosine"))

recommended_books2 <- predict(IBCF_N_C, book_new_matrix, n=3)
as(recommended_books2, "list")

################################# END #######################################

