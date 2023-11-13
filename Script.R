##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggplot2)
library(dplyr)
library(viridis)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
#set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

#Create edx_test

# Set a seed for reproducibility 
set.seed(123)

# Create indices for the split
train_index <- createDataPartition(movielens$rating, p = 0.9, list = FALSE)

# Training subset
edx_train <- edx[train_index,]

# Testing subset
edx_test <- edx[-train_index,]
rm(dl, ratings, movies, test_index, temp, movielens, removed, train_index, edx_train)




#### Methods and Analysis ####

### Data Analysis ###

# Estructure edx
str(edx) 

# Summary edx Dataset
summary(edx)

# Number of unique movies and users in the edx dataset 
edx %>%
  summarize(Num_users = n_distinct(userId), 
            Num_movies = n_distinct(movieId))

# Ratings distribution
edx %>%
  ggplot(aes(x=rating)) + 
  geom_bar(aes(fill=after_stat(count) + 0.01), color = "black", width=0.25, show.legend = TRUE) +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5), name="Rating Value") +
  scale_y_continuous(breaks = seq(0, 3000000, 500000), name="Number of Ratings") +
  scale_fill_viridis_c(trans="log", name="Number of Ratings") +
  ggtitle("Rating distribution") +
  labs(
    caption = "Source: MovieLens Dataset",
    subtitle = "Distribution of Ratings from 0.5 to 5.0",
    tag = "Figure A"
  ) +
  theme_minimal() +
  theme(legend.position="right")


# Plot number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(x=n, fill=after_stat(count))) +
  geom_histogram(bins = 30, color = "black", show.legend = TRUE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_fill_viridis_c(trans="sqrt", name="Frequency") +
  labs(
    x = "Number of ratings (Log Scale)",
    y = "Number of movies",
    title = "Number of ratings per movie",
    caption = "Source: MovieLens Dataset",
    subtitle = "Distribution of Ratings for Movies",
    tag = "Figure B"
  ) +
  theme_minimal() +
  theme(legend.position="top")


#Top 10 Highest Rated Movies

# Group by movie title, calculate the average rating and total number of ratings
top10_rated_movies <- edx %>%
  group_by(title) %>%
  summarize(average_rating = mean(rating), total_ratings = n(), .groups = "drop") %>%
  arrange(desc(total_ratings), desc(average_rating)) %>%  # Sort by total number of ratings first and then by average rating
  head(10)  # Select the top 10 movies

print(top10_rated_movies)


# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating), .groups = "drop") %>%
  ggplot(aes(x = b_u, fill = after_stat(count + 0.01))) +  # Add a small value to count
  geom_histogram(bins = 30, color = "black", show.legend = TRUE) +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_continuous(limits = c(0.5, 5), breaks = seq(0.5, 5, 0.5)) +
  scale_fill_viridis_c(name = "Frequency", trans = "log") +
  labs(
    caption = "Source: MovieLens Dataset",
    subtitle = "Only users who rated at least 100 movies are considered",
    tag = "Figure C"
  ) +
  theme_light() +
  theme(legend.position = "right")

#Total ratings by movie genre

genre_ratings <- edx %>%
  mutate(genres = str_split(genres, pattern = "\\|")) %>%
  unnest(cols = c(genres)) %>%
  group_by(genres) %>%
  summarise(count = n(), .groups = "drop")


# Bar chart Total ratings by movie genre
genre_ratings %>%
  ggplot(aes(x = reorder(genres, count), y = count, fill = genres)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Total ratings by movie genre") +
  xlab("Genre") +
  ylab("Total ratings") +
  scale_fill_viridis_d(name = "Genre") +
  labs(
    caption = "Source: MovieLens Dataset",
    subtitle = "Distribution of ratings by genre",
    tag = "Figure D"
  ) +
  theme_light() +
  theme(legend.position = "none")


### Modelling Approach ###

## Average movie rating model ##

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Test results based on simple prediction
naive_rmse <- RMSE(edx_test$rating, mu)
naive_rmse

# Check results
# Save prediction in data frame
rmse_results <- tibble(method = "Average movie rating model", RMSE = naive_rmse)
rmse_results %>% knitr::kable(format = "simple")

## Movie effect model ##

# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu), .groups = 'drop')

movie_avgs %>%
  ggplot(aes(x = b_i, fill = after_stat(count))) + 
  geom_histogram(bins = 30, color = "black") +  
  scale_fill_gradient(low = "blue", high = "red") +  
  ylab("Number of movies") +
  ggtitle("Distribution of b_i Across Movies") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Test and save rmse results 
predicted_ratings <- mu +  edx_test %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie effect model",  
                                     RMSE = model_1_rmse ))
# Check results
rmse_results %>% knitr::kable(format = "simple")


## Movie and user effect model ##

# Calculate the penalty term user effect
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i), .groups = 'drop')

# Plot the user effect with a color gradient
user_avgs %>% 
  ggplot(aes(x=b_u, fill = after_stat(count))) + 
  geom_histogram(bins=30, color="black") +
  scale_fill_gradient(low = "yellow", high = "red") +  # Gradient from blue to red
  labs(x = "User Effect (b_u)", y = "Number of Users") +
  ggtitle("Distribution of User Effect (b_u)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the average user bias (user effect) after accounting for movie effects
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Test and save rmse results 
# Generate predicted ratings by joining the test dataset with movie averages
# and user averages, then calculate predictions using the baseline (mu), 
# movie effect (b_i), and user effect (b_u)
predicted_ratings <- edx_test%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Compute the Root Mean Square Error (RMSE) between predicted and actual ratings 
# in the test dataset to evaluate the model's performance
model_2_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie and user effect model",  
                                     RMSE = model_2_rmse))


# Record the RMSE result in a summary data frame, along with the name of the method used
# This allows for comparison of this model's performance against other models
# Check result
rmse_results %>% knitr::kable(format = "simple")



## Regularized movie and user effect model ##

# lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)


# For each lambda value, compute the movie and user biases (b_i and b_u)
# Then predict the ratings and calculate the RMSE for each lambda
# Note: the following loop may take some time to execute due to the computations involved

rmses <- sapply(lambdas, function(l){
  # Calculate the global average rating across all movies
  mu <- mean(edx$rating)
  # Compute the bias for each movie, regularized by lambda
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  # Compute the bias for each user, regularized by lambda
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
 
   # Predict the ratings using the biases and calculate the RMSE for the current lambda
  predicted_ratings <- 
    edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test$rating))
})


# Plot rmses vs lambdas to select the optimal lambda using ggplot2
ggplot(data.frame(lambdas, rmses), aes(x=lambdas, y=rmses)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title="RMS Error vs. Lambda", x="Lambda", y="RMS Error") +
theme_minimal() 


# The optimal lambda                                                             
# The lambda that gives the lowest RMSE is considered the optimal value for regularization

lambda <- lambdas[which.min(rmses)]
lambda

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))
#============================================================
# Apply the optimal lambda value to a final_holdout_test set
lambda_optimal <- lambdas[which.min(rmses)]

# Recalculate movie biases using the optimal lambda
b_i_optimal <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda_optimal))

# Recalculate user biases using the optimal lambda
b_u_optimal <- edx %>%
  left_join(b_i_optimal, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda_optimal))

# Predict the ratings on the final holdout test and calculate RMSE
predicted_ratings_final <- final_holdout_test %>%
  left_join(b_i_optimal, by = "movieId") %>%
  left_join(b_u_optimal, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

final_rmse <- RMSE(predicted_ratings_final, final_holdout_test$rating)

# Test and save the final RMSE result                                                             
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg. movie and user effect model final_holdout_test",  
                                     RMSE = final_rmse))


#================================================================

#### Results ####                                                            
# RMSE results and overview                                                          
rmse_results %>% knitr::kable(format = "simple")

#### Appendix ####
print("Operating System:")
version

#### Appendix ####


RMSE <- function(predicted_ratings, true_ratings){
  sqrt(mean((predicted_ratings - true_ratings)^2))
}


print("Operating System:")
version



