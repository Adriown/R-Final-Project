# Adrian Mead
# atm4rf
# STAT 6430
# Final Project
# Appendix
# Aug 1, 2017

library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

# Load in the files here
genres <- read_delim('genres.txt', delim = '|', col_names = FALSE)
reviewers <- read_delim('reviewers.txt', delim = '|', col_names = FALSE)
reviews <- read_delim('reviews.txt', delim = '\t', col_names = FALSE)
zipcodes <- read_csv('zipcodes.csv')
# Do a little process to get header info in correctly (using README info)
names(genres) <- c('movie_id','movie_title','release_date','video_release_date',
                     'imdb_url','unknown','action','adventure','animation',
                     'children','comedy','crime','documentary','drama','fantasy',
                   'film_noir','horror','musical','mystery','romance','sci_fi',
                   'thriller','war','western')
names(reviewers) <- c('reviewer_id','age','gender','occupation','zip_code')
names(reviews) <- c('reviewer_id','movie_id','rating','timestamp')
names(zipcodes) <- c('zip_code', 'zip_code_type', 'city', 'state', 'location_type', 'latitude', 'longitude', 'location', 'decommissioned')

  # 1. Which percentage of each rating was given?
# First group
reviews_rating_grpd <- reviews %>%
  group_by(rating)
# Now take the proportion
rating_percents <- reviews_rating_grpd %>%
  summarise(rating_per = NROW(movie_id) / NROW(reviews_rating_grpd) * 100)
# Answer
rating_percents
# OUTPUT: 
#   A tibble: 5 x 2
#   rating rating_per
#    <int>      <dbl>
# 1      1      6.110
# 2      2     11.370
# 3      3     27.145
# 4      4     34.174
# 5      5     21.201

  # 2. Which reviewers were the top-10 in terms of number of movies reviewed? (Provide the reviewer number
  # and the number of movies reviewed. If there is a tie for 10th place, include all that tied.)
# Need a new grouping by reviewer
reviews_reviewers_grpd <- reviews %>%
  group_by(reviewer_id)
# Now take the counts and sorting
movies_reviewed_per_reviewer <- reviews_reviewers_grpd %>%
  summarise(num_movies = NROW(movie_id)) %>%
  arrange(desc(num_movies))
# Answer:
head(movies_reviewed_per_reviewer,10) # There were no ties
# OUTPUT:
#   A tibble: 10 x 2
#    reviewer_id num_movies
#          <int>      <int>
#  1         405        737
#  2         655        685
#  3          13        636
#  4         450        540
#  5         276        518
#  6         416        493
#  7         537        490
#  8         303        484
#  9         234        480
# 10         393        448

  # 3. Find a 95% confidence interval for the average rating among all reviewers, and a 95% confidence
  # interval for the average rating among the top-10 reviewers. Does there appear to be evidence that the
  # two groups differs?
# Start with the average rating amongst all viewers
all_reviewers_rating <- rename(reviews, value_of_interest = rating)
# Wrote a function to handle the conf interval
conf_interval_t <- function(df, pvalue){
  # Need number in the sample
  n = NROW(df)
  # And standard deviation
  stand_dev = sd(df$value_of_interest)
  # As well as the mean of the sample
  xmean = mean(df$value_of_interest)
  # And the critical t-value
  t_crit = qt(pvalue + (1-pvalue)/2, n-1)
  lower_bound = xmean - t_crit * stand_dev / sqrt(n)
  upper_bound = xmean + t_crit * stand_dev / sqrt(n)
  return (list(lower_bound = lower_bound, upper_bound = upper_bound))
}
# Answer:
conf_interval_t(all_reviewers_rating, .95)
# OUTPUT:
# 3.522883, 3.536837

# Now for the average rating for the top-10 reviewers
# Start with a vector of the top 10 reviewer_ids
top_ten_reviewer <- head(movies_reviewed_per_reviewer,10)$reviewer_id
# Prep for Conf Int
top_ten_reviewer_rating <- rename(reviews[reviews$reviewer_id %in% top_ten_reviewer, ], value_of_interest = rating)
# Answer:
conf_interval_t(top_ten_reviewer_rating, .95)
# OUTPUT:
# 3.073829, 3.138837

# Yes. There appears to be strong evidence that the two groups, 
# all reviewers vs. the top 10 reviewers, diff from one another 
# to the extent that the 95% confidence intervals around their 
# means do not intersect

  # 4. Which movies were the top-10 based on of number of times reviewed? (Provide the movie title and
  # the number of times reviewed. If there is a tie for 10th place, include all that tied.)
# Now group by movie_id
reviews_movie_id_grpd <- reviews %>%
  group_by(movie_id)
# Now take the counts and sorting
reviews_per_movie <- reviews_movie_id_grpd %>%
  summarise(num_reviews = NROW(rating)) %>%
  arrange(desc(num_reviews))
# Just get the top 10
top_ten_movies_no_names <- head(reviews_per_movie,10) # There were no ties
# Need to merge the title on
top_ten_movies_w_names <- inner_join(top_ten_movies_no_names, genres[, c('movie_id', 'movie_title')], by = c('movie_id'))
# And get the formatting the way the problem wants
top_ten_movies <- top_ten_movies_w_names[, c('movie_title', 'num_reviews')]
# Answer:
top_ten_movies
# OUTPUT:
#   A tibble: 10 x 2
#                      movie_title num_reviews
#                            <chr>       <int>
#  1              Star Wars (1977)         583
#  2                Contact (1997)         509
#  3                  Fargo (1996)         508
#  4     Return of the Jedi (1983)         507
#  5              Liar Liar (1997)         485
#  6   English Patient, The (1996)         481
#  7                 Scream (1996)         478
#  8              Toy Story (1995)         452
#  9          Air Force One (1997)         431
# 10 Independence Day (ID4) (1996)         429

  # 5. Which genre occurred most often, based on the number of reviews. Which was least often? (Don’t
  # include “unknown” as a genre for this question.)
# Trickier problem. Need to take the number of times each movie was reviewed and fit that in with the genre data
tot_movie_info <- inner_join(reviews_per_movie, genres[, colnames(genres)[c(1,6:24)]], by = c('movie_id'))
# Very similar work to the other problems, but first have to 'melt' the data into a long form once unknown and other columns removed
genre_occurrence <- gather(tot_movie_info, key = genre, value = count, unknown:western)
# Weight the counts by the num_reviews from the movie
genre_occurrence$weighted_count <- genre_occurrence$num_reviews * genre_occurrence$count
# Now group by genre
genre_occ_grpd <- genre_occurrence %>%
  group_by(genre)
# And finally summarize to add all the 0's and 1's together
genre_sums <- genre_occ_grpd %>%
  summarise(totNum = sum(weighted_count)) %>%
  arrange(desc(totNum))
# Remove 'unknown'
unknown_removed <- genre_sums[genre_sums$genre != 'unknown',]
# Answer:
rbind(head(unknown_removed,1), tail(unknown_removed, 1))
# OUTPUT:
# Most often: Drama - 39895
# Least often: Documentary - 758

  # 6. What percentage of reviews involved movies classified in at least two genres?
# Need to group by movie_id and num_reviews
grpd_movie_id_num_reviews <- group_by(genre_occurrence, movie_id, num_reviews)
# Now break out a count of the distinct genres that the movies belong to for each movie
movies_reviews_and_genreCount <- grpd_movie_id_num_reviews %>%
  summarise(totGenreCount = sum(count))
# Get just the entries with > 1 genre and those with just one genre
reviews_of_movie_genre_gt_one <- movies_reviews_and_genreCount[movies_reviews_and_genreCount$totGenreCount > 1,]
# Now do the count proportion to find the answer
# ANSWER:
sum(reviews_of_movie_genre_gt_one$num_reviews) /  # Numerator is num of review for movie with > 1 genres
  NROW(reviews) * 100 # Denominator is the total number of reviews
# OUTPUT:
# 69.938

  # 7. Give a 95% confidence interval for the average rating for male reviewers, and do the same for female
  # reviewers.
# Male reviewers first. Break out that group
# Going to require merges
reviewer_gender_rating <- inner_join(reviewers[, c('reviewer_id', 'gender')], reviews[, c('reviewer_id', 'rating')], by = c('reviewer_id'))
# Just males vs females
male_reviewer_gender_rating <- reviewer_gender_rating[reviewer_gender_rating$gender == 'M',]
female_reviewer_gender_rating <- reviewer_gender_rating[reviewer_gender_rating$gender == 'F',]
# Prep Males for Conf Int
male_reviewer_gender_rating <- rename(male_reviewer_gender_rating, value_of_interest = rating)
# Answer -- Male:
conf_interval_t(male_reviewer_gender_rating, .95)
# OUTPUT:
# 3.521309, 3.537269

# Prep Feales for Conf Int
female_reviewer_gender_rating <- rename(female_reviewer_gender_rating, value_of_interest = rating)
# Answer -- Female:
conf_interval_t(female_reviewer_gender_rating, .95)
# OUTPUT:
# 3.517202, 3.545813

  # 8. Which state/territory/Canada/unknown produced the top-5 most reviews?
# Not entirely sure the context here, but going to figure out what state had the top-5 most reviews.
# This will require 3 merges -- reviews, reviewers, and zipcodes
# First lets find the reviews per reviewer -- already have from Prob 2
# Now first merge -- reviewers and reviews
reviewer_reviews_count <- inner_join(reviewers[, c('reviewer_id', 'zip_code')], movies_reviewed_per_reviewer, by = c('reviewer_id'))
# Need to merge on the zip code info now -- need to do a left join as some of the zip codes in reviewers.txt aren't present in zipcodes.csv
reviewer_reviews_zip_code_count <- left_join(reviewer_reviews_count, zipcodes[, c('zip_code', 'state')], by = c('zip_code'))
# Replace all the missing 'state' info after the merge with 'unknown'
reviewer_reviews_zip_code_count$state[is.na(reviewer_reviews_zip_code_count$state)] <- 'unknown'
# Look for any zip code with a letter in it; if it exists then the state becomes 'Canada'
reviewer_reviews_zip_code_count$state[str_detect(reviewer_reviews_zip_code_count$zip_code, '[A-Z,a-z]')] <- 'Canada'
# Now group by state and count up the reviews by state
grpd_by_state <- group_by(reviewer_reviews_zip_code_count, state)
reviews_by_state <- grpd_by_state %>%
  summarise(num_reviews = sum(num_movies)) %>%
  arrange(desc(num_reviews))
# Now just show the top 5
# Answer:
head(reviews_by_state, 5)
# OUTPUT:
#   A tibble: 5 x 2
#   state num_reviews
#   <chr>       <int>
# 1    CA       13842
# 2    MN        7635
# 3    NY        6882
# 4    IL        5740
# 5    TX        5042

  # 9. What percentage of movies have exactly 1 review? 2 reviews? 3 reviews? Continue to 20 reviews.
# Using a table from problem 4 that had movie_id and num_reviews
# Keep track of the number of reviews we're looking for
exact_reviews <- 1:20
tot_movies <- NROW(reviews_per_movie)
# Run an apply over the number of reviews
percentage_moviews_w_reviews_up_to_twenty <- lapply(exact_reviews, function(X){
  # Take the percentage after subsetting on number of reviews
  percentage_movies = NROW(reviews_per_movie[reviews_per_movie$num_reviews == X,]) / tot_movies * 100
  return (tibble(num_reviews = X, percentage = percentage_movies))
}) %>%
  bind_rows()
# Answer:
percentage_moviews_w_reviews_up_to_twenty
# OUTPUT:
#   A tibble: 20 x 2
#    num_reviews percentage
#          <int>      <dbl>
#  1           1  8.3828775
#  2           2  4.0428062
#  3           3  3.5671819
#  4           4  3.8049941
#  5           5  3.0321046
#  6           6  2.3186683
#  7           7  2.6159334
#  8           8  1.7835910
#  9           9  1.9619501
# 10          10  1.9619501
# 11          11  1.1890606
# 12          12  1.6646849
# 13          13  1.4863258
# 14          14  0.8323424
# 15          15  1.3079667
# 16          16  1.1296076
# 17          17  0.5945303
# 18          18  1.4268728
# 19          19  1.0701546
# 20          20  0.7134364

  # 10. Which genre had the highest average review, and which had the lowest average review?
# Going to use some output from problem 5
genre_list <- genre_sums$genre
# I need to get a column like the following: movie_id average_rating num_ratings
# And another like the following:            movie_id genre
# Using more output from problem 4 -- gives reviews grouped by movie_id
# Take the average rating for each movie
ave_review_per_movie <- reviews_movie_id_grpd %>%
  summarise(ave_review = sum(rating) / NROW(rating))
# Now merge together the average review per movie with the number of occurences you get for each genre
movies_reviews_genres <- inner_join(genre_occurrence[, c('movie_id', 'num_reviews', 'genre', 'count')], ave_review_per_movie, by = c('movie_id'))
# Group by genre
grpd_by_genre <- movies_reviews_genres %>%
  group_by(genre)
# And now do the calculation to get the average
ave_review_per_genre <- grpd_by_genre %>%
  summarise(average_review = sum(num_reviews * ave_review * count) / sum(num_reviews * count)) %>%
  arrange(desc(average_review))
# Answer:
rbind(head(ave_review_per_genre,1), tail(ave_review_per_genre, 1))
# OUTPUT:
#   A tibble: 2 x 2
#       genre average_review
#       <chr>          <dbl>
# 1 film_noir       3.921523     MOST
# 2   unknown       3.200000     LEAST

  # 11. Repeat the previous question, for reviewers age 30 and under and then for reviewers over 30.
# Repeating a lot of the work in the last question, but first need to get average movie rating and num of reviews based on a subset of the ages
# Start with people 30 and under
reviewer_movie_age <- inner_join(reviews[, c('reviewer_id', 'movie_id', 'rating')], reviewers[, c('reviewer_id', 'age')], by = c('reviewer_id'))
reviewer_lte_thirty <- reviewer_movie_age[reviewer_movie_age$age <= 30 ,]
reviewer_gt_thirty <- reviewer_movie_age[reviewer_movie_age$age > 30 ,]
# Group by
grpd_lte_thirty <- reviewer_lte_thirty %>%
  group_by(movie_id)
grpd_gt_thirty <- reviewer_gt_thirty %>%
  group_by(movie_id)
# Now summarise to pick up total num of reviews and average rating
ave_tot_lte_thirty <- grpd_lte_thirty %>%
  summarise(num_reviews = NROW(rating), ave_review = sum(rating) / NROW(rating))
ave_tot_gt_thirty <- grpd_gt_thirty %>%
  summarise(num_reviews = NROW(rating), ave_review = sum(rating) / NROW(rating))
# Now merge in with the genre info and we can figure out the average ratings per genre
movies_reviews_genres_lte_thirty <- inner_join(ave_tot_lte_thirty, genre_occurrence[, c('movie_id', 'genre', 'count')], by = c('movie_id'))
movies_reviews_genres_gt_thirty <- inner_join(ave_tot_gt_thirty, genre_occurrence[, c('movie_id', 'genre', 'count')], by = c('movie_id'))
# Group by genre
grpd_by_genre_lte_thirty <- movies_reviews_genres_lte_thirty %>%
  group_by(genre)
grpd_by_genre_gt_thirty <- movies_reviews_genres_gt_thirty %>%
  group_by(genre)
# And now do the calculation to get the average
ave_review_per_genre_lte_thirty <- grpd_by_genre_lte_thirty %>%
  summarise(average_review = sum(num_reviews * ave_review * count) / sum(num_reviews * count)) %>%
  arrange(desc(average_review))
ave_review_per_genre_gt_thirty <- grpd_by_genre_gt_thirty %>%
  summarise(average_review = sum(num_reviews * ave_review * count) / sum(num_reviews * count)) %>%
  arrange(desc(average_review))
# Answer: for people age 30 and under:
rbind(head(ave_review_per_genre_lte_thirty,1), tail(ave_review_per_genre_lte_thirty, 1))
# OUTPUT:
#   A tibble: 2 x 2
#       genre average_review
#       <chr>          <dbl>
# 1 film_noir       3.809929     MOST
# 2   fantasy       3.090786     LEAST

# Answer: for people over 30
rbind(head(ave_review_per_genre_gt_thirty,1), tail(ave_review_per_genre_gt_thirty, 1))
# OUTPUT:
#   A tibble: 2 x 2
#       genre average_review
#       <chr>          <dbl>
# 1 film_noir       3.998054     MOST
# 2   unknown       3.000000     LEAST