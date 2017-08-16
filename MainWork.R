# Adrian Mead
# atm4rf
# STAT 6430
# Final Project
# Appendix
# Aug 1, 2017

# Answering the main questions now
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
dummy <- (reviewers %>%
  inner_join(reviews, by = c('reviewer_id')))[,c('occupation', 'rating')]
colnames(dummy) <- c('occupation', 'value_of_interest')
dummy2 <- bind_rows(lapply(X = unique(dummy$occupation), FUN = function(X){
  return_this <-dummy[dummy$occupation == X,] %>%
    conf_interval_t(.95)
  return(data_frame(occupation = X, type = c('conf', 'mean', 'conf'), value = c(return_this$lower_bound, (return_this$lower_bound + return_this$upper_bound) / 2, return_this$upper_bound)))
}))
levels_to_factor <- arrange(dummy2[dummy2$type == 'mean',],value)$occupation
dummy2$occupation <- factor(dummy2$occupation, levels = rev(levels_to_factor))
ggplot(dummy2, aes(occupation, value, color = type)) + 
  geom_point(size=2) + 
  theme_bw(base_size = 16) + 
  scale_x_discrete('Occupation') + 
  scale_y_continuous('Rating') + 
  scale_color_discrete(name = "Type of Value") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))






#################################################
#######            AGE GROUPS             #######
#################################################

library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

# Load in the files here
genres <- read_delim('genres.txt', delim = '|', col_names = FALSE)
reviewers <- read_delim('reviewers.txt', delim = '|', col_names = FALSE)
reviews <- read_delim('reviews.txt', delim = '\t', col_names = FALSE)
zipcodes <- read_csv('zipcodes.csv')
# Do a little processING to get header info in correctly (using README info)
names(genres) <- c('movie_id','movie_title','release_date','video_release_date',
                   'imdb_url','unknown','action','adventure','animation',
                   'children','comedy','crime','documentary','drama','fantasy',
                   'film_noir','horror','musical','mystery','romance','sci_fi',
                   'thriller','war','western')
names(reviewers) <- c('reviewer_id','age','gender','occupation','zip_code')
names(reviews) <- c('reviewer_id','movie_id','rating','timestamp')
names(zipcodes) <- c('zip_code', 'zip_code_type', 'city', 'state', 'location_type', 'latitude', 'longitude', 'location', 'decommissioned')

# get genre data in long format using gather. Want to make cuts of genre later
genre_occurrence <- gather(genres[, c(1, 6:24)], key = genre, value = count, unknown:western)
# Subset so that we're only looking at genre == 1 (so where movie_id falls into that genre )
genres_total <- genre_occurrence[genre_occurrence$count == 1, c('movie_id', 'genre')]
# Need to get just the ratings and make age buckets
reviewers_and_reviews <- inner_join(reviews, reviewers, by = c('reviewer_id'))
# Now do age bucketing
ages <- reviewers_and_reviews$age
# Key age demographics with ifelse
reviewers_and_reviews$age_bucket <- ifelse(ages <=11, '02-11',
                                           ifelse(ages <= 17, '12-17',
                                                  ifelse(ages <= 24, '18-24',
                                                         ifelse(ages <= 34, '25-34',
                                                               ifelse(ages <= 49, '35-49',
                                                                      ifelse(ages <= 64, '50-64', 
                                                                             '65+'))))))
# Merge info in so now we have the genres and the ratings
tot_genre_age_info <- inner_join(reviewers_and_reviews[, c('movie_id', 'rating', 'timestamp', 'age_bucket')], genres_total, by = c('movie_id'))
# Produce info for the mean of each movie by genre and age_bucket (spreads out data)
agg_genre_age_info <- tot_genre_age_info %>%
  group_by(movie_id, age_bucket, genre) %>%
  summarise(ave_rating = mean(rating))

# Most general cut; just looking at age preferences 
ggplot(agg_genre_age_info[!(agg_genre_age_info$age_bucket %in% c('02-11', '65+', '12-17')),], aes(ave_rating, color = age_bucket)) +
  geom_density() +
  theme_bw(base_size=16) + 
  scale_x_continuous('Average Rating per Movie') +
  scale_y_continuous('Density') + 
  scale_color_discrete(name = "Age \nDemographics")

# Now break down the preferences further by looking at the ave ratings across genres
ggplot(agg_genre_age_info[!(agg_genre_age_info$age_bucket %in% c('02-11', '65+', '12-17') | agg_genre_age_info$genre %in% c('unknown')),], aes(ave_rating, color = age_bucket)) + 
  geom_density() + 
  facet_wrap(~genre, nrow = 3) + 
  theme_bw(base_size=16) + 
  scale_x_continuous('Average Rating per Movie') + 
  scale_y_continuous('Density') + 
  scale_color_discrete(name = "Age \nDemographics")