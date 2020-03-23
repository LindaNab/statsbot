library(dplyr)
library(rtweet)
library(purrr)

statstwitter_token <- function() {
  rtweet::create_token(
    "stats_twitter_bot",
    consumer_key = Sys.getenv("STATSBOT_CONSUMER_KEY"),
    consumer_secret = Sys.getenv("STATSBOT_CONSUMER_SECRET"),
    access_token = Sys.getenv("STATSBOT_ACCESS_TOKEN"),
    access_secret = Sys.getenv("STATSBOT_ACCESS_SECRET"),
    set_renv = FALSE
  )
}

# pull new tweets --------------------------------------------------------------

## search terms
statstwitter <- "#statstwitter"

## use since_id from previous search (if exists)
if (file.exists(file.path("data", "stats_search.rds"))) {
  previous_tweets <- readRDS(file.path("data", "stats_search.rds"))
  since_id <- previous_tweets$status_id[1]
} else {
  since_id <- NULL
}

## search for up to 100,000 tweets using the hashtag
statstwitter_tweets <- rtweet::search_tweets(
  statstwitter,
  n = 1e5,
  verbose = FALSE,
  since_id = since_id,
  retryonratelimit = TRUE,
  include_rts = FALSE,
  token = statstwitter_token()
)

if (!is_empty(statstwitter_tweets)) {
  statstwitter_tweets <- distinct(statstwitter_tweets,
                                  status_id,
                                  .keep_all = TRUE)
}

# select tweets to retweet -----------------------------------------------------

# the chance of being retweeted is 0.95
retweet_true <- rbinom(NROW(statstwitter_tweets), 1, 0.95)
if (!all(retweet_true == 0)){
  tweets_to_retweet <- statstwitter_tweets[retweet_true == 1,]$status_id
} else {
  tweets_to_retweet <- NULL
}

# bind and save data -----------------------------------------------------------

if (!is_empty(statstwitter_tweets)) {
  ## if there's already a search data file saved, then read it in,
  ## drop the duplicates then update the data
  if (file.exists(file <- file.path("data", "stats_search.rds"))) {

    ## bind rows with archive
    statstwitter_tweets <- do_call_rbind(
      list(statstwitter_tweets[, "status_id"], readRDS(file = file)))

    ## determine whether each observation has a unique status_id
    kp <- !duplicated(statstwitter_tweets$status_id)

    ## remove rows with duplicated status_ids
    statstwitter_tweets <- statstwitter_tweets[kp, ]
  }
  ## save shareable data (only status_ids)
  saveRDS(statstwitter_tweets[, "status_id"], file = file)
}

# retweet tweets (all with a probability of 0.95 of getting retweeted) ---------
if (!is.null(tweets_to_retweet)) {
  walk(tweets_to_retweet, function(.x) {
    post_tweet(retweet_id = .x,
               token = statstwitter_token())
    Sys.sleep(20)
  })
}
