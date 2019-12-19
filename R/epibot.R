library(dplyr)
library(purrr)
library(rtweet)

epitwitter_token <- function() {
  rtweet::create_token(
    "epi_twitter_bot",
    consumer_key = Sys.getenv("EPIBOT_CONSUMER_KEY"),
    consumer_secret = Sys.getenv("EPIBOT_CONSUMER_SECRET"),
    access_token = Sys.getenv("EPIBOT_ACCESS_TOKEN"),
    access_secret = Sys.getenv("EPIBOT_ACCESS_SECRET"),
    set_renv = FALSE
  )
}

# pull new tweets ---------------------------------------------------------

## search terms
epitwitter <- ("#epitwitter OR #blackepimatters")

## use since_id from previous search (if exists)
if (file.exists(file.path("data", "epi_search.rds"))) {
  previous_tweets <- readRDS(file.path("data", "epi_search.rds"))
  since_id <- previous_tweets$status_id[1]
} else {
  since_id <- NULL
}

## search for up to 100,000 tweets using the hashtag
epitwitter_tweets <- search_tweets(
  epitwitter,
  n = 1e5, verbose = FALSE,
  since_id = since_id,
  retryonratelimit = TRUE,
  include_rts = FALSE,
  token = epitwitter_token()
)

if (!is_empty(epitwitter_tweets)) {
  epitwitter_tweets <- distinct(epitwitter_tweets, status_id, .keep_all = TRUE)
}

# select tweets to retweet ------------------------------------------------

if (nrow(epitwitter_tweets) > 5) {
  tweets_to_retweet <- epitwitter_tweets %>%
    sample_n(5) %>%
    arrange(desc(created_at)) %>%
    pull(status_id)

  bem <- epitwitter_tweets$hashtags %>%
    map_lgl(~ "blackepimatters" %in% tolower(.x))

  bem_ids <- epitwitter_tweets %>%
    filter(bem) %>%
    pull(status_id)

  tweets_to_retweet <- unique(c(bem_ids, tweets_to_retweet))
} else {
  tweets_to_retweet <- epitwitter_tweets$status_id
}

# bind and save data ------------------------------------------------------

if (!is_empty(epitwitter_tweets)) {
  kp <- !duplicated(epitwitter_tweets$status_id)
  ## only keep rows (observations) with unique status IDs
  users <- users_data(epitwitter_tweets)[kp, ]
  ## the rows of users should correspond with the tweets
  epitwitter_tweets <- epitwitter_tweets[kp, ]
  ## restore as users attribute
  attr(epitwitter_tweets, "users") <- users

  ## if there's already a search data file saved, then read it in,
  ## drop the duplicates then update the data
  if (file.exists(file.path("data", "epi_search.rds"))) {

    ## bind rows (for tweets AND users data)
    epitwitter_tweets <- do_call_rbind(
      list(epitwitter_tweets, readRDS(file.path("data", "epi_search.rds")))
    )

    ## determine whether each observation has a unique status ID
    kp <- !duplicated(epitwitter_tweets$status_id)

    ## only keep rows (observations) with unique status IDs
    users <- users_data(epitwitter_tweets)[kp, ]

    ## the rows of users should correspond with the tweets
    epitwitter_tweets <- epitwitter_tweets[kp, ]

    ## restore as users attribute
    attr(epitwitter_tweets, "users") <- users
  }

  ## save the data
  saveRDS(epitwitter_tweets, file.path("data", "epi_search.rds"))

  ## save shareable data (only status_ids)
  saveRDS(epitwitter_tweets[, "status_id"], file.path("data", "epi_search-ids.rds"))
}

# retweet random 5 --------------------------------------------------------

if (!is.null(tweets_to_retweet)) {
  walk(tweets_to_retweet, function(.x) {
    post_tweet(
      retweet_id = .x,
      token = epitwitter_token()
    )
    Sys.sleep(20)
  })
}
