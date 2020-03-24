library(rvest)

library(stringr)

library(dplyr)

library(lubridate)

library(readr)

library('rvest')

webpage <- read_html("https://music.apple.com/us/playlist/top-100-usa/pl.606afcbb70264d2eb2b51d8dbcfa6a12")

results <- webpage %>% html_nodes(".targeted-link__target")

records <- vector("list", length = length(results))

url <- "**https://music.apple.com/us/playlist/top-100-usa/pl.606afcbb70264d2eb2b51d8dbcfa6a12**"

webpage <- read_html (url)

wanted_data <- html_nodes(webpage,'**.targeted-link__target**')

rank_data <- html_text(wanted_data)





#4
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "I2j9G106bfnJt88rSjrteTRGj"
consumerSecret = "1gxYmadllk95mJXyId0zoGoyHB8QJdHxy0eUey8s6EeFIIdrd7"
accessToken = "1055216750562820099-AnnRWiuJYV400GgPTh8zvcs1Vvlhat"
accessSecret = "h5Gk3w3nfrG7tk9GzMPaoHArXt52piBLTtuaH9dKAR32z"
setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)

tweets <- searchTwitter('#coronavirus',
                        geocode='35.2225685120,-97.4394760132,500mi', 
                        n=5000, retryOnRateLimit=1)

tweets.df <- twListToDF(tweets) 
View(tweets.df)
head(tweets.df$text)

length(which(tweets.df$isRetweet == TRUE))
