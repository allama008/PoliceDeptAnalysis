library(Rfacebook)
access_token <- "EAACEdEose0cBAGqaVsNoQe4OOKX95rqPOSoT0cGg1mgTIXT5r71Mmbmw6gS5fxfZAI5rfhob6JgMZA7JlJJOPIGF8z2fiI0tLT3t7ZA9HQZBGY9lUBHtKfp4wVBwAxpwj6ThQQAEL1cyeBZBfQHLgIMWZAnT4gR2l7ljMPnwnYBg6znbMq9Huz8U4Fn5sK9gwZD"
post_v<- getPage(page="KolkataTrafficPolice", n=5000, token= access_token, feed = T)
posts <- subset(post_v, from_name != "Kolkata Traffic Police")
visitors_post <- posts[, c(7, 3, 4, 9, 10, 11)]
write.csv(visitors_post, 'visitors_post.csv')
