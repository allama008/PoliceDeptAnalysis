library(Rfacebook)
i=1245
result(post[i,7], access_token)
access_token = "EAACEdEose0cBABgMgDBUxUEiaEfQeDFwVcg3iDyFTcvRZBv5N15stTAiBmvh1b0KK3EBw4aps8oEBb0LElLWKFQ2k6AVmOJUx3KOIL9IfKwBFp0b8ikSo1QtET5kveRbQlskWGHveG2DlR84W5iFagWphNWErgJgdukT4IKHWHZC0Lk77ZCn5nsUAbONk4ZD"
post<- getPage(page="KolkataTrafficPolice", n=2000, token= access_token)
result <- function(id, access_token) {
  comm<- getPost(post= id, access_token, n=1000, comments = TRUE)
  write.csv(comm[3], 'comments.csv')
  id
}
View(post[511, c(3, 5, 7, 9, 10)])
View(post[1:503, c(3, 5, 7, 9, 10)])
View(post[900:1208, c(7, 3, 4, 10)])
#19, 23, 28, 30, 36, 42, 43, 47, 50, 51, 54, 57, 58, 62, 63, 65, 66, 67, 69, 70, 71, 73,
#76, 79, 80, 81, 83, 84, 89, 90, 91, 92, 93, 94, 97, 98, 99, 09, 15, 23
#
