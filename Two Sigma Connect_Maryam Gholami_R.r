
install.packages("jsonlite")
install.packages("rjson")
install.packages("dplyr")
install.packages("purrr")
install.packages("nnet")
install.packages("ggplot2")
install.packages("tm")
install.packages("wordcloud")
install.packages("data.table")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("C50")
install.packages("class")



library("data.table")
library("rjson")
library("dplyr")
library("purrr")
library("ggplot2")
library("tm")
library("wordcloud")
library("nnet")
library("rpart")
library("rpart.plot")
library("C50")
require("class")

train <- fromJSON('/resources/data/train.json')

test <- fromJSON('/resources/data/test.json')


varstrain <- setdiff(names(train), c("photos", "features"))
dftrain <- map_at(train, varstrain, unlist) %>% tibble::as_tibble(.)

varstest <- setdiff(names(test), c("photos", "features"))
dftest <- map_at(test, varstest, unlist) %>% tibble::as_tibble(.)


colnames(dftrain)
head(summary(dftrain))


table(as.character(dftrain$building_id))
table(as.character(dftrain$manager_id))


dftrain$bathrooms<- as.factor(dftrain$bathrooms)
dftrain$bedrooms<-as.factor(dftrain$bedrooms)
created_train <- strptime(dftrain$created, format = "%Y-%m-%d %H:%M:%S")
creat_month <- as.numeric(month(created_train))
creat_hour <-as.numeric(hour(created_train))
creat_day <-as.numeric(wday(created_train))
dftrain$listing_id<- as.character(dftrain$listing_id)
dftrain$interest_level<- as.factor(dftrain$interest_level)
feature_count <- as.numeric(lapply(dftrain$features, length))
photo_count <- as.numeric(lapply(dftrain$features, length))


colnames(dftrain)
dftrain_red<- data.frame(dftrain[,-c(3,4,5,6,7,11,12,14)], creat_month,creat_hour,creat_day, feature_count,photo_count)
colnames(dftrain_red)

dftrain_red<-na.omit(dftrain_red)

############clean the bathroom variable

summary(dftrain_red$bathrooms)

dftrain_red <- dftrain_red [!(dftrain_red$bathrooms == 6.5),]
dftrain_red <- dftrain_red [!(dftrain_red$bathrooms == 7),]
dftrain_red <- dftrain_red [!(dftrain_red$bathrooms == 10),]

############cleaning the bedroom variable

summary(dftrain_red$bedrooms)

dftrain_red <- dftrain_red [!(dftrain_red$bedrooms == 7),]
dftrain_red <- dftrain_red [!(dftrain_red$bedrooms == 8),]

###########cleaning listing_id

dt = data.table(dftrain_red)
dt[duplicated(listing_id), cbind(.SD[1], number = .N), by = listing_id]

##############################latitude
summary(dftrain_red$latitude)
a<-boxplot(dftrain_red$latitude)
b<-boxplot(dftrain_red$longitude)


outliers <- dftrain_red [dftrain_red$longitude == 0 |dftrain_red$latitude == 0, ]$listing_id
outliers


dftrain_red<- subset(dftrain_red, dftrain_red$longitude !=0 |dftrain_red$latitude !=0)

########################Cleaning the variable price
boxplot(dftrain_red$price)
dftrain_red<- dftrain_red[!(dftrain_red$price=="43"),]
dftrain_red<- dftrain_red[!(dftrain_red$price=="45"),]
dftrain_red<- dftrain_red[!(dftrain_red$price=="4490000"),]
dftrain_red<- dftrain_red[!(dftrain_red$price=="1150000"),]
dftrain_red<- dftrain_red[!(dftrain_red$price=="1070000"),]
##############################################################

summary(dftrain_red)


colnames(dftrain_red)

summary (dftrain_red$interest_level)
plot(droplevels(dftrain_red$interest_level))


summary (dftrain_red$bathrooms)
plot(droplevels(dftrain_red$bathrooms))

summary(dftrain_red$bedrooms)
plot(droplevels(dftrain_red$bedrooms))

qplot(interest_level,price, data=dftrain_red)


qplot(bedrooms, price, data = dftrain_red, colour =interest_level ) 

qplot(bathrooms, price, data = dftrain_red, colour =interest_level ) 

qplot(bathrooms, bedrooms, data = dftrain_red, colour =interest_level ) 

qplot(price,bathrooms, data = dftrain_red, colour =interest_level ) 

qplot(latitude, longitude, data= dftrain_red, col=interest_level)
qplot(latitude, longitude, data= dftrain_red, col=interest_level, xlim = c(40.57,40.80), ylim = c(-73.75, -74.05))

subdata <- subset(dftrain_red, interest_level %in% c("high", "medium"))


qplot(bathrooms, bedrooms, data = subdata, colour =interest_level )


qplot(bedrooms, price, data = subdata, colour =interest_level ) 


qplot(bathrooms, price, data = subdata, colour =interest_level ) 


qplot(bathrooms, bedrooms, data = subdata, colour =interest_level ) 


qplot(price,bathrooms, data = subdata, colour =interest_level ) 

qplot(latitude, longitude, data= subdata, col=interest_level, xlim = c(40.57,40.80), ylim = c(-73.75, -74.05))

features<- Corpus(VectorSource(dftrain$features))
inspect(features[1])
features_clean<-tm_map(features,removePunctuation)

wordcloud(features_clean, random.order = F, max.words = 30,colors = rainbow(50))



features_clean<-tm_map(features_clean,removeWords,c("allowed","fee","floors","room"))
wordcloud(features_clean, random.order = F, max.words = 30,colors = rainbow(50))



dff<-data.frame(dftrain$listing_id,dftrain$interest_level) 

dtm <- TermDocumentMatrix(features_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 70)
str(v)
barplot(d[1:15,]$freq, las = 2, names.arg = d[1:15,]$word,
        main ="Most frequent words",ylab = "Words")

colnames(dftrain_red)
dftrain_red_cl<- dftrain_red[,-c(1,2,4,7,8,9,10,11,12,13)]
dftest_red_cl <- dftest_red[,-c(1,2,4,7,8,9,10,11,12,13)]

table(dftrain_red$interest_level)

normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

dftrain_red_class<- as.data.frame(lapply(dftrain_red_cl[,c(1,2,3)],normalize))
summary(dftrain_red_class)


head(dftrain_red_cl)
head(dftest_red_cl)


classification<- knn(train = dftrain_red_cl, test = dftest_red_cl, cl=dftrain_red$interest_level ,k=10)
classification





classification_trees<-C5.0(dftrain_red_cl,dftrain_red$interest_level)
summary(classification_trees)

p1<- predict(classification_trees,dftest_red_cl)

p1


########supervised choices######
dff1 <- data.frame(dftrain_red_cl,interest_level =dftrain_red$interest_level)
head(dff1)
classification_trees1<- rpart(interest_level~., data=dff1, method = "class")
rpart.plot(classification_trees1)


pairs(interest_level ~ bathrooms+bedrooms+ latitude+longitude+price+creat_month+
      creat_hour+creat_day+ feature_count+photo_count, data = dftrain_red,
   main="Simple Scatterplot Matrix")

colnames(dftrain_red)
dftrain_red$interest_level1 <- relevel(dftrain_red$interest_level , ref = "low")
Modelfull <- multinom(interest_level1 ~ bathrooms+bedrooms+ latitude+longitude+price+creat_month+creat_hour+creat_day+ feature_count+photo_count, data = dftrain_red)


Modelfull

summary(Modelfull)

Ztest <- summary(Modelfull)$coefficients/summary(Modelfull)$standard.errors

Ztest

exp(coef(Modelfull))


head(perediction <- fitted(Modelfull))
round(head(perediction <- fitted(Modelfull)),2)

dftest$bathrooms<- as.factor(dftest$bathrooms)
dftest$bedrooms<-as.factor(dftest$bedrooms)
created_test <- strptime(dftest$created, format = "%Y-%m-%d %H:%M:%S")
creat_month <- as.numeric(month(created_test))
creat_hour <-as.numeric(hour(created_test))
creat_day <-as.numeric(wday(created_test))
feature_count <- as.numeric(lapply(dftest$features, length))
photo_count <- as.numeric(lapply(dftest$features, length))

############we just omite text and pic
colnames(dftest)
dftest_red<- data.frame(dftest[,-c(3,4,5,6,7,11,12,14)], 
                        creat_month,creat_hour,creat_day, feature_count,photo_count)



dftest_red <- dftest_red[!(dftest_red$bathrooms==7.5), ]
dftest_red <- dftest_red[!(dftest_red$bathrooms==20), ]
dftest_red_1 <- dftest_red[!(dftest_red$bathrooms==112), ]

na.omit(dftest_red)


dt = data.table(dftest_red)
dt[duplicated(listing_id), cbind(.SD[1], number = .N), by = listing_id]


dftest_red<-dftest_red[!(dftest_red$bathrooms==7.5), ]
dftest_red<-dftest_red[!(dftest_red$bathrooms==20), ]
dftest_red<-dftest_red[!(dftest_red$bathrooms==112), ]


Final_prediction_full<-predict(Modelfull, newdata = dftest_red, "probs")

data.frame(dftest_red$listing_id,round(Final_prediction_full,2))

barplot(round(Final_prediction_full,2))

install.packages("MASS")
library("MASS")
step <- stepAIC(Modelfull, direction="both")
step$anova


colnames(dftrain_red)
dftrain_red$interest_level1 <- relevel(dftrain_red$interest_level , ref = "low")

Modelfinal <- multinom(interest_level1 ~ bathrooms+bedrooms+ latitude+longitude+price+creat_month+creat_hour+creat_day+ feature_count, data = dftrain_red)


summary(Modelfinal)

Ztest <- summary(Modelfinal)$coefficients/summary(Modelfinal)$standard.errors

Ztest

exp(coef(Modelfinal))


head(perediction <- fitted(Modelfinal))


Final_prediction_final <- predict( Modelfinal, newdata = data.frame(dftest_red), "probs")

data.frame(listing_id=dftest_red$listing_id,round(Final_prediction_final,2))


