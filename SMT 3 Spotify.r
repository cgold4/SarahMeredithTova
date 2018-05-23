##########Data_Collection#######################################

f1=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_1995.txt")
f2=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_1996.txt")
f3=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_1997.txt")
f4=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_1998.txt")
f5=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_1999.txt")
f6=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2000.txt")
f7=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2001.txt")
f8=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2002.txt")
f9=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2003.txt")
f10=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2004.txt")
f11=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2005.txt")
f12=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2006.txt")
f13=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2007.txt")
f14=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2008.txt")
f15=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2009.txt")
f16=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2010.txt")
f17=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2011.txt")
f18=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2012.txt")
f19=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2013.txt")
f20=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2014.txt")
f21=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2015.txt")
f22=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2016.txt")
f23=read.delim("C:/Users/Owner/Desktop/BIMA Capstone/Final Project/_txtfiles2/all_2017.txt")

##########Data_Collection#######################################

f1=read.delim("/Users/sarahcouzens/Documents/project capstone/all_1995.txt")
f2=read.delim("/Users/sarahcouzens/Documents/project capstone/all_1996.txt")
f3=read.delim("/Users/sarahcouzens/Documents/project capstone/all_1997.txt")
f4=read.delim("/Users/sarahcouzens/Documents/project capstone/all_1998.txt")
f5=read.delim("/Users/sarahcouzens/Documents/project capstone/all_1999.txt")
f6=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2000.txt")
f7=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2001.txt")
f8=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2002.txt")
f9=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2003.txt")
f10=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2004.txt")
f11=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2005.txt")
f12=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2006.txt")
f13=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2007.txt")
f14=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2008.txt")
f15=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2009.txt")
f16=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2010.txt")
f17=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2011.txt")
f18=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2012.txt")
f19=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2013.txt")
f20=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2014.txt")
f21=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2015.txt")
f22=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2016.txt")
f23=read.delim("/Users/sarahcouzens/Documents/project capstone/all_2017.txt")


setwd("/Users/sarahcouzens/Documents/project capstone/") #folder with only the files that worked

dataset = rbind(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23)


#remove first column
dataset=dataset[,-1]

################## DATASET PREPARATION ###################################

library(boot)
library(ggplot2)
library(ggridges)
library(FNN)

#change name of row so if needed can work with spotifyr package
colnames(dataset)[11] = "track_uri"

#remove first 14 characters from each row in this column to be left only with uri number
as.character(dataset$track_uri)
dataset$track_uri = substr(dataset$track_uri,15,36)

# change class of date column to dates
class(dataset$album_release_date)      
dataset$album_release_date=as.Date(dataset$album_release_date)

#take out rows that did not have the day/month info - they became na when changing to date class
dataset = na.omit(dataset)


################ FEATURE_ENGINEERING ##################

# The popularity score of a song is based not only on listens recieved, but WHEN those listens occured
# therefore, we believe that the amt of time passed since a song was released will affect its popularity score.
# We create the timeAgo variable which is the number of days since the release date of the track.

timeAgo = Sys.Date()-dataset$album_release_date
timeAgo = as.numeric(timeAgo)

#add to dataframe
dataset = data.frame(dataset,timeAgo)


# We beleive that knowing which season a song was released has potential to be a helpful predictor 
# variable, especially taking into account genres of the song. We created vectors of months that fall
# within each season and then created a vector stating which season each track was released in and
# added it to the dataset.

Winter = c(12,01,02)
Spring = c(03,04,05)
Summer = c(06,07,08)
Fall = c(09,10,11)

# Originally made 4 seperate columns that were all binary, but decided to merge all into one column
# for cleanliness purposes. Also remembered that most algorithms classify it themselves.

season_release = c()

for(i in 1:length(dataset$album_release_date)){
  
  if (as.numeric(substr(dataset$album_release_date[i],6,7)) %in% Winter){
    season_release = c(season_release, "winter")
  }else if(as.numeric(substr(dataset$album_release_date[i],6,7)) %in% Spring){
    season_release = c(season_release, "spring")
  }else if(as.numeric(substr(dataset$album_release_date[i],6,7)) %in% Summer){
    season_release = c(season_release, "summer")
  }else if(as.numeric(substr(dataset$album_release_date[i],6,7)) %in% Fall){
    season_release = c(season_release, "fall")
  }
}

dataset = data.frame(dataset, season_release)

################ SEED SETTING, TRAINING AND TESTING SETS ############################


set.seed(1)

mix =sample(1:1843)
train = mix[1:1382]
test = mix[1383:1843]

trainData = dataset[train,]
testData = dataset[test,]


# removing columns that cannot logically contribute predictively ie: urls
# also removed release date column bc now we have days_ago

trainData = trainData[, -c(2:7,11,13,22:23,31)]


########################## EXPLORATORY VISUALIZATION #########################

# Let's make some pretty graphs to see which variables seem to correlate and/or cause higher popularity


# dataset with only numeric columns in case we need
onlyNums = trainData[-c(18,22)]
onlyNums$explicit = as.numeric(onlyNums$explicit)

min(dataset$popularity)
max(dataset$popularity)
# Note that our range in response variable is only 53-94. Ideally we would have a more variant range than
# this with lower popularities as well. However the technique we used to pull the data returned only songs
# above 53 in popularity.

sum(dataset$popularity <= 60)
# There are 215 pbservations whose popularity falls between 53 - 60, inclusively

sd(dataset$popularity) #6.709367
boxplot(onlyNums$popularity)

par(mfrow=c(4,5))
par(mar=c(1,1,1,1))
popularity = onlyNums$popularity
for(r in 2:20){
  this = onlyNums[,r]
  plot(popularity,this,main=colnames(onlyNums[r]))
}

# We see obvious positive correlation between popularity and album_popularity,
# as well as with artist_popularity. We see a clear negative correlation between
# popularity and timeAgo

# Let's see popularity distribution through genre
ggplot(trainData, aes(x = popularity, y = artist_genres)) + 
  geom_joy(fill = 'mediumorchid2') + 
  theme_joy()

# it's hard to tell if Genre is very significant. However we do see some normal curves which signifies
# some meaningful relationship btwn genre and popularity

#What about season release?
ggplot(trainData, aes(x = popularity, y = season_release)) + 
  geom_joy(fill = 'pink3') + 
  theme_joy()

# There doesnt seem to be major implications here

######################################### STEPPWISE REGRESSION ###############################


fullModel = glm(popularity~.,data=trainData)
fullModel #AIC 7793

#Let's see what variables stepwise thinks we should take
step(glm(popularity~.,data=trainData))
#AIC: 7769

# results in model with 9 vars, AIC: 7769: 
step1 = glm(formula = popularity ~ explicit + track_number + mode + liveness + 
              danceability + artist_popularity + album_popularity + timeAgo + 
              season_release, data = trainData)

#storing coefficients here - note there are many bc not all the columns are numeric:
coefStep1 = coefficients(glm(formula = step1))
length(coefStep1) #12

missclassStep1 = sum(abs(trainData$popularity-predict(step1,newdata=trainData)))/1382
missclassStep1
#3.180397

################ SCALING COLUMNS #######################################################
                                                                                        #
 trainScales = scale(trainData[-c(1,2,18,22)])                                          #                                                                     #
 trainScales = data.frame(trainData[1],trainScales,trainData[c(2,18,22)])               #
 step(glm(popularity~.,data=trainScales))                                               #
                                                                                        #
 #this made no differnce so not going to use it. Would rather original numbers          #
                                                                                        #
########################################################################################

#Let's see if forward selection is better:
step(glm(popularity~1,data=trainData),direction="forward",scope=list(lower=glm(popularity~1, data=trainData),
                                                                       upper=glm(popularity~.,data=trainData)))

# results in 9 vars, AIC: 7769 - the same

step1Forward = glm(formula = popularity ~ album_popularity + timeAgo + artist_popularity + 
                     track_number + danceability + season_release + explicit + 
                     liveness + mode, data = trainData)
coefStep1Forward = coefficients(glm(formula = step1Forward))


#try with BIC - ends up being same as step1Forward
step(glm(popularity~1,data=trainData),direction="forward",scope=list(lower=glm(popularity~1,data=trainData),
                                                          upper=glm(popularity~.,data=trainData),k=log(1382)))

#try with probit - same
step(glm(popularity~.,data=trainData), family=probit)

# Album popularity is highly correlated with the response variable, however logically we probably
# will not know album popularity before we know the track popularity. Let's see how much
# worse the model gets if we do not use album poularity.

step2 = glm(formula = popularity ~ explicit + track_number + mode + liveness + 
              danceability + artist_popularity + timeAgo + 
              season_release, data = trainData)

step2 #AIC 8415 - a lot higher. But might be more realistic quality of the model

missclassStep2 = sum(abs(trainData$popularity-predict(step2,newdata=trainData)))/1382
missclassStep2 #4.044064

############### CROSS VALIDATION ##############################


# Let's use cross validation to see if the best chosen model from above works better 
# or worse than using all variables

cv.glm(trainData,step1,K=10)$delta 
#16.21517 16.19741

cv.glm(trainData,fullModel,K=10)$delta
#this doesn't work bc of the artist genre column, it isnt in the stepwise model 
#so we do not think it is one of the most important columns, going to remove it 

trainData = trainData[-18]
fullModel = glm(popularity~.,data = trainData)
cv.glm(trainData,fullModel,K=10)$delta
#16.42685 16.39502
# higher than other model - expected bc not chosen by stepwise


############################################### KNN #############################################

knn1 = knn.reg(train = onlyNums[,-1],test=onlyNums[,-1],y=popularity,k=10)
misclassKnn1 = sum(abs(onlyNums$popularity - knn1$pred))/1382
misclassKnn1 #3.995803

# using scaled variables 
trainScales1 = trainScales[-c(21,22)] #removing nonnumeric columns
trainScales1$explicit = as.numeric(trainScales1$explicit) #making explicit 0 and 1

knn2 = knn.reg(train=trainScales1[,-1],test=trainScales1[,-1],popularity,k=10)
misclassKnn2 = sum(abs(onlyNums$popularity - knn2$pred))/1382
misclassKnn2 #3.424964 - better

# Let's remove album popularity for same reason as before
View(trainScales1)
trainScales2 = trainScales1[-18]
knn3 = knn.reg(train=trainScales2[,-1],test=trainScales2[,-1],popularity,k=10)
misclassKnn3 = sum(abs(onlyNums$popularity - knn3$pred))/1382
misclassKnn3 #4.067438 - not awful but worse, and prob more realistic in what data we will have

############################# ENSEMBLE ######################################################

# I am going to conclude with 2 models, one using album popularity and one not using it. 
# The ensemble method we will use is taking the predictions of 2 models, and averaging them.
# We will be using the step1 model created from stepwise regression, and the knn2 model
# created from knn using the scaled variables. We will then use both of those models again,
# excluding the album_popularity variable (step2 and knn3).

# step1 and knn2
ensemblePreds1 = (predict(step1,newdata=trainData) + knn2$pred)/2 
misclassEnsemble1 = sum(abs(trainData$popularity - ensemblePreds1))/1382
misclassEnsemble1 #3.1307 - better than both ind. models

#step2 and knn3
ensemblePreds2 = (predict(step2,newdata = trainData) + knn3$pred)/2
misclassEnsemble2 = sum(abs(trainData$popularity - ensemblePreds2))/1382
misclassEnsemble2 #3.937929 - better than both ind. models used

#################################### TEST TIME #############################################################

#Lets see how our models fair with the test data

# Data preperation
testData = testData[, -c(2:7,11,13,22:23,31)] #remove obviously irrelevant columns
scaledTest = scale(testData[-c(1,2,18,22)])    #scale numeric columns
scaledTest = data.frame(testData[1],testData[2],scaledTest) #add response and T/F back
scaledTest$explicit = as.numeric(scaledTest$explicit) #change T/F to 0/1
scaledTest1 = scaledTest[-19] #remove album_popularity
View(scaledTest1)

# Predictions from step1
testStep1 = predict(step1,newdata=testData) 
testStep1 = as.vector(testStep1)
sum(abs(testStep1 - testData$popularity))/461 #3.082998

#Predictions from step2
testStep2 = (predict(step2,newdata=testData) )
testStep2 = as.vector(testStep2)
sum(abs(testStep2 - testData$popularity))/461 #3.877785

#knn2
knnTest1 = knn.reg(train = trainScales1[,-1],test=scaledTest[,-1],y=trainScales[,1],k=10)$pred
sum(abs(knnTest1 - testData$popularity))/461 #5.504338

#knn3
knnTest2 = knn.reg(train = trainScales2[,-1],test=scaledTest1[,-1],y=trainScales2[,1],k=10)$pred
sum(abs(knnTest2 - testData$popularity))/461 #6.680477

# Ensembles 
ensembleTest1 = (testStep1 + knnTest1)/2
ensembleTest2 = (testStep2 + knnTest2)/2


# Misclassification Rates found by taking the averages of the
# absolute values of the differences between the predictions and
# the actual popularity ratings.

testMisclass1 = sum(abs(ensembleTest1 - testData$popularity))/461
testMisclass1 #3.816926

testMisclass2 = sum(abs(ensembleTest2 - testData$popularity))/461
testMisclass2 #4.763454 

# Seems like the knn models actually bring down the accuracy of the 
# step models, and that we are better off usng the step models to predict
# rather than the ensemble methods.

#Exporting the predictions of step1, since this is the best model.

predictions = data.frame(dataset[1383:1843,c(5,6)],testStep1)
colnames(predictions)[3] <- "PopularityPredict"
View(predictions)
write.csv(predictions, file="spotifyPredictions.csv", row.names = F)
