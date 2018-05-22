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

####################################################################################
library(dplyr)
library(spotifyr)
library(boot)
library(caret)
library(caret)
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

###############################################################################

###Make training and testing sets
set.seed(1)

mix =sample(1:1843)
train = mix[1:1382]
test = mix[1383:1843]

trainData = dataset[train,]
testData = dataset[test,]


# removing columns that cannot logically contribute predictively ie: urls
trainData = trainData[, -c(2:7,11,13,22:23,31)]

fullModel = glm(popularity~.,data=trainData)
fullModel #AIC 7793

########################### STEPPWISE REGRESSION #############################


#Let's see what variables stepwise thinks we should take
step(glm(popularity~.,data=trainData))

################ SCALING COLUMNS ##################
trainScales = scale(trainData[-c(1,2,18,22)])
View(trainScales)
trainScales = data.frame(trainData[1],trainScales,trainData[c(2,18,22)])
step(glm(popularity~.,data=trainScales))

#this made no differnce so not going to use it

##################################################

# results in model with 9 vars, AIC: 7769: 
step1 = glm(formula = popularity ~ explicit + track_number + mode + liveness + 
              danceability + artist_popularity + album_popularity + timeAgo + 
              season_release, data = trainData)

#storing coefficients here - note there re many bc not all the columns are numeric:
coefStep1 = coefficients(glm(formula = step1))
length(coefStep1) #12

missclassStep1 = sum(abs(trainData$popularity-predict(step1,newdata=trainData)))/1382
missclassStep1
#3.180397

#Let's see if forward selection is better:
step(glm(popularity~1,data=trainData),direction="forward",scope=list(lower=glm(popularity~1, data=trainData),upper=glm(popularity~.,data=trainData)))

# results in 9 vars, AIC: 7769 - the same
step1Forward = glm(formula = popularity ~ album_popularity + timeAgo + artist_popularity + 
                     track_number + danceability + season_release + explicit + 
                     liveness + mode, data = trainData)
coefStep1Forward = coefficients(glm(formula = step1Forward))


#try with BIC - ends up being same as step1Forward
step(glm(popularity~1,data=trainData),direction="forward",scope=list(lower=glm(popularity~1,data=trainData),upper=glm(popularity~.,data=trainData),k=log(1382)))

#try with probit - same
step(glm(popularity~.,data=trainData), family=probit)


############### CROSS VALIDATION ##############


# Let's use cross validation to see if the best chosen model from above works better or worse than using all variables

cv.glm(trainData,step1,K=10)$delta 
#16.21517 16.19741

cv.glm(trainData,fullModel,K=10)$delta
#this doesn't work bc of the artist genre column, it isnt in the stepwise model so we do not
#think it is one of the most important columns, going to remove it 

trainData = trainData[-18]
fullModel = glm(popularity~.,data = trainData)
cv.glm(trainData,fullModel,K=10)$delta
#16.42685 16.39502
# higher than other model
