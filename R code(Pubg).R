library(corrplot)
library(caret)
library(class)
library(hydroGOF)
library(purrr)
library(tidyr)
library(ggplot2)
library(gplots)
library(rpart)
library(rpart.plot)

#importing dataset
df <- read.csv("C:/Users/ABC/Desktop/BattleRoyale/train_V2.csv", head = TRUE, stringsAsFactor = FALSE)

#choosing random 100,000 rows for analysis
set.seed(123)
df <- df[sample(nrow(df), 100000), ]
dim(df)
colnames(df)
pubg <- df

#excluding few cokumns from dataset

colnames(df)
df <- df[,-c(1,2,3,16)]
dim(df)

#plotting variable to find their relationship
df %>%
  gather(-winPlacePerc, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = winPlacePerc)) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()


#Box plot to detect outliers
par(mfrow=c(2,2))
for(i in 1:4) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 5:8) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 9:12) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 13:16) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 17:20) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

par(mfrow=c(2,2))
for(i in 21:24) {
  boxplot(df[,i], xlab = colnames(df)[i])
}

#handling above identified outliers
as <- which(df$assists >= 1)
df2 <- df[-as,]
dim(df2)
bo <- which(df2$boosts >= 6)
df2 <- df2[-bo,]
dim(df2)
Dma <- which(df2$damageDealt >= 500)
df2 <- df2[-Dma,]
dim(df2)
DBno <- which(df$DBNOs >= 3)
df2 <- df2[-Dma,]
dim(df2)
hskill <- which(df2$headshotKills >= 2)
df2 <- df2[-hskill,]
dim(df2)
hels <- which(df2$heals >= 5)
df2 <- df2[-hels,]
dim(df2)
kills_ind <- which(df2$kills >= 4)
df2 <- df2[-kills_ind,]
dim(df2)
killstr <- which(df2$killStreaks >= 3)
df2 <- df2[-killstr,]
dim(df)
longestKill <- which(df2$longestKill >= 53)
df2 <- df2[-longestKill,]
dim(df2)
matdur <- which(df2$matchDuration <= 641)
df2 <- df2[-matdur,]
dim(df2)
maxplace <- which(df2$maxPlace >= 80)
df2 <- df2[-hskill,]
dim(df2)
numgrp <-  which(df2$maxPlace >= 80)
df2 <- df2[-hskill,]
dim(df2)
rankpoints <- which(df2$rankPoints >= 3500)
df2 <- df2[-hskill,]
dim(df2)
revi <- which(df2$revives >= 2)
df2 <- df2[-hskill,]
dim(df2)
ridedist <- which(df2$rideDistance >= 1)
df2 <- df2[-hskill,]
dim(df2)

roadKills <- which(df2$roadKills >= 1)
df2 <- df2[-hskill,]
dim(df2)
swimdist <- which(df2$swimDistance >= 1)
df2 <- df2[-hskill,]
dim(df2)
tmKill <- which(df2$teamKills >= 1)
df2 <- df2[-hskill,]
dim(df2)
vehdest <- which(df2$vehicleDestroys >= 1.0)
df2 <- df2[-hskill,]
dim(df2)
walkdist <- which(df2$walkDistance >= 4735)
df2 <- df2[-hskill,]
dim(df2)
wepacc <- which(df2$weaponsAcquired >= 10)
df2 <- df2[-hskill,]
dim(df2)

#Taking unique values out
x <- unique(c(as,bo,Dma,DBno,hskill,hels,kills_ind,killstr,longestKill,matdur,maxplace,numgrp,rankpoints, revi, ridedist, roadKills, swimdist, tmKill, vehdest, walkdist, wepacc))

df3 <- df[-x,]
dim(df3)
length(x)

table(df$kills)

compare(x,as)

View(x)
dev.off()

cormat<- cor(df3)
corrplot(cormat,method = 'number', title = 'Corealation Matrix', addCoefasPercent = TRUE, number.cex = 0.65)
cor(df$winPlacePerc, df$roadKills)
cor(df$winPlacePerc, df$numGroups)
cor(df$winPlacePerc, df$maxPlace)
cor(df$winPlacePerc, df$killPoints)
cor(df$winPlacePerc, df$winPoints)
cor(df$winPlacePerc, df$rankPoints)
df3_bkp <- df3
df3 <- df3[,-c(,13)]
cormat<- cor(df3)

#checking correlation matrix
cormat<- cor(df3)

corrplot(cormat,method = 'number', title = 'Corealation Matrix', addCoefasPercent = TRUE, number.cex = 0.65)

# checking corelation with y 
cor(df$winPlacePerc, df$roadKills)
df3 <- df3[,-1]
df3 <- df3[,-18]
names(df3)
cormat<- cor(df3)
corrplot(cormat,method = 'number', title = 'Corealation Matrix', addCoefasPercent = TRUE, number.cex = 0.65)
cormat<- cor(df3[1:23])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormat, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         addCoefasPercent = TRUE, number.cex = 0.65,
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

heatmap.2(cormat)

# checking corelation of highly corelated variables with y 
cor(df$winPlacePerc, df$numGroups)
cor(df$winPlacePerc, df$maxPlace)
cor(df$winPlacePerc, df$killPoints)
cor(df$winPlacePerc, df$winPoints)
cor(df$winPlacePerc, df$rankPoints)

df3_bkp <- df3
#df3 <- df3_bkp
names(df3_bkp)
names(df3)
dim(df3)
df3 <- df3[,-c(13,15,23)]
names(df3)
dim(df3)

df3_bkp_unscaled <- df3

df3 <- as.data.frame(scale(df3[,c(1:20)]))
df4 <- df3_bkp_unscaled
class(df3)
summary(df3)
names(df3)
names(df3_bkp_unscaled)
dim(df3)

logt<- log(df3_bkp_unscaled$winPlacePerc/(1-df3_bkp_unscaled$winPlacePerc))
lo <- log(o/(1-o))
head(o)
head(logt)
length(logt)
lo <- as.data.frame(lo)
class(lo)
df3 <- cbind(df3,logt)
df3 <- cbind(df3,df3_bkp_unscaled$winPlacePerc)
dim(df3)
names(df3)
colnames(df3)[21] <- "winPlacePerc"
names(df3)
sapply(X = df4, FUN = function(x) sum(is.infinite(x)))
sapply(X = df4, FUN = function(x) sum(is.na(x)))
inf_index <- which(df3$winPlacePerc=="-Inf")
inf_index2 <- which(df4$winPlacePerc=="-Inf")
df4[inf_index2,]
df4 <- df3[-inf_index,]
#df3 <- cbind(df3,df3_bkp_2$winPlacePerc)
dim(df3)
dim(df4)
names(df3)
dim(df4)

# Data Partitioning 
sapply(X = train_df, FUN = function(x) sum(is.na(x)))
trainind <- sample(1:nrow(df3), nrow(df3)*.7)
trainind4 <- sample(1:nrow(df4), nrow(df3)*.7)
length(trainind4)
testind <- setdiff(1:nrow(df3), trainind)
testind4 <- setdiff(1:nrow(df4), trainind)
length(testind4)
train_df <- df3[trainind, ]
test_df <- df3[testind, ]
train_df4 <- df4[trainind, ]
test_df4 <- df4[testind, ]
dim(train_df)
names(train_df)
dim(test_df)
names(train_df)

# Multiple Linear Regression

obj.null <- lm(winPlacePerc ~ 1, dat = train_df)
obj.full = lm(winPlacePerc ~ ., dat = train_df)
obj_fwd = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='forward')
obj_bkwd = step(obj.full, scope=list(lower=obj.null, upper=obj.full), direction='backward')
obj_step = step(obj.null, scope=list(lower=obj.null, upper=obj.full), direction='both')

summary(obj_fwd)
summary(obj_bkwd)
summary(obj_step)

yhat = predict(obj_fwd, newdata=test_df[,])
ytest = test_df$winPlacePerc
rmse(ytest, yhat)
hist(ytest-yhat)

par(mfrow=c(2,2))
plot(obj_fwd)


# Regression Tree 

cart_fit = rpart(winPlacePerc~.,method="anova", data=train_df4)
dev.off()
rpart.plot(cart_fit)
names(cart_fit)
printcp(cart_fit) # display the results 
plotcp(cart_fit) # visualize cross-validation results 
summary(cart_fit) # detailed summary of splits
pfit = prune(cart_fit, cp = cart_fit$cptable[which.min(cart_fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit)

yhat.test = predict(cart_fit, newdata = test_df[,-21])
yhat.test_prune = predict(pfit, newdata = test_df[,-21])
length(yhat.test)
rmse(yhat.test, ytest)
rmse(yhat.test_prune, ytest)
dev.off()

# create additional plots 
par(mfrow=c(1,2))# two plots on one page 
par(mfrow=c(1,1))
rsq.rpart(cart_fit) # visualize cross-validation results    

# plot tree 
#plot(cart_fit, uniform=TRUE, main="Regression Tree for Mileage ")
#text(cart_fit, use.n=TRUE, all=TRUE, cex=.8)

#summary(df3$killPlace)
#summary(df3$walkDistance)
#summary(df3)

# KNN Optimal

knn.reg.bestK = function(Xtrain, Xtest, ytrain, ytest, kmax=NULL) {
  if (is.null(kmax)) kmax = length(ytrain)^.5
  vec.rmse = rep(NA, kmax)
  for (k in 1:kmax) {
    yhat.test = knn.reg(Xtrain, Xtest, ytrain, k)$pred
    vec.rmse[k] = rmse(yhat.test, ytest)
  }
  list(k.opt = which.min(vec.rmse), rmse.min = min(vec.rmse), vec.rmse)
}

knn.reg.bestK(train_df[,1:20], test_df[,1:20], train_df[,21], test_df[,21])

knn_obj <- kknn(winPlacePerc~., train_df, test_df, k=11)
names(knn_obj)
yhat.knn <- predict(knn_obj, newdata = df_test3)
rmse(ytest, yhat.knn)

#dim(train_df2)
#names(train_df2)

dev.off()
#Errors histogram
hist(yhat.knn, main="Predicted_winPlacePerc", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")

#Residual plot
plot(test_df$winPlacePerc, yhat.knn, ylab="Residuals", xlab="WinPlacePerc", main="Residual Plot (test)") 
abline(0, 0, col = "red")

#Q-Q plot
stdres.KNN = (yhat.knn)
qqnorm(stdres.KNN, ylab="Standardized Residuals", xlab="Normal Scores", main="QQ Plot") 
qqline(stdres.KNN)
