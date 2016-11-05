setwd("E:/SS/AV/OnlineHack/Kaggle/House Prices Advanced Regression Techniques")

train <- read.csv("train.csv",stringsAsFactors = T)
test  <- read.csv("test.csv", stringsAsFactors = T)
sample_submissionn <- read.csv("sample_submission.csv")

test$SalePrice <- 0

## Removing Outliers
plot(train$GrLivArea)
train1 <- subset(train, train$GrLivArea <= 4000)

test1 <- test
# The test example with ID 666 has GarageArea, GarageCars, and GarageType 
# but none of the other fields, so use the mode and median to fill them in.
test1[667, "GarageQual"] = "TA"
test1[667, "GarageCond"] = "TA"
test1[667, "GarageFinish"] = "Unf"
test1[667, "GarageYrBlt"] = "1980"

# The test example 1116 only has GarageType but no other information. We'll 
# assume it does not have a garage.
test1[1117, "GarageType"] = NA


## combining rows of train and test
data <- rbind(train1,test1)

all_data <- data

## features
# IR2 and IR3 don't appear that often, so just make a distinction
# between regular and irregular.
table(data["LotShape"])
#IR1  IR2  IR3  Reg 
#965   76   15 1859 
all_data[,"IsRegularLotShape"] = (data["LotShape"] == "Reg") * 1

# Most properties are level; bin the other possibilities together
# as "not level".
table(data["LandContour"])
#Bnk  HLS  Low  Lvl 
#115  120   60 2620 
all_data[,"IsLandLevel"] = (data["LandContour"] == "Lvl") * 1

# Most land slopes are gentle; treat the others as "not gentle".
table(data["LandSlope"])
all_data[,"IsLandSlopeGentle"] = (data["LandSlope"] == "Gtl") * 1

# Most properties use standard circuit breakers.
all_data[,"IsElectricalSBrkr"] = (data["Electrical"] == "SBrkr") * 1

# About 2/3rd have an attached garage.
all_data[,"IsGarageDetached"] = (data["GarageType"] == "Detchd") * 1

# Most have a paved drive. Treat dirt/gravel and partial pavement
# as "not paved".
all_data[,"IsPavedDrive"] = (data["PavedDrive"] == "Y") * 1

# The only interesting "misc. feature" is the presence of a shed.
all_data[,"HasShed"] = (data["MiscFeature"] == "Shed") * 1 

# If YearRemodAdd != YearBuilt, then a remodeling took place at some point.
all_data[,"Remodeled"] = (all_data["YearRemodAdd"] != all_data["YearBuilt"]) * 1

# Did a remodeling happen in the year the house was sold?
all_data[,"RecentRemodel"] = (all_data["YearRemodAdd"] == all_data["YrSold"]) * 1

# Was this house sold in the year it was built?
all_data[,"VeryNewHouse"] = (all_data["YearBuilt"] == all_data["YrSold"]) * 1
all_data[,"Has2ndfloor"] = (all_data["X2ndFlrSF"] > 0) * 1
all_data[,"HasMasVnr"] = (all_data["MasVnrArea"] > 0) * 1
all_data[,"HasWoodDeck"] = (all_data["WoodDeckSF"] > 0) * 1
all_data[,"HasOpenPorch"] = (all_data["OpenPorchSF"] > 0) * 1
all_data[,"HasEnclosedPorch"] = (all_data["EnclosedPorch"] > 0) * 1
all_data[,"Has3SsnPorch"] = (all_data["X3SsnPorch"] > 0) * 1
all_data[,"HasScreenPorch"] = (all_data["ScreenPorch"] > 0) * 1

# These features actually lower the score a little.
# all_data[,"HasBasement"] = data["BsmtQual"].isnull() * 1
# all_data[,"HasGarage"] = data["GarageQual"].isnull() * 1
# all_data[,"HasFireplace"] = data["FireplaceQu"].isnull() * 1
# all_data[,"HasFence"] = data["Fence"].isnull() * 1

# Months with the largest number of deals may be significant.
all_data[,"HighSeason"] = ifelse((data["MoSold"]==4 | data["MoSold"]==5 | data["MoSold"]==6 | 
                                      data["MoSold"]==7 ),1,0)


all_data[,"NewerDwelling"] = ifelse((data["MSSubClass"]==20 | data["MSSubClass"]==60 | data["MSSubClass"]==120 | 
                                        data["MSSubClass"]==160 ),1,0)

all_data$Neighborhood_Good <- 0
all_data[data$Neighborhood == 'NridgHt', "Neighborhood_Good"] = 1
all_data[data$Neighborhood == 'Crawfor', "Neighborhood_Good"] = 1
all_data[data$Neighborhood == 'StoneBr', "Neighborhood_Good"] = 1
all_data[data$Neighborhood == 'Somerst', "Neighborhood_Good"] = 1
all_data[data$Neighborhood == 'NoRidge', "Neighborhood_Good"] = 1


all_data[,"SaleCondition_PriceDown"] = ifelse((data$SaleCondition=='Abnorml'| data$SaleCondition=='Alloca' |
                                            data$SaleCondition=='AdjLand'|data$SaleCondition=='Family'),1,0 )

# House completed before sale or not
all_data[,"BoughtOffPlan"] = ifelse((data$SaleCondition=="Partial"),1,0 )
    

# Heating Quality Poor
all_data[,"BadHeating"] = ifelse((data$HeatingQC=='Po' | data$HeatingQC=='Fa'),1,0 )
    

# Total Area
area_cols = data[,c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
             'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
             'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')]

all_data[,"TotalArea"] = apply(area_cols,1,sum)

#First and Second Floor Area
all_data[,"TotalArea1st2nd"] = all_data["X1stFlrSF"] + all_data["X2ndFlrSF"]

## Age
table(all_data["YrSold"])
#2006 2007 2008 2009 2010 
#619  689  621  647  339 

all_data[,"Age"] = 2010 - all_data["YearBuilt"]
all_data[,"TimeSinceSold"] = 2010 - all_data["YrSold"]

# season
all_data$SeasonSold <- 0
all_data$SeasonSold[all_data$MoSold==3 | all_data$MoSold==4 | all_data$MoSold==5]   <- 1
all_data$SeasonSold[all_data$MoSold==6 | all_data$MoSold==7 | all_data$MoSold==8]   <- 2
all_data$SeasonSold[all_data$MoSold==9 | all_data$MoSold==10 | all_data$MoSold==11] <- 3

# YearsSinceRemodel
all_data[,"YearsSinceRemodel"] = all_data["YrSold"] - all_data["YearRemodAdd"]


# imputing missing values
all_data1 <- all_data

## near
library(caret)
nearZeroVar(all_data1)
#6   9  10  12  15  23  32  36  37  40  46  53  56  64  65  68  69  70  71  72  76  84  91  97 104
all_data2 <- all_data1[,-c(6,9,10,12,15,23,32,36,37,40,46,53,56,64,65,68,69,70,71,72,76,84,91,97,104)]


## imputation imputing missing values as -99
all_data2[is.na(all_data2)] <- -99

all_data3 <- all_data2

for( i in 1:85){
     all_data3[,i]   <- as.numeric(all_data3[,i])
}

all_data3[is.na(all_data3)] <- -99
summary(all_data3)

## dividing to test and train
tr <- all_data3[1:nrow(train1),]
te <- all_data3[nrow(train1)+1:nrow(test),]



# cross-validation
library(xgboost)
DMMatrixTr <- xgb.DMatrix(data = as.matrix(tr[,c(2:59,61:85)]), label = as.matrix(log(tr$SalePrice)))
model_xgb1 <- xgboost(data=DMMatrixTr, objective="reg:linear", nfold=5, nrounds=200, eta=0.02, max_depth=8, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")

nam <- dimnames(as.matrix(tr[,c(2:59,61:85)]))[[2]]
imp <-xgb.importance(nam, model=model_xgb1)
xgb.plot.importance(imp[1:30,])

model_xgb_cv <- xgb.cv(data=as.matrix(tr[,imp$Feature[1:25]]), label=as.matrix(log(tr$SalePrice)), objective="reg:linear", nfold=5, nrounds=1500, eta=0.005, max_depth=5, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")
bst <- which.min(model_xgb_cv$test.rmse.mean)-1
model_xgb_cv$test.rmse.mean[bst] #0.1215

# model building
model_xgb <- xgboost(data=as.matrix(tr), label=as.matrix(log(tr$SalePrice)), objective="reg:linear", nrounds=572, eta=0.02, max_depth=5, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")

# model scoring
pred1 <- predict(model_xgb, as.matrix(te))
exp_pred1 <- exp(pred1)

sub3 <- data.frame(Id=test$Id, SalePrice=exp_pred1)
write.csv(sub3,"sub3.csv",row.names=FALSE)



## randomForest

samp <- sample(1:nrow(tr),0.7*nrow(tr))

cvtr <- tr[samp,]
cvte <- tr[-samp,]

#validation
library(randomForest)
modR<- randomForest(log(cvtr$SalePrice) ~., cvtr[,2:85], ntrees=200, mtry=9, importance=TRUE,seed=125)
imp1 <- data.frame(importance(modR))
impvar <-row.names(imp1[order(imp1$X.IncMSE,decreasing=TRUE),])[1:40]

modRF1 <- randomForest(log(cvtr$SalePrice) ~., cvtr[,c(impvar,"SalePrice")], ntrees=250, mtry=4, importance=TRUE,seed=125)
predRFcv <- predict(modRF1, cvte)
sqrt(mean((predRFcv-log(cvte$SalePrice))^2)) #0.1290596


### ensembling 50 randomForest models
predRFn  <- rep(0,nrow(te))

library(randomForest)
for( i in 1:50){
modRF <- randomForest(log(tr$SalePrice) ~., tr[,c(2:85)], ntrees=200, mtry=9, importance=TRUE,do.trace=10)
predRF <- predict(modRF, te[,c(2:85)])
predRFx <- exp(predRF)
predRFn <- predRFn+predRFx
}
predRFnn <- predRFn/i

## submission
sub4 <- data.frame(Id=test$Id, SalePrice=predRFnn)
write.csv(sub4,"sub4.csv",row.names=FALSE)
## Public Leader Board 0.14326



## Lasso
library(glmnet)
x <- model.matrix(log(SalePrice) ~ ., data=tr[,c(2:85)])
x <- x[,-1]
glmnet1 <- cv.glmnet(x=x, y=log(tr$SalePrice) , type.measure = "mse", nfolds=5, alpha=1)

c <- coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables1 <- variables[2:53]

glmnet1$lambda.min #0.003096298

#plot variable coefficients vs. shrinkage parameter lambda.
glmmod <- glmnet(x=x,y=tr$SalePrice, alpha=1)
plot(glmmod,xvar="lambda")

## putting lambda as glmnet1$lambda.min
glmmod <- glmnet(x=x,y=log(tr$SalePrice), alpha=1, lambda=glmnet1$lambda.min)
tex <- model.matrix(~ ., data=te[,c(2:59,61:85)])
tex <- tex[,-1]
pred2 <- predict(glmmod, tex)
pred2exp <- exp(pred2)

#blending
pred_data <- data.frame(m1=predRFnn, m2=drop(pred2exp))
pred_data$bl <- apply(pred_data,1,mean)

## submission file
sub5 <- data.frame(Id=test$Id, SalePrice=pred_data$bl)
write.csv(sub5,"sub5.csv",row.names=FALSE)
## Public Leader Board 0.13578



























































































































































































































































