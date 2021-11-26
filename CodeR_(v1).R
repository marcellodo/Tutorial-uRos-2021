
###############################################
# # create data.frame
# # install.packages("PSLM2015")
# data(Expenditure, package = "PSLM2015")
# 
# Exp <- data.frame(hhcode=Expenditure[,1],
#                   log(Expenditure[,-1]+1))
# library(dplyr)
# data(Employment, package = "PSLM2015")
# income<- Employment %>% rowwise() %>%
#   mutate(TotalIncome = sum((s1bq08*s1bq09),
#    s1bq10, s1bq15, s1bq17, s1bq19, s1bq21
#    , na.rm = TRUE))
# HHIncome <- tapply(income$TotalIncome, income$hhcode, sum, na.rm=T)
# dfinc <- data.frame(hhcode=as.numeric(names(HHIncome)), 
#                     HHincome=log(HHIncome+1))
# data(HHRoster, package = "PSLM2015")
# head <- subset(HHRoster, s1aq02=="Head", select = c(hhcode, Province, Region, PSU,
#                                                     s1aq04, age, s1aq07))
# colnames(head) <- c("hhcode", "Province", "Region", "PSU","gender", "age", "marital")
# 
# levels(HHRoster$s1aq11)
# appo <- subset(HHRoster, s1aq11=="yes")
# nn <- tapply(appo$idc, appo$hhcode, length)
# levels(HHRoster$s1aq02)
# df1 <- data.frame(hhcode=as.numeric(names(nn)), hhsize=nn)
# 
# appo2 <- subset(appo, s1aq02=="Son/Daughter" | s1aq02=="Son/Daughter-in-law")
# nfig <- tapply(appo2$idc, appo2$hhcode, length)
# df2 <- data.frame(hhcode=as.numeric(names(nfig)), nSD=nfig)
# 
# mm <- merge(head, df1, by="hhcode")
# mm <- merge(mm, df2, by="hhcode", all.x=T)
# mm$nSD[is.na(mm$nSD)] <- 0
# 
# mm <- merge(mm, Exp, by="hhcode")
# mm <- merge(mm, dfinc, by="hhcode")
# 
# hhExp <- mm
# save(hhExp, file = "HHExp.RData")
# 
# rm(list=ls())

#################################

load(file = "HHExp.RData")
str(hhExp)
summary(hhExp)
# removes "Durable" e "NonDurable"
hhExp$DurableGoods <- NULL
hhExp$NonDurable <- NULL

# vector with names of potential predictors of "Food"
xs <-c("age", "hhsize", "nSD", "Hotels", "Furnishing", "Misc", "Tobacco", 
       "Housing", "Clothing", "Health", "Transport", "Communication", 
       "Recreation", "HHincome")

# standard linear regression
lreg <- lm(Food~., data=hhExp[ ,c("Food", xs)])
summary(lreg)

set.seed(1256)
pos <- sample(x=1:nrow(hhExp), size = 200, replace = F)
oo <- hhExp[pos, ]

lreg <- lm(Food~., data=oo[ ,c("Food", xs)])
summary(lreg)



# backward removal predictors
library(MASS)

# Akaike criterion (AIC) variable selection
newreg <- stepAIC(lreg, direction="backward")
summary(newreg)

###############################################
# pairwise correlation
# install.packages("Hmisc")
# correlazione a coppie: spearman2 
library(Hmisc)
spearman2(formula=Food~., data=hhExp[ ,c("Food", xs)], p=1)

# clustering delle variabili
vc <- varclus(x = data.matrix(hhExp[,xs]), similarity = "spearman")
plot(vc)
abline(h=0.5, lty=2)

#####
# spearman2 and mixed type predictors
xc <- c("Province", "Region", "gender","marital")
summary(hhExp[,xc])
spearman2(formula=Food~., data=hhExp[ ,c("Food", xc, xs)], p=1)
spearman2(formula=HHincome~., data=hhExp[ ,c("Food", xc, xs)], p=1)

######################################################
# elastic net
install.packages("glmnet")
library(glmnet)
?glmnet

# lasso
fit0 <- glmnet(x = hhExp[,xs], y = hhExp$Food, 
                family="gaussian")
print(fit0)
par(mfrow=c(1,2))
plot(fit0, xvar="lambda", label = TRUE)
plot(fit0, xvar="dev", label = TRUE)

# estimated coefficients when lambda=0.07858
coef.glmnet(fit0, s = 0.07858)

# cross validation to select lambda
fitcv <- cv.glmnet(x = data.matrix(hhExp[,xs]), 
                   y = hhExp$Food, nfolds=10)

fitcv$lambda.min # min lambda
coef.glmnet(fitcv, s="lambda.min")

fitcv$lambda.1se # max lambda 1se MSE confidence interval
coef.glmnet(fitcv, s="lambda.1se")


#################
# relaxed lasso
fitr <- glmnet(x = hhExp[,xs], y = hhExp$Food, 
               family="gaussian", relax=TRUE)
print(fitr)
# estimated coefficients with lambda=0.114
coef.glmnet(fitr, s = 0.114)

# cross validation to pick-up lambda
fitrcv <- cv.glmnet(x = data.matrix(hhExp[,xs]), 
                   y = hhExp$Food, nfolds=10, relax=T)

print.cv.glmnet(fitrcv)

###########################
# glmnet() and mixed-type predictors

matxx <- makeX(hhExp[, c(xc, xs)]) # transform categorical preds.
View(matxx)

fitr <- glmnet(x = matxx, y = hhExp$Food, 
               family="gaussian", relax=TRUE)
print(fitr)

coef.glmnet(fitr, s = 0.1039)

###########################################################
###########################################################
# lasso and logistic regression 

binTob <- rep(0, nrow(hhExp)) # 
binTob[hhExp$Tobacco>0] <- 1 # 1 if expentidures for tobacco are >0
table(binTob)

xs <-c("age", "hhsize", "nSD", "Hotels", "Furnishing", "Misc", 
       "Housing", "Clothing", "Health", "Transport", "Communication", 
       "Recreation", "HHincome")
xc <- c("Province", "Region", "gender","marital")

matxx <- makeX(hhExp[, c(xc, xs)])

fitLg <- glmnet(x = matxx, y = binTob, family="binomial", 
                relax=TRUE)

print(fitLg)

coef.glmnet(fitLg, s = 0.024950)
pp <- predict(fitLg, newx = matxx, type = "class", s = 0.024950)

plot(fitLg, xvar="dev", label=TRUE)

###################################################################
#################################################################
## pairwise association measures
install.packages(StatMatch)
library(StatMatch)
data(samp.B) # loads samp.B from StatMatch
str(samp.B)

?pw.assoc

prem <- pw.assoc(labour5~area5+urb+hsize5+c.age+sex+marital+edu7, 
                 data=samp.B, out.df = TRUE)
prem
prem[,c("U","npar")]

prem.w <- pw.assoc(labour5~area5+urb+hsize5+c.age+sex+marital+edu7, 
                 data=samp.B, out.df = TRUE, weights = "ww")

cbind(prem[,c("U")], prem.w[,c("U")])

# AIC, BIC
prem[, c("U","AIC","BIC","npar")]

######
# catdap
# install.packages("catdap")
# library(catdap)
# ?catdap2
# cdout <- catdap2(data=samp.B[, -c(1,4,6,12)], response.name="labour5")           
# cdout$subset$vname
# cdout$subset$aic


#####################################################################
##################################################################

install.packages("tree")
citation("tree")
library(tree)

?tree
xs <-c("age", "hhsize", "nSD", "Hotels", "Furnishing", "Misc", "Tobacco",
       "Housing", "Clothing", "Health", "Transport", "Communication", 
       "Recreation", "HHincome")
xc <- c("Province", "Region", "gender","marital")

ftr <- tree(Food~., data=hhExp[, c("Food", xc, xs)])
plot(ftr)
text(ftr)

#####################
# randomForest
install.packages("randomForest")
citation("randomForest")
library(randomForest)
?randomForest

rffit <- randomForest(Food~., data=hhExp[, c("Food", xc, xs)], ntree=50,
                      importance=TRUE)
rffit
importance(rffit)

varImpPlot(rffit, sort=TRUE)

#######################################
# conditional inference forest
# install.packages("party")
# citation("party")
# library(party)
# ?cforest
# cff <- cforest(Food~., data=hhExp[, c("Food", xc, xs)],
#                control = cforest_unbiased(ntree = 50))
# 
# varimp(cff)
# varimp(cff, conditional = TRUE) # in case of multicollinearity

#######################################
install.packages("caret")
install.packages("xgboost")

library(caret)
library(xgboost)

fitxgb <- train(Food~., data=hhExp[, c("Food", xc, xs)], 
                method = "xgbTree", trControl = trainControl("cv", number = 5))
fitxgb
caret::varImp(fitxgb)
