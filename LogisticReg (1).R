getwd()
setwd("D:/R/R Analytics data sets")

#GermanBank <- read.csv(file.choose())
GermanBank <- read.csv(choose.files())
head(GermanBank)
names(GermanBank)
dim(GermanBank)

str(GermanBank)
plot(GermanBank$RESPONSE)

german_bank <- GermanBank[,-1]
str(german_bank)


for(i in c(1,3:9,11:21,23:31)){
        german_bank[,i] <- factor(x = german_bank[,i], levels= sort(unique(german_bank[,i])))
}


summary(german_bank)
table(german_bank$RESPONSE)

# class0 <- german_bank[german_bank$RESPONSE == 0, ]
# nrow(class0)
# class1 <-german_bank[german_bank$RESPONSE ==1,]
# nrow(class1)
set.seed(1)

ranuni <- sample(x=c("Training","Testing"),size= nrow(german_bank), 
                 replace= T, prob= c(0.7,0.3))

#myvector <- c(1,2,3,4,5,6)
#sample(myvector,size=4)
#sample(myvector,size=10)
#sample(myvector,size=10,replace = T)

Trngdt <- german_bank[ranuni == "Training", ]
table(Trngdt$RESPONSE)
dim(Trngdt)

# Trclass0 <- Trngdt[Trngdt$RESPONSE == 0, ]
# Trclass1 <- Trngdt[Trngdt$RESPONSE == 1, ]
# nrow(Trclass0)
# nrow(Trclass1)

Tstdt <- german_bank[ranuni == "Testing",]
table(Tstdt$RESPONSE)

Training_model <- glm(RESPONSE~. , data= Trngdt, family= "binomial")
summary(Training_model)#..AIC is Akaike information creteria..AIC=-2LL+2K..used for penality
#we cant conclude from r2 only that our model is good or not..if AIC in dec good model if inc bad
#as no. of predictor inc AIC inc if they are waseful

library(MASS)
Training_model1 <- stepAIC(object = Training_model, direction = "backward")

Training_model2 <- glm(RESPONSE ~ CHK_ACCT + DURATION + HISTORY + NEW_CAR + USED_CAR + 
                         AMOUNT + SAV_ACCT + INSTALL_RATE + MALE_SINGLE + MALE_MAR_or_WID + 
                         GUARANTOR + PROP_UNKN_NONE + AGE + OTHER_INSTALL + RENT + 
                         TELEPHONE + FOREIGN, data=Trngdt,
                       family = "binomial")
summary(Training_model2)
anova(Training_model, Training_model2, test = "Chisq")

#install.packages("pROC")
library(pROC)

#AT train dataset

troc <- roc(response =Training_model2$y,
            predictor = Training_model2$fitted.values,plot = T )
troc$auc

#ifelse(test, yes, no)

trPred <- ifelse(test = Training_model2$fitted.values > 0.5, yes = 1, no = 0) #cut-off is 0.5
table(trPred)
table(Training_model2$y,trPred)
(104+438)/(104+438+102+52)

#at test dataset

tsPred <- predict.glm(object = Training_model2, newdata = Tstdt, type = "response")
tsPred
tsroc <- roc(response= Tstdt$RESPONSE, predictor = tsPred, plot= T)
tsroc$auc
tsPred <- ifelse(test= tsPred <0.5, yes= 0, no = 1)
table(Tstdt$RESPONSE,tsPred)

(38+198)/(38+198+12+56)

