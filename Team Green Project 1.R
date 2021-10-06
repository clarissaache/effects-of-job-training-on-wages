library(stargazer)
#library(tidyverse)
library(ggplot2)
library(arm)
library(pROC)
library(caret)
library(e1071)
library(GGally)
library("rms")
library(plyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(gganimate) 
#############Part I#################
wages <- read.table("./Data//lalondedata.txt",sep=",",header=TRUE)
summary(wages)
wages$nonZero78 <- ifelse(wages$re78 > 0,1,0)
wages$nonZero78_fac <-factor(wages$nonZero78,levels=c(0,1),labels=c("Zero","Non-zero"))
wages$train_fac <-factor(wages$treat,levels=c(0,1),labels=c("Not trained","Trained"))
wages$black_fac <- factor(wages$black,levels=c(0,1),labels=c("Not Black","Black"))
wages$hispan_fac <- factor(wages$hispan,levels=c(0,1),labels=c("Not Hispanic","Hispanic"))
wages$married_fac <- factor(wages$married,levels=c(0,1),labels=c("Not Married","Married"))
wages$nodegree_fac <- factor(wages$nodegree,levels=c(0,1),labels=c("degree","nodegree"))
wages$age_c <- wages$age-mean(wages$age)
wages$differ7874 <- wages$re78-wages$re74
wages$educ1[wages$educ<6]<-"No middle school"
wages$educ1[wages$educ>=6 & wages$educ<9]<-"Middle school"
wages$educ1[wages$educ>=9 & wages$educ<=13]<-"High school"
wages$educ1[wages$educ>=13 & wages$educ<17]<-"College"
wages$educ1[wages$educ>=17]<-"5+ years educ"

wages$educ1<-factor(wages$educ1)
## makes education as a categorical variable
wages$educ_fac <- factor(wages$educ)

######EDA

#Univariate
table(wages[,c("train_fac","married_fac")])
table(wages[,c("train_fac","black_fac")])
table(wages[,c("train_fac","hispan_fac")])
table(wages[,c("train_fac","nodegree_fac")])
table(wages[,c("train_fac","nonZero78_fac")])

hist(wages$re74)
hist(wages$re78)
hist(wages$differ7874,xlab="Change in earnings between 1974 and 1978",main="Distribution of difference in earnings between 1974 and 1978")
par(new = TRUE)
boxplot(wages$differ7874,horizontal=TRUE,axes=FALSE,col = rgb(0, 0.8, 1, alpha = 0.5))
box()
#if we remove the 2 outliers
hist(wages[wages$differ7874<30000,]$differ7874,xlab="Change in earnings between 1974 and 1978",main="Distribution of difference in earnings between 1974 and 1978")
par(new = TRUE)
boxplot(wages[wages$differ7874<30000,]$differ7874,horizontal=TRUE,axes=FALSE,col = rgb(0, 0.8, 1, alpha = 0.5))
box()

ggplot(wages, aes(x=age))+geom_bar()
ggplot(wages, aes(educ))+geom_bar()
ggplot(wages, aes(educ1))+geom_bar()
ggplot(wages, aes(black_fac))+geom_bar()
ggplot(wages, aes(hispan_fac))+geom_bar()
ggplot(wages, aes(married_fac))+geom_bar()
ggplot(wages, aes(nodegree_fac))+geom_bar()
ggplot(wages, aes(train_fac))+geom_bar()

#Bivariate for response variable vs each of the categorical variables

p_meds1 <- ddply(wages, .(black_fac), summarise, med = median(differ7874))
# categorical variable: raceBlack vs differ7874
ggplot(wages, aes(x=black_fac,y=differ7874,fill=black_fac))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Change in earnings between 1978 and 1974 vs Black",
       x="Black", y="Diff earnings 78-74")+geom_text(data=p_meds1,aes(x=black_fac,y=med,label=med),size=3,vjust=-1.3)
p_meds2 <- ddply(wages, .(hispan_fac), summarise, med = median(differ7874))
# categorical variable: raceHispanic
ggplot(wages, aes(x=hispan_fac,y=differ7874,fill=hispan_fac))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Change in earnings between 1978 and 1974  vs Hispanic",
       x="Hispanic", y="Diff earnings 78-74")+geom_text(data=p_meds2,aes(x=hispan_fac,y=med,label=med),size=3,vjust=-1.3)

p_meds3 <- ddply(wages, .(married_fac), summarise, med = median(differ7874))

ggplot(wages, aes(x=married_fac,y=differ7874,fill=married_fac))+
  geom_boxplot()+scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Change in earnings between 1978 and 1974 vs Married",
       x="Married", y="Diff earnings 78-74")+geom_text(data=p_meds3,aes(x=married_fac,y=med,label=med),size=3,vjust=-1.3)

p_meds <- ddply(wages, .(nodegree_fac), summarise, med = median(differ7874))
ggplot(wages, aes(x=nodegree_fac,y=differ7874,fill=nodegree_fac))+
  geom_boxplot()+
  labs(title="Change in earnings between 1978 and 1974 vs with high school degree",
       x="High school degree or not", y="Diff earnings 78-74")+geom_text(data=p_meds,aes(x=nodegree_fac,y=med,label=med),size=3,vjust=-1.3)
p_meds <- ddply(wages, .(train_fac), summarise, med = median(differ7874))
ggplot(wages, aes(x=train_fac,y=differ7874,fill=train_fac))+
  geom_boxplot()+
  labs(title="Change in earnings between 1978 and 1974 vs Treated",
       x="Treated or not", y="Diff earnings 78-74")+geom_text(data=p_meds,aes(x=train_fac,y=med,label=med),size=3,vjust=-1.3)

# interaction between categorical variables
ggplot(wages, aes(x=train_fac,y=differ7874,fill=differ7874))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Diff in Earnings 1978-1974 VS treatment By Black",
       x="Training ", y="wage 1978-1974")+facet_wrap(~black_fac)

ggplot(wages, aes(x=train_fac,y=differ7874,fill=differ7874))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Diff in Earnings 1978-1974 VS treatment By Hispanic",
       x="Training Status ", y="wage 1978-1974")+facet_wrap(~hispan_fac)


ggplot(wages, aes(x=train_fac,y=differ7874,fill=differ7874))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Diff in Earnings 1978-1974 VS treatment by Married ",
       x="Training Status", y="wage 1978-1974")+facet_wrap(~married_fac)


ggplot(wages, aes(x=train_fac,y=differ7874,fill=differ7874))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="Diff in Earnings 1978-1974 VS treatment by degree",
       x="Training status", y="wage 1978-1974")+facet_wrap(~nodegree_fac)


#Bivariate for each of the discrete/continuous predictors with the response variable

ggplot(wages,aes(x=age,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title=" Age vs Diff wages 78-74",y="wage78-74",x="age")
ggplot(wages,aes(x=age,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Age vs Diff wages 78-74 by treatment",y="wage78-74",x="age") +facet_wrap(~train_fac)
ggplot(wages,aes(x=educ,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Education vs Diff wages 78-74",y="wage78-74",x="educaiton year")
ggplot(wages,aes(x=educ,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Education vs Diff wages 78-74 by training",y="wage78-74",x="education year") +facet_wrap(~train_fac)
ggplot(wages,aes(x=age,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Age VS Diff wages 78-74 by black",y="wage78-74",x="age centered") +facet_wrap(~black_fac)
ggplot(wages,aes(x=age,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Age VS Diff wages 78-74 by hispanic",y="wage78-74",x="age centered") +facet_wrap(~hispan_fac)
ggplot(wages,aes(x=educ,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Education vs Diff wages 78-74 black",y="wage78-74",x="education year") +facet_wrap(~black_fac)
ggplot(wages,aes(x=educ,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Education vs Diff wages 78-74 hispanic",y="wage78-74",x="education year") +facet_wrap(~hispan_fac)
ggplot(wages,aes(x=log(age),y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Age vs Diff wages 78-74 by married",y="wage78-74",x="age centered") +facet_wrap(~married_fac)
ggplot(wages,aes(x=educ,y=differ7874))+ geom_point()+geom_smooth(method="lm",col="red3")+labs(title="Education vs Diff wages 78-74 by married",y="wage78-74",x="education year") +facet_wrap(~married_fac)

####Model

###First Iteration with just main effects
model0 <- lm(differ7874~age_c+educ+married_fac+train_fac+black_fac+hispan_fac+nodegree_fac,data=wages)
summary(model0)
plot(model0)
plot(model0,which=4)
ggplot(wages,aes(x=age, y=model0$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs age_c",x="age_c",y="Residuals")


ggplot(wages,aes(x=educ, y=model0$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs education year",x="educ year",y="Residuals")

###Second Iteration with all interactions +AIC stepwise

nullModel <- lm(differ7874~train_fac,data= wages)
fullModel <- lm(differ7874~age_c*educ*married_fac*train_fac*black_fac*hispan_fac*nodegree_fac,data=wages)
stepwise <- step(nullModel,scope = formula(fullModel),direction="both",trace=0)
stepwise$call 

model1 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac +train_fac:age_c + 
               age_c:married_fac, data = wages)
model2 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac +train_fac:black_fac+train_fac:age_c + 
               age_c:married_fac, data = wages)
anova(model1,model2)

### ANOVA test on dropped predictors

# check if black? and black?:treat
model2 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac + train_fac:age_c + 
               age_c:married_fac+black_fac+black_fac:train_fac, data = wages)
anova(model1,model2)

## hispanic and hispanic:treatment
model3 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac + train_fac:age_c + 
               age_c:married_fac+hispan_fac+hispan_fac:train_fac, data = wages)
anova(model1,model3)

## just black
model4 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac + train_fac:age_c + 
               age_c:married_fac+black_fac, data = wages)
anova(model1,model4)

## just nodegree
model5 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac + train_fac:age_c + 
               age_c:married_fac+nodegree_fac, data = wages)
anova(model1,model5)

### Final Model 

model1 <- lm(formula = differ7874 ~ train_fac + age_c + married_fac +train_fac:age_c + 
               age_c:married_fac, data = wages)
ggplot(wages,aes(x=age_c, y=model1$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs age_c",x="age_c",y="Residuals")
summary(model1)
confint(model1)
plot(model1)
vif(model1)

###############Part II####################

###EDA
countBYtraining <- table(wages[,c("nonZero78_fac","train_fac")])
countBYtraining
jointProb_training <- table(wages[,c("nonZero78_fac","train_fac")])/sum(countBYtraining)
conditionalProb_training <- apply(jointProb_training, 2, function(x) x/sum(x))
conditionalProb_training
chisq.test(countBYtraining)

countBYblack <- table(wages[,c("nonZero78_fac","black_fac")])
countBYblack
jointProb_black <- countBYblack/sum(countBYblack)
conditionalProb_black <- apply(jointProb_black, 2, function(x) x/sum(x))
conditionalProb_black
chisq.test(countBYblack)

countBYhispan <- table(wages[,c("nonZero78_fac","hispan_fac")])
countBYhispan
jointProb_hispan <- countBYhispan/sum(countBYhispan)
conditionalProb_hispan <- apply(jointProb_hispan, 2, function(x) x/sum(x))
conditionalProb_hispan
chisq.test(countBYhispan)

countBYnodegree <- table(wages[,c("nonZero78_fac","nodegree_fac")])
countBYnodegree
jointProb_nodegree <- countBYnodegree/sum(countBYnodegree)
conditionalProb_nodegree<- apply(jointProb_nodegree, 2, function(x) x/sum(x))
conditionalProb_nodegree
chisq.test(countBYnodegree)

countBYmarried <- table(wages[,c("nonZero78_fac","married_fac")])
countBYmarried
jointProb_married <- countBYmarried/sum(countBYmarried)
conditionalProb_married <- apply(jointProb_married, 2, function(x) x/sum(x))
conditionalProb_married
chisq.test(countBYmarried)

#More EDA for numerical

ggplot(wages, aes(x=nonZero78_fac,y=age, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="age vs nonzero78",
       x="nonzero78", y="age")

ggplot(wages, aes(x=nonZero78_fac,y=educ, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="age vs nonzero78",
       x="nonzero78", y="educ")


ggplot(wages, aes(x=nonZero78_fac,y=re74, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="re74 vs nonzero78",
       x="nonzero78", y="re74")


binnedplot(y=wages$nonZero78,x=wages$age,xlab="age",ylab="zero wage 78",ylim=c(0,1),main = "zero wage 78 vs age")
binnedplot(y=wages$nonZero78,x=wages$age_c,xlab="age_c",ylab="zero wage 78",ylim=c(0,1),main = "zero wage 78 vs age centered")
binnedplot(y=wages$nonZero78,x=wages$educ,xlab="education year",ylab="zero wage 78",ylim=c(0,1),main = "zero wage 78 vs education year")
binnedplot(y=wages$nonZero78,x=wages$re74,xlab="re74",ylab="zero wage 78",ylim=c(0,1),main = "zero wage 78 vs re74")

sum(wages$age>40)
sum(wages$age>50)

#age:categorical

ggplot(wages, aes(x=nonZero78_fac,y=age, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="age vs nonzero78",
       x="nonzero78", y="age")+facet_wrap(~train_fac)

ggplot(wages, aes(x=nonZero78_fac,y=age, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="age vs nonzero78",
       x="nonzero78", y="age")+facet_wrap(~married_fac)


ggplot(wages, aes(x=nonZero78_fac,y=age, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="age vs nonzero78",
       x="nonzero78", y="age")+facet_wrap(~black_fac)

ggplot(wages, aes(x=nonZero78_fac,y=age, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="age vs nonzero78",
       x="nonzero78", y="age")+facet_wrap(~hispan_fac)
#using education 

ggplot(wages, aes(x=nonZero78_fac,y=educ, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="educ vs nonzero78",
       x="nonzero78", y="educ")+facet_wrap(~train_fac)

ggplot(wages, aes(x=nonZero78_fac,y=educ, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="educ vs nonzero78",
       x="nonzero78", y="educ")+facet_wrap(~married_fac)


ggplot(wages, aes(x=nonZero78_fac,y=educ, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="educ vs nonzero78",
       x="nonzero78", y="educ")+facet_wrap(~black_fac)

ggplot(wages, aes(x=nonZero78_fac,y=educ, fill=nonZero78_fac))+
  geom_boxplot()+coord_flip()+
  scale_fill_brewer(palette = "Reds")+
  labs(title="educ vs nonzero78",
       x="nonzero78", y="educ")+facet_wrap(~hispan_fac)
##EDA with binned plot

par(mfcol=c(2,1))
binnedplot(y=wages$nonZero78[wages$treat==0],x=wages$age[wages$treat==0],xlab="age",ylab="nonZero",ylim=c(0,1),main = "Binned age vs Nonzero78 Not treated")
binnedplot(y=wages$nonZero78[wages$treat==1],x=wages$age[wages$treat==1],xlab="age",ylab="nonZero",ylim=c(0,1),main = "Binned age vs Nonzero78")


par(mfcol=c(2,1))
binnedplot(y=wages$nonZero78[wages$treat==0],x=wages$educ[wages$treat==0],xlab="education years",ylab="nonZero",ylim=c(0,1),main = "Binned education year vs Nonzero78 Not treated")
binnedplot(y=wages$nonZero78[wages$treat==1],x=wages$educ[wages$treat==1],xlab="edcation years",ylab="nonZero",ylim=c(0,1),main = "Binned education year vs Nonzero78")

par(mfcol=c(2,1))
binnedplot(y=wages$nonZero78[wages$treat==0],x=wages$re74[wages$treat==0],xlab="education years",ylab="nonZero",ylim=c(0,1),main = "Binned re74 vs Nonzero78 Not treated")
binnedplot(y=wages$nonZero78[wages$treat==1],x=wages$re74[wages$treat==1],xlab="edcation years",ylab="nonZero",ylim=c(0,1),main = "Binned re74 vs Nonzero78")


par(mfcol=c(2,1))
binnedplot(y=wages$nonZero78[wages$nodegree==0],x=wages$educ[wages$nodegree==0],xlab="education years",ylab="nonZero",ylim=c(0,1),main = "Binned education year vs Nonzero78 nodegree")
binnedplot(y=wages$nonZero78[wages$nodegree==1],x=wages$educ[wages$nodegree==1],xlab="edcation years",ylab="nonZero",ylim=c(0,1),main = "Binned education year vs Nonzero78")

######Modeling

wages$re74_c <-wages$re74 -mean(wages$re74)
model0 <- glm(nonZero78_fac~age_c+educ+train_fac+black_fac+hispan_fac+nodegree_fac+re74_c+married_fac,family=binomial,data=wages)
summary(model0)
binnedplot(x=fitted(model0),y=model0$residuals)
binnedplot(x=wages$age,y=model0$residuals)
binnedplot(x=wages$educ,y=model0$residuals)

#Using AIC 
nullModel <- glm(nonZero78_fac~train_fac,family=binomial,data=wages)
fullModel <- glm(nonZero78_fac~age_c+educ+married_fac+train_fac+black_fac+hispan_fac+nodegree_fac+
                   re74_c+nonZero74+age_c:train_fac+
                   educ:train_fac+
                   black_fac:train_fac+
                   hispan_fac:train_fac+
                   re74_c:age_c+
                   re74_c:married_fac+
                   re74_c:train_fac+
                   married_fac:train_fac+
                   married_fac:age_c+
                   married_fac:educ+
                   nodegree_fac:train_fac,family=binomial,data=wages)
stepwise2 <- step(nullModel,scope = formula(fullModel),direction="both",trace=0)#k=log(length(wages)))
stepwise2$call

#Final Model
glmmodel1 <- glm(formula = nonZero78_fac ~ train_fac + age_c+I(age_c^2)+I(age_c^3) +re74_c + black_fac + 
                   train_fac:age_c + train_fac:black_fac, 
                 family = binomial, data = wages)

summary(glmmodel1)
exp(glmmodel1$coefficients)
exp(confint(glmmodel1))
vif(glmmodel1)

## AUC 

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(glmmodel1) >= 0.823, "1","0")),as.factor(wages$nonZero78),positive = "1")      
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] 

roc(wages$nonZero78,fitted(glmmodel1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")