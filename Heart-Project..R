#Data set download
dl <- tempfile()
url="https://raw.githubusercontent.com/Gabrielepa/Heart-Project/main/heart.csv"
download.file(url,dl)
heart_df<-read.csv("https://raw.githubusercontent.com/Gabrielepa/Heart-Project/main/heart.csv", sep = ',', header = TRUE)
names(heart_df)[1]<-"age"

#Installing of all needed libraries if not present

if(!require(tibble)) install.packages("tibble", repos =
                                          "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos =
                                           "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos =
                                           "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos =
                                           "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos =
                                           "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos =
                                           "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos =
                                           "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos =
                                           "http://cran.us.r-project.org")


#Loading libraries

library(tibble)            #data set management
library(dplyr)             #pipeline
library(psych)             #descriptive statistics
library(ggplot2)           #graphs
library(caret)             #Algorithm analysis
library(rattle)            #Decision tree graph
library(xgboost)           #Extreme Boosting Gradient
library(GGally)            #ggcorr

# -----> EXPLORATORY ANALYSIS

cat("The Dataset has ", dim(heart_df)[1], " rows and ",dim(heart_df)[2]," columns")
names(heart_df) #Variables

#1. age (#)
#2. sex : 1= Male, 0= Female (Binary)
#3. (cp) chest pain type (4 values -Ordinal):Value 1: typical angina ,Value 2: atypical angina, Value 3: non-anginal pain , Value 4: asymptomatic 
#4. (trestbps) resting blood pressure (#)
#5. (chol) serum cholesterol in mg/dl (#)
#6. (fbs) fasting blood sugar > 120 mg/dl(Binary)(1 = true; 0 = false)
#7. (restecg) resting electrocardiography results(values 0,1,2)
#8. (thalach) maximum heart rate achieved (#)
#9. (exang) exercise induced angina (binary) (1 = yes; 0 = no) 
#10. (oldpeak) = ST depression induced by exercise relative to rest (#) 
#11. (slope) of the peak exercise ST segment (Ordinal) (Value 1: up sloping , Value 2: flat , Value 3: down sloping )
#12. (ca) number of major vessels (0-3, Ordinal) colored by fluoroscopy 
#13. (thal) maximum heart rate achieved - (Ordinal): 3 = normal; 6 = fixed defect; 7 = reversible defect
          
str(heart_df) #"Type of variables
ifelse(colSums(heart_df)!=0,"No N/A","N/A") #Any missing value?
describe(as_tibble(heart_df)) #Descriptive statistics

#Proportion between 0 (No heart disease) and 1 (Heart disease)
table(heart_df$target)                
data.frame(as.data.frame(table(heart_df$target))[1],as.data.frame(table(heart_df$target))[2],Percentuale=c(mean(heart_df$target==0)*100,mean(heart_df$target==1)*100))

#Correlation among the target and the other variables
correlazion<-cor(heart_df)
a<-data.frame(Target=correlazion[,14],names = row.names(correlazion)) #correlazione dei Feature con il target
a[order(a$Target,decreasing=TRUE),] %>% select(1)
ggcorr(heart_df, method = c("everything", "pearson"))

#SEX
**Male have more probability to have a heart disease**

data.frame(group1=c("Female","Male"),tot=c(table(heart_df$target)[[1]],table(heart_df$target)[[2]])) %>% 
   arrange(desc(group1)) %>% 
   mutate(prop=round(tot/sum(tot)*100)) %>% 
   mutate(ypos=cumsum(prop)-0.5*prop) %>%
   ggplot(aes(x="",y=prop,fill=group1)) +
      geom_bar(stat="identity")+
      coord_polar("y",start=0)+
      geom_text(aes(y=ypos,label=prop))+
      labs(title="Heart Disease for Male and Female",
           subtitle="Percentage M/F",
           caption="Dataset Heart")+
           theme_void()

#AGE

data1<-data.frame(type=as.factor(c(heart_df$target[heart_df$target==1],heart_df$target[heart_df$target==0])),value=c(heart_df[heart_df$target==1,]$age,heart_df[heart_df$target==0,]$age)) %>%
       mutate(Heart_disease=ifelse(type==0,"NO","SI")) %>% select(2,3)
data1 %>%
   ggplot( aes(x=value, fill=Heart_disease)) +
   geom_histogram( color="#e9ecef", alpha=0.6, position = 'dodge') +
   scale_fill_manual(values=c("Orange", "Blue")) +
   labs(fill="Heart disease", 
        title ="Age vs Heart disease",
        caption="Dashed line=median value")+
   xlab("Age")+
  geom_vline(aes(xintercept=40),color="red",size=1,linetype="dashed")+
  geom_vline(aes(xintercept=60),color="red",size=1,linetype="dashed")+
  geom_segment(mapping=aes(x=47,y=10,xend=51,yend=12),arrow=arrow(),size=1)

#CHOLESTEROL

data2<-data.frame(type1=as.factor(c(heart_df$target[heart_df$target==1],heart_df$target[heart_df$target==0])),
                          cholest=c(heart_df[heart_df$target==1,]$chol,heart_df[heart_df$target==0,]$chol)) %>%
                          mutate(heart_disease=ifelse(type1==0,"NO","SI")) %>% select(2,3)
data2 %>%
   ggplot( aes(x=cholest, fill=heart_disease)) +
   geom_histogram( color="#e9ecef", alpha=0.6, position = 'dodge') +
   scale_fill_manual(values=c("Orange", "Blue")) +
   labs(fill="Heart disease", 
        title ="Cholesterol with Heart disease",
        caption="Dashed line=median value")+
   xlab("Chol")+
   geom_vline(data=data2, aes(xintercept = median(cholest)),linetype="dashed",size=1)

chol1<-data.frame(chol_t1=heart_df[heart_df$target==1,]$chol) %>% 
   ggplot(aes(x=chol_t1))+
   geom_histogram(fill="green",color="blue")+
   labs(title="Chol vs heart disease",
        caption="Blue line=maximum cholesterol limit")+
   xlab("Chol with heart disease")+
   ylim(c(0,30))+
   geom_vline(aes(xintercept=200),color="blue",size=1)+
   geom_segment(aes(x=200,y=26,xend=300,yend=26,color="red",size=3))
chol2<-data.frame(chol_t1=heart_df[heart_df$target==0,]$chol) %>% 
   ggplot(aes(x=chol_t1))+
   geom_histogram(fill="red",color="blue")+
   labs(title="Chol vs no heart disease",
        caption="Blue line=maximum cholesterol limit")+
   xlab("Chol without heart disease")+
   ylim(c(0,30))+
   geom_vline(aes(xintercept=200),color="blue",size=1)
gridExtra::grid.arrange(chol1,chol2,ncol=2)


# RESTING BLOOD PRESSURE

data3<-data.frame(type1=as.factor(c(heart_df$target[heart_df$target==1],heart_df$target[heart_df$target==0])),
                  restbp=c(heart_df[heart_df$target==1,]$trestbps,heart_df[heart_df$target==0,]$trestbps)) %>%
                  mutate(heart_disease=ifelse(type1==0,"NO","SI")) %>% select(2,3)
data3 %>%
   ggplot( aes(x=restbp, fill=heart_disease)) +
   geom_histogram( color="#e9ecef", alpha=0.6, position = 'dodge') +
   scale_fill_manual(values=c("Orange", "Blue")) +
   labs(fill="Heart disease", title ="Blood pressure vs Heart disease")+
   xlab("Blood pressure")

bp1<-data.frame(bp_t1=heart_df[heart_df$target==1,]$trestbps) %>% 
   ggplot(aes(x=bp_t1))+
   geom_histogram(fill="green",color="blue")+
   labs(title="Blood pressure vs heart disease")+
   xlab("Systolic blood pressure with heart disease")+
   ylim(c(0,30))+
   geom_vline(aes(xintercept=120),color="blue",size=1)
bp2<-data.frame(bp_t1=heart_df[heart_df$target==0,]$trestbps) %>% 
   ggplot(aes(x=bp_t1))+
   geom_histogram(fill="red",color="blue")+
   labs(title="Blood pressure vs no heart disease")+
   xlab("Systlolic blood pressure without heart disease")+
   ylim(c(0,30))+
   geom_vline(aes(xintercept=120),color="blue",size=1)
gridExtra::grid.arrange(bp1,bp2,ncol=2)

# MAXIMUM HEART RATE

data4<-data.frame(type1=as.factor(c(heart_df$target[heart_df$target==1],heart_df$target[heart_df$target==0])),
                  hr=c(heart_df[heart_df$target==1,]$thalach,heart_df[heart_df$target==0,]$thalach)) %>%
   mutate(heart_disease=ifelse(type1==0,"NO","SI")) %>% select(2,3)
data4 %>%
   ggplot( aes(x=hr, fill=heart_disease)) +
   geom_histogram( color="#e9ecef", alpha=0.6, position = 'dodge') +
   scale_fill_manual(values=c("Orange", "Blue")) +
   labs(fill="Heart disease", title ="Heart rate with Heart disease")+
   xlab("Heart rate")

hr1<-data.frame(hr_t1=heart_df[heart_df$target==1,]$thalach) %>% 
   ggplot(aes(x=hr_t1))+
   geom_histogram(fill="green",color="blue")+
   labs(title="Heart rate and heart disease",
        caption="Blue line=Median value")+
   xlab("Heart rate with heart disease")+
   ylim(c(0,30))+
   geom_vline(aes(xintercept=median(data4$hr[data4$heart_disease=="SI"])),color="blue",size=2)
hr2<-data.frame(hr_t1=heart_df[heart_df$target==0,]$thalach) %>% 
   ggplot(aes(x=hr_t1))+
   geom_histogram(fill="red",color="blue")+
   labs(title="Heart rate and heart disease",
        caption="Blue line=Median value")+
   xlab("Heart rate without heart disease")+
   ylim(c(0,30))+
   geom_vline(aes(xintercept=median(data4$hr[data4$heart_disease=="NO"])),color="blue",size=1)
gridExtra::grid.arrange(hr1,hr2,ncol=2)

# OLDPEAK OR ST DEPRESSION

#Oldpeak vs target
data5<-data.frame(type1=as.factor(c(heart_df$target[heart_df$target==1],heart_df$target[heart_df$target==0])),
                  oldpea=c(heart_df[heart_df$target==1,]$oldpeak,heart_df[heart_df$target==0,]$oldpeak)) %>%
  mutate(heart_disease=ifelse(type1==0,"NO","SI")) %>% select(2,3)

data5 %>%
  ggplot( aes(x=oldpea, fill=heart_disease)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'dodge') +
  scale_fill_manual(values=c("Orange", "Blue")) +
  labs(fill="Heart disease", title ="ST depression vs Heart disease",
       caption="0=No depression   1=mild   2=moderate   >4=severe")+
  xlab("ST depression")

# -----> DATA CLEANING

#Let's find the outlier values
library(psych)
df1<-heart_df %>% select("age","trestbps","chol","thalach","oldpeak")
describe((df1))
summary(df1)
gr1<-df1 %>% ggplot(aes(y=age))+ 
     geom_boxplot(fill="green",alpha=0.2)+
     ggtitle("Age")+
     ylab("Age")+
     theme(plot.title=element_text(color="red",size=12,face="bold.italic"))
#   ggtitle("Età")
gr2<-df1 %>%
     ggplot(aes(y=trestbps))+
     geom_boxplot(fill="green",alpha=0.4)+
     ggtitle("Pressure")+
     ylab("trestbps")+
     theme(plot.title = element_text(color="red",size=12,face="bold.italic"))
gr3<-df1 %>% ggplot(aes(y=chol))+ 
             geom_boxplot(fill="green",alpha=0.6)+
             ylab("chol")+
             ggtitle("Cholesterol")+
             theme(plot.title = element_text(color="red",size=12,face="bold.italic"))
gr4<-df1 %>% ggplot(aes(y=thalach))+ 
             geom_boxplot(fill="green",alpha=0.8)+
             ylab("thalach")+
             ggtitle("Heart rate")+
             theme(plot.title = element_text(color="red",size=12,face="bold.italic"))
gr5<-df1 %>% ggplot(aes(y=oldpeak))+ 
             geom_boxplot(fill="green",alpha=0.9)+
             ylab("oldpeak")+
             ggtitle("ST depression")+
             theme(plot.title = element_text(color="red",size=12,face="bold.italic"))
gridExtra::grid.arrange(gr1,gr2,gr3,gr4,gr5,ncol=3)

#Let's erase the outliers, values>-/+ 3 sd
df1<-heart_df[-which(heart_df$trestbps> mean(heart_df$trestbps)+3*sd(heart_df$trestbps)),]
df2<-df1[-which(df1$chol> mean(df1$chol)+3*sd(df1$chol)),]
df3<-df2[-which(df2$thalach< mean(df2$thalach)-3*sd(df2$thalach)),]
df_new<-df3[-which(df3$oldpeak> mean(df3$oldpeak)+3*sd(df3$oldpeak)),]

# -----> TRAIN AND TEST SET BUILDING
            
#Train e test dataset building
df_new$target=as.factor(df_new$target)
set.seed(1222)
index<-createDataPartition(df_new$target,times=1,p=0.8,list=FALSE)
trainset<-df_new[index,]
testset<-df_new[-index,]
cat("Trainset dimension is",dim(trainset));cat("Testset dimension is ",dim(testset))

# ----> MODELING

#We want to predict if a pazient will have a heart disease (1) or will not with 
#6 machine learning algorithm:
#- Decision Tree
#- Random Forest
#- Logistin regression
#- Knn Algorithm
#- Extreme Gradient Boosting Algorithm
#- Naive Bayes
          
## 1. --------> Decision Tree

trcontrol<-trainControl(method="repeatedcv",number = 10,repeats = 3)
model1_dt<-train(target ~ .,
                 data=trainset,
                 method="rpart",
                 trControl=trcontrol,
                 tuneLength=10)
model1_dt
model1_dt$bestTune
plot(model1_dt)
fancyRpartPlot(model1_dt$finalModel)
          
tunegrid1=expand.grid(cp=model1_dt$bestTune)
model1_dt<-train(target ~ .,
                 data=trainset,
                 method="rpart",
                 trControl=trcontrol,
                 tuneGrid=tunegrid1)
pred<-predict(model1_dt,testset)
confusionMatrix(pred,testset$target)
Accuracy_dt<-confusionMatrix(pred,testset$target)$overall[1]
cat("Accuracy for the Decision Tree Model is ", Accuracy_dt)

# 2. --------> Random Forest
#Let's tune: mtry, maxnodes, ntree
tunegridrf<-expand.grid(mtry=c(1:5))
model2_rf<-train(target~.,
                 data=trainset,
                 method="rf",
                 trControl=trcontrol,
                 tuneGrid=tunegridrf,
                 metric="Accuracy")
model2_rf
model2_rf$bestTune
          
store_maxnode <- list() #Best maxnode
tunegridrf <- expand.grid(mtry = model2_rf$bestTune)
for (maxnodes in c(10:20)) {
     set.seed(1234)
     rf_maxnode <- train(target~.,
                         data = trainset,
                         method = "rf",
                         tuneGrid = tunegridrf,
                         trControl = trcontrol,
                         importance = TRUE,
                         nodesize = 24,
                         maxnodes = maxnodes,
                         ntree = 300)
key <- toString(maxnodes)
store_maxnode[[key]] <- rf_maxnode
}
results_node <- resamples(store_maxnode)
summary(results_node)
best_maxnode<-maxnodes  #Best maxnode
          
store_maxtrees <- list()  #Best ntree research
for (ntree in c(100, 250, 300, 350, 400, 450, 500, 550, 600, 800)) {
set.seed(5678)
rf_maxtrees <- train(target~.,
                     data = trainset,
                     method = "rf",
                     tuneGrid=tunegridrf,
                     trControl = trcontrol,
                     importance = TRUE,
                     nodesize = 24,
                     maxnodes = best_maxnode,
                     ntree = ntree)
key <- toString(ntree)
store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) 
best_maxtree<-ntree

#Random Forest built with the best parameters

model2_rf<-train(target~.,
          data=trainset,
          method="rf",
          trControl=trcontrol,
          tuneGrid=tunegridrf,
          maxnodes=best_maxnode,
          ntree=best_maxtree)

pred_rf<-predict(model2_rf,testset)
confusionMatrix(pred_rf,testset$target)
Accuracy_rf<-confusionMatrix(pred_rf,testset$target)$overall[1]
cat("Accuracy for the Random Forest Model is ", Accuracy_rf)

# 3. --------> Logistic Regression Algorithm

model3_lr<-train(target~.,
                 data=trainset,
                 method="glm",
                 trControl=trcontrol,
                 family="binomial")
model3_lr
model3_lr$results 
model3_lr$finalModel
summary(model3_lr)
pred_lr<-predict(model3_lr,testset)
confusionMatrix(pred_lr,testset$target)
Accuracy_lr<-confusionMatrix(pred_lr,testset$target)$overall[1]
cat("Accuracy for the Logistic Regression model is ", Accuracy_lr)

# 4. --------> Knn Algorithm
          
tunegrid<-expand.grid(k=1:20)
model4_kn<-train(target~.,
                 data=trainset,
                 method="knn",
                 trControl=trcontrol,
                 preProcess=c("center","scale"),
                 tuneGrid=tunegrid,
                 tuneLength=10)

model4_kn$results #Accuracy and AccuracySD
model4_kn$bestTune #best k is 13
plot(model4_kn)

knn_pred_1 <- as.factor(knn3Train(trainset, testset, trainset$target, k = model4_kn$bestTune, use.all = FALSE))
confusionMatrix(data = knn_pred_1, reference = testset$target)
Accuracy_kn<-confusionMatrix(knn_pred_1,testset$target)$overall[1]
Accuracy_kn
cat("Accuracy for the Knn model is ", Accuracy_kn)

# 5. --------> Extreme Gradient Boosting Algorithm

feature_train<-as.matrix(trainset[-14])
feature_test<-as.matrix(testset[-14])
response_train<-as.matrix(trainset$target)
response_test<-as.matrix(testset$target)

# train model
feature_train=as.matrix(trainset[-14])
feature_test=as.matrix(testset[-14])
response_train=as.matrix(trainset[14])
response_test=as.matrix(testset[14])
          
#Grid Search for Hyperparameters
          
#Step 1: Number of Iterations and the Learning Rate
#maximum number of trees:
nrounds <- 1000
tune_grid <- expand.grid(
nrounds = seq(from = 200, to = nrounds, by = 100),
            eta = c(0.025, 0.05, 0.1, 0.3),
            max_depth = c(2, 3, 4, 5, 6),
            gamma = 0,
            colsample_bytree = 1,
            min_child_weight = 1,
            subsample = 1
          )
          
          
tune_control <- caret::trainControl(
            method = "cv", # cross-validation
            number = 3, # with n folds 
            #index = createFolds(tr_treated$Id_clean), # fix the folds
            verboseIter = FALSE, # no training log
            allowParallel = TRUE # FALSE for reproducible results 
)
          
xgb_tune <- caret::train(
            x = feature_train,
            y = response_train,
            trControl = tune_control,
            tuneGrid = tune_grid,
            method = "xgbTree",
            verbose = TRUE
          )
          
ggplot(xgb_tune)
xgb_tune$bestTune
          
#Step 2: Maximum Depth and Minimum Child Weight
          
tune_grid2 <- expand.grid(
            nrounds = seq(from = 50, to = nrounds, by = 50),
            eta = xgb_tune$bestTune$eta,
            max_depth = 2,
            gamma = 0,
            colsample_bytree = 1,
            min_child_weight = c(1, 2, 3),
            subsample = 1
          )
          
xgb_tune2 <- caret::train(
            x = feature_train,
            y = response_train,
            trControl = tune_control,
            tuneGrid = tune_grid2,
            method = "xgbTree",
            verbose = TRUE
          )
          
ggplot(xgb_tune2)
xgb_tune2$bestTune
          
#Step 3: Column and Row Sampling
          
tune_grid3 <- expand.grid(
            nrounds = seq(from = 50, to = nrounds, by = 50),
            eta = xgb_tune$bestTune$eta,
            max_depth = xgb_tune2$bestTune$max_depth,
            gamma = 0,
            colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
            min_child_weight = xgb_tune2$bestTune$min_child_weight,
            subsample = c(0.5, 0.75, 1.0)
          )
          
xgb_tune3 <- caret::train(
            x = feature_train,
            y = response_train,
            trControl = tune_control,
            tuneGrid = tune_grid3,
            method = "xgbTree",
            verbose = TRUE
          )
          
ggplot(xgb_tune3)
xgb_tune3$bestTune
          
#Step 4: Gamma
          
tune_grid4 <- expand.grid(
            nrounds = seq(from = 50, to = nrounds, by = 50),
            eta = xgb_tune$bestTune$eta,
            max_depth = xgb_tune2$bestTune$max_depth,
            gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
            colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
            min_child_weight = xgb_tune2$bestTune$min_child_weight,
            subsample = xgb_tune3$bestTune$subsample
          )
          
xgb_tune4 <- caret::train(
            x = feature_train,
            y = response_train,
            trControl = tune_control,
            tuneGrid = tune_grid4,
            method = "xgbTree",
            verbose = TRUE
          )
          
ggplot(xgb_tune4)
xgb_tune4$bestTune
          
#Step 5: Reducing the Learning Rate
          
tune_grid5 <- expand.grid(
            nrounds = seq(from = 100, to = 1000, by = 100),
            eta = c(0.01, 0.05, 0.1),
            max_depth = xgb_tune2$bestTune$max_depth,
            gamma = xgb_tune4$bestTune$gamma,
            colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
            min_child_weight = xgb_tune2$bestTune$min_child_weight,
            subsample = xgb_tune3$bestTune$subsample
          )
          
xgb_tune5 <- caret::train(
            x = feature_train,
            y = response_train,
            trControl = tune_control,
            tuneGrid = tune_grid5,
            method = "xgbTree",
            verbose = TRUE
          )
          
ggplot(xgb_tune5)
xgb_tune5$bestTune
          
#Fitting the Model
(final_grid <- expand.grid(
            nrounds = xgb_tune5$bestTune$nrounds,
            eta = xgb_tune5$bestTune$eta,
            max_depth = xgb_tune5$bestTune$max_depth,
            gamma = xgb_tune5$bestTune$gamma,
            colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
            min_child_weight = xgb_tune5$bestTune$min_child_weight,
            subsample = xgb_tune5$bestTune$subsample
          ))
(xgb.fit.final1 <- caret::train(
            x = feature_train,
            y = response_train,
            trControl = tune_control,
            tuneGrid = final_grid,
            method = "xgbTree",
            verbose = TRUE
          ))
pred_xg <-predict(xgb.fit.final1, feature_test)
pred_xg
          
#Now, we'll convert the result into factor type
pred_y<-as.factor(pred_xg)
confusionMatrix(pred_y,as.factor(response_test))
Accuracy_xg<-confusionMatrix(pred_y,as.factor(response_test))$overall[1]
Accuracy_xg
cat("Accuracy for the Extreme Boosting Gradient model is ", Accuracy_xg)

# 6. --------> Naive Bayes Algorithm
x = trainset[,-14]
y = trainset$target
model_nb<-train(x,y,'nb',trControl=trcontrol)
predict_nb <- predict(model_nb,newdata = testset )
confusionMatrix(predict_nb,testset$target)
Accuracy_nb<-confusionMatrix(predict_nb,testset$target)$overall[1]
cat("Accuracy for the Naive Bayes model is ", Accuracy_nb)

# --------> FINAL COMPARISON AMONG MODELS

result<-data.frame(knn=Accuracy_kn,logisticR=Accuracy_lr,NaiveB=Accuracy_nb,
                   RandomF=Accuracy_rf,ExtremeB=Accuracy_xg,DecisionT=Accuracy_dt)
cat("Accuracy per varie tecniche:","\n");knitr::kable(result,escape = FALSE, booktabs = TRUE)

#The Best Algorithms are the Random Forest and the Extreme Boost Gradient nut the first is better
#in terms of processing time consuming
          
# Importance Variables for the Random Forest Algorithm.

rf_imp<-varImp(model2_rf,scale=FALSE)
rf_imp
plot(rf_imp)
