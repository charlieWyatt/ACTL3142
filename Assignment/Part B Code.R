# Part B Code

# importing libraries
library(dplyr)
library(tidyverse)
library(tidyselect)
library(ISLR)
library(MASS) # For qda and lda functions
library(class) # for KNN
library(leaps) # for subset selection
library(glmnet) # for lasso regression
library(pls) # for pcr model
library(boot) # for cv.glm
library(pROC) # for ROC curves
library(e1071) # for SVM
library(FactoMineR)  # for pca
library(tree) # for tree
library(gbm) # for boosting
library(randomForest) # for bagging
library(caret)
library(RANN) # for imputation
library(ggplot2)
library(modeest) # for finding mode of attending physician and race
library(ggpubr) # for arranging ggplot2
library(reshape2) # for melt
library(DMwR) # for SMOTE
library(randomForestExplainer) # for random forest graphs




# notes from reading forums -
  # try aregimpute



# importing data
setwd("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/DataB")


data <- read.csv("Medicare_Outpatient_Inpatient_Beneficiary_PartB.csv", stringsAsFactors = FALSE)
fraudStatus <- read.csv("Medicare_Provider_PartB.csv", stringsAsFactors = FALSE)

data <- left_join(data, fraudStatus)

# Cleaning data

  # note: these columns are all NA - remove them all
  length(data$ClmProcedureCode_6)
  sum(is.na(data$ClmProcedureCode_6))
  length(data$ClmProcedureCode_5)
  sum(is.na(data$ClmProcedureCode_5))
  
  data <- data[,colSums(is.na(data))<nrow(data)] # removes full na columns
  

  # changing those with dates to date type
  date_vars = c(
    "ClaimStartDt",
    "ClaimEndDt",
    "AdmissionDt",
    "DischargeDt",
    "DOB",
    "DOD"
  )
  for (var in date_vars) {
    data[,var] =  as.Date(data[,var])
  }
  
  # changing ClmProcedureCodes, race, state and county to character variables
  # this avoids averaging them in later analysis
  char_vars = c(
    as.character(vars_select(colnames(data), starts_with('Clm'))),
    "Race",
    "State",
    "County"
  )
  for (var in char_vars) {
    data[,var] = as.character(data[,var])
  }
  
  # Changing fraud from "yes"/"no" to 1/0
  # can easily count
  # Checking no NA's in Fraud column
  if(sum(is.na(data$Fraud)) == 0) {
    print("Fraud entries complete")
  }
  data$Fraud <- ifelse(data$Fraud=="Yes", 1, 0)
  data$RenalDiseaseIndicator <- ifelse(data$RenalDiseaseIndicator=="Y", 1, 0)
  
  # Changing claimType from "yes"/"no" to 1/0
  # can easily count
  # Checking no NA's in claimType column
  if(sum(is.na(data$ClaimType)) == 0) {
    print("Claimtype entries complete")
  }
  data$ClaimType <- ifelse(data$ClaimType=="Inpatient", 1, 0)
  # rename column to inpatient
  names(data)[names(data) == "ClaimType"] <- "Inpatient"
 


# Creating new variables for modelling
  # num chronic conditions
  data <- data %>%
    mutate("ChronicCond_MixedNum" = 22 - rowSums(.[38:48])) # Hardcoded values for Chronic diseases
  data$ChronicCond_MixedNum <- as.integer(data$ChronicCond_MixedNum)
  
  # if code changes
  data <- data %>%
    mutate("changedCode" = (ifelse(ClmAdmitDiagnosisCode == ClmDiagnosisCode_1, 0, 1)))
  data$changedCode <- as.integer(data$changedCode)

  # Adding in the claim length in days
  data <- data %>%
    mutate(ClaimLength = ClaimEndDt - ClaimStartDt)
  data$ClaimLength <- as.numeric(data$ClaimLength)
  
  
  # sanity check on variables
  variables = colnames(data)
  for (var in variables) {
    print(var)
    print(summary(na.omit(data[,var])))
    
  }

  # calculating age (in years) at start of claim
  # ASSUMPTION MADE - since there are many providers with no claim start dt
  # the minimum start date was chosen of all claims
  # Its assumed that a year difference doesnt have a significant effect
  data$Age <- as.numeric((data$ClaimStartDt - data$DOB)/365.25)
  
  # Creates categorical variables for all variables.
  # If they contain a NA value, it is set to the baseline value of a regression (e.g. usually 0)
  # and the NA categorical value is set to 1
  
  data <- data %>% mutate(hos_stay_len =  as.numeric(DischargeDt - AdmissionDt)) %>%
    mutate(AttendingPhysicianNA = is.na(AttendingPhysician)) %>%
    mutate(OperatingPhysicianNA = is.na(OperatingPhysician)) %>%
    mutate(OperatingPhysicianNA = is.na(OperatingPhysician)) %>%
    mutate(ClmDiagnosisCodeNA = is.na(ClmDiagnosisCode_1)) %>%
    mutate(DODNA = !is.na(DOD)) %>%
    mutate(changedCodeNA = is.na(changedCode) & Inpatient == 0) %>%
    mutate(ClmAdmitDiagnosisCodeNA = is.na(ClmAdmitDiagnosisCode) &  Inpatient == 0)%>%
    mutate(ClmProcedureCodeNA = is.na(ClmProcedureCode_1) & Inpatient == 1)
  
  
  
  # INPATIENTS MAY NOT HAVE A START DATE - CHECK THE LENGTH
  for(i in 1:length(data)) {
    if(any(is.na(data[,i]))) { # if there is an na value in column
      # indicatorVarName <- paste0(colnames(data)[i], "NA")
      #
      # data[[indicatorVarName]] <- as.integer(is.na(data[,i]))
      #
      # replaces an na value in a numeric column with the minimum value
      if(class(data[, i]) != "character") { # checks to see if character first -> doesnt make sense to take a minimum of a char
        data[,i][is.na(data[, i])] <- mean(na.omit(data[,i]))


        # better way to impute these. Create a linear regression model to approximate the value
        # use caret preProcess
      }
    }
  }
  
# # impute missing values
  # preImputesd <- c(rep(0, length(data)))
  # preImputeMean <- c(rep(0, length(data)))
  # for(i in 1:length(data)) {
  #   preImputesd[i] <- sd(as.numeric(na.omit(data[,i])))
  #   preImputeMean[i] <- mean(as.numeric(na.omit(data[,i])))
  # }
  # 
  # # imputting NA values with KNN
  # data_toimp <- dplyr::select_if(data, is.numeric) # I think they need to be numeric for knn to work?
  # # data_toimp <- data[-(colSums(is.na(data)) >= 19*length(data$BeneID)/20)]
  # # myK = sum(apply(data, 1, function(r) all(!is.na(r))))
  # # 
  # # https://stackoverflow.com/questions/41813372/how-can-you-improve-computation-time-when-predicting-knn-imputation
  # preProcValues <- preProcess(data_toimp, method = "knnImpute", k = 3)
  # data_imp <- predict(preProcValues, data_toimp)
  # 
  # # de-normalising data
  # procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
  # for(i in procNames$col){
  #   impute_claim_info[i] <- impute_claim_info[i]*preProcValues$std[i]+preProcValues$mean[i] 
  # }
  
  # finds most fraudulent physicians
  RANK = 5
  # creates indicator variables of wether a claim comes from a fraudulent state/county or physician
  {
    fraudPhys <- function(data, RANK) {
      fraud.data.phys <- data %>%
        filter(Fraud == 1) %>%
        group_by(AttendingPhysician) %>%
        summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
        mutate(rank = rank(-Count, ties.method = 'first')) %>%
        filter(rank <= RANK) 
      fraud.data.phys <- fraud.data.phys[order(fraud.data.phys$rank),] # ordering it as ranked
      fraud.data.phys$AttendingPhysician <- factor(fraud.data.phys$AttendingPhysician, fraud.data.phys$AttendingPhysician) # adding in as factors
      # ^ Used to plot in order
      return(fraud.data.phys)
    }
    notFraudPhys <- function(data, RANK) {
      notfraud.data.phys <- data %>%
        filter(Fraud == 0) %>%
        group_by(AttendingPhysician) %>%
        summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
        mutate(rank = rank(-Count, ties.method = 'first')) %>%
        filter(rank <= RANK) 
      notfraud.data.phys <- notfraud.data.phys[order(notfraud.data.phys$rank),] # ordering it as ranked
      notfraud.data.phys$AttendingPhysician <- factor(notfraud.data.phys$AttendingPhysician, notfraud.data.phys$AttendingPhysician) # adding in as factors
      return(notfraud.data.phys)
    }
    plotphys <- function(fraud.data.phys, notfraud.data.phys) {
      data.phys <- full_join(fraud.data.phys, notfraud.data.phys)
      fraud.phys.plot <- ggplot(data = fraud.data.phys, aes(x = AttendingPhysician, y = Count)) + 
        geom_col(fill = "blue") +
        ylab("") + 
        xlab("") +
        ggtitle("Fraud") +
        coord_cartesian(ylim = c(0, max(data.phys$Count)))
      notfraud.phys.plot <- ggplot(data = notfraud.data.phys, aes(x = AttendingPhysician, y = Count)) + 
        geom_col(fill = "red") + 
        ylab("") + 
        xlab("") +
        ggtitle("Not Fraud") +
        coord_cartesian(ylim = c(0, max(data.phys$Count)))
      phys.plot <- ggarrange(fraud.phys.plot, notfraud.phys.plot)
      annotate_figure(phys.plot,
                      top = text_grob("Most active Physicians"),
                      left = "Count",
                      bottom = "Attending Physician"
      )
    }
    fraudPhys(data, RANK)
    notFraudPhys(data, RANK)
    plotphys(fraudPhys(data, RANK), notFraudPhys(data, RANK))
    
    FraudyPhys <- as.character((fraudPhys(data, RANK)[,1][[1]]))
    
    data$FraudPhys <- match(as.character(data$AttendingPhysician), as.character((fraudPhys(data, RANK)[,1][[1]])))
    data$FraudPhys[is.na(data$FraudPhys)] <- 0
    
    # Do the same as above for county and state
    fraudCounty <- function(data, RANK) {
      fraud.data <- data %>%
        filter(Fraud == 1) %>%
        group_by(County) %>%
        summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
        mutate(rank = rank(-Count, ties.method = 'first')) %>%
        filter(rank <= RANK) 
      fraud.data <- fraud.data[order(fraud.data$rank),] # ordering it as ranked
      fraud.data$County <- factor(fraud.data$County, fraud.data$County) # adding in as factors
      # ^ Used to plot in order
      return(fraud.data)
    }
    notfraudCounty <- function(data, RANK) {
      notfraud.data <- data %>%
        filter(Fraud == 0) %>%
        group_by(County) %>%
        summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
        mutate(rank = rank(-Count, ties.method = 'first')) %>%
        filter(rank <= RANK) 
      notfraud.data <- notfraud.data[order(notfraud.data$rank),] # ordering it as ranked
      notfraud.data$County <- factor(notfraud.data$County, notfraud.data$County) # adding in as factors
      # ^ Used to plot in order
      return(notfraud.data)
    }
    
    FraudyCounty <- as.character((fraudCounty(data, RANK)[,1][[1]]))
    
    data$FraudCounty <- match(as.character(data$County), as.character((fraudCounty(data, RANK)[,1][[1]])))
    data$FraudCounty[is.na(data$FraudCounty)] <- 0
    
    fraudState <- function(data, RANK) {
      fraud.data <- data %>%
        filter(Fraud == 1) %>%
        group_by(State) %>%
        summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
        mutate(rank = rank(-Count, ties.method = 'first')) %>%
        filter(rank <= RANK) 
      fraud.data <- fraud.data[order(fraud.data$rank),] # ordering it as ranked
      fraud.data$State <- factor(fraud.data$State, fraud.data$State) # adding in as factors
      # ^ Used to plot in order
      return(fraud.data)
    }
    notfraudState <- function(data, RANK) {
      notfraud.data <- data %>%
        filter(Fraud == 0) %>%
        group_by(State) %>%
        summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
        mutate(rank = rank(-Count, ties.method = 'first')) %>%
        filter(rank <= RANK) 
      notfraud.data <- notfraud.data[order(notfraud.data$rank),] # ordering it as ranked
      notfraud.data$State <- factor(notfraud.data$State, notfraud.data$State) # adding in as factors
      # ^ Used to plot in order
      return(notfraud.data)
    }
    
    FraudyState <- as.character((fraudState(data, RANK)[,1][[1]]))
    
    data$FraudState <- match(as.character(data$State), as.character((fraudState(data, RANK)[,1][[1]])))
    data$FraudState[is.na(data$FraudState)] <- 0
  }
  
  # Finds average claims per beneficiary for each provider
    # introduce counties, race and state here
  data.bene <- data %>%
    group_by(BeneID, ProviderID, Race) %>%
    summarise("NumClaims" = n())
  data.bene <- data.bene %>%
    group_by(ProviderID) %>%
    summarise("AvgClaimsPerBene" = mean(NumClaims), "ModeRace" = mlv(Race, method = 'mfv', na.rm = TRUE)[1])
  data.bene$ModeRace <- as.factor(data.bene$ModeRace)
  
# Aggregating data per provider
  # Lose a lot of variables in this step (e.g. physicians etc). Need to think of a way to retain them
  # Bootstrap thoughts
    # Could potentially think of doing bootstrap to get std error of the averages
    # would also give p-values on how reliable each average is
  data.agg <- aggregate(data, by = list(data$ProviderID), FUN = mean) # Takes the mean of all the other columns for each provider
  data.agg <- subset(data.agg, select = -c(ProviderID))
  names(data.agg)[names(data.agg) == "Group.1"] <- "ProviderID"
  data.agg$ProviderID <- as.character(data.agg$ProviderID) # changing provider ID to character variable again
  data.agg2 <- aggregate(data, by = list(data$ProviderID), FUN = length) # Takes the number of claims per provider id
  data.agg <- data.agg %>% mutate(num_clms = data.agg2$BeneID) # mixes the two datasets
  data.agg$num_clms <- as.double(data.agg$num_clms) # Changes to double for later loop
  data.agg <- left_join(data.agg, data.bene) # adds average claims per benficiary
  
  # Removes full NA columns
  data.agg <- data.agg[,colSums(is.na(data.agg))<nrow(data.agg)]
  # changes Fraud class to integer
  data.agg.np$Fraud <- as.integer(data.agg.np$Fraud)
  
  data.agg.np <- data.agg[,-1] # no providers
  data.agg.np <- data.agg.np %>% # changes date to days since 2008-11-27 (GIVES SOME WEIRD VALUES)
    mutate("StartDtdays_from_271108" = as.numeric(ClaimStartDt - as.Date("2008-11-27"))) %>%
    mutate("EndDtdays_from_271108" = as.numeric(ClaimEndDt - as.Date("2008-11-27")))
  data.agg.np <- data.agg.np[,-which(sapply(data.agg.np, class) == "Date")]
  
  # removes Claim start date, claim end date and some other variables deemed inappropriate for aggregated data
    # data.agg <- data.agg %>%
    # dplyr::select(-c(ClaimStartDt, ClaimEndDt, AdmissionDt, DischargeDt, DOD, changedCode, ClaimLength, DOB))
    # 
    # data.agg.np <- data.agg[,-1] # no providers
    
  data.agg.np$Fraud <- as.factor(data.agg.np$Fraud)
  levels(data.agg.np$Fraud)[1] <- "No"
  levels(data.agg.np$Fraud)[2] <- "Yes"  
  
  # Splitting into training data for aggregate providers
  set.seed(1)
  train.set <- sample(length(data.agg$ProviderID), length(data.agg$ProviderID)/2) # this code taken from tutorials
  train <- (seq(1, length(data.agg$ProviderID)) %in% train.set)
  
{
  full.model <- glm(Fraud ~ ., family = "binomial", data = data.agg.np, subset = train)
  nothing <- glm(Fraud ~ 1,family= "binomial", data = data.agg.np)
  pred.full <- predict(full.model, data.agg.np[!train,], type = "response")
  full.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.full), main = "Logisitic regression")
  
  backwards <- step(full.model)
  summary(backwards)
  pred.back <- predict(backwards, data.agg.np[!train,], type = "response")
  back.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.back), main = "Logisitic regression")
  
  forwards <- step(nothing, scope=list(lower=formula(nothing),upper=formula(full.model)), direction="forward")
  pred.forw <- predict(forwards, data.agg.np[!train,], type = "response")
  forw.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.forw), main = "Logisitic regression", col = "blue")
  lines(full.rocplot, col = "black")
  lines(back.rocplot, col = "red")
  legend(0.4, 0.4, legend = c("Backwards", "Full", "Forwards"), col = c("Red", "Black", "Blue"), lty = 1)
  
  
  # Ridge regression (UNSURE IF THESE SHOULD BE IN THE BINOMIAL FAMILY, prediction outputs weird values)
  lambda.grid <- exp(seq(-5,0,length=100)) # grid of all lambdas to be tested
  data.agg.np.modelmatrix <- model.matrix(Fraud ~ ., data.agg.np)[,-1]
  fit.ridge <- glmnet(x = data.agg.np.modelmatrix[train,], y = as.matrix(data.agg.np[train,FraudIndx]), family = "binomial",
                      alpha=0, lambda=lambda.grid)
  cv.fit.ridge <- cv.glmnet(x = data.agg.np.modelmatrix[train,-FraudIndx],y = data.agg.np[train,FraudIndx],
                            alpha=0, family = "binomial", lambda=lambda.grid, type.measure = "class") # type.measure = class gives misclassification error
  plot(fit.ridge, xvar = "dev", label = TRUE)
  plot(cv.fit.ridge)
  ridge.coef <- coef(cv.fit.ridge, s = "lambda.min")
  
  # ridge error
  # type = response gives probabilities
  # type = class gives predicition of either 1 or 0
  ridge.predict <- predict(fit.ridge, s=cv.fit.ridge$lambda.min, newx = data.agg.np.modelmatrix[!train,], type = "response") 
  ridge.predict <- as.numeric(ridge.predict)
  ridge.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = ridge.predict), main = "Ridge ROC")
  
  
  # ridge with caret
  # trainControl <- trainControl(method = "cv",
  #                              number = 10,
  #                              # Compute Recall, Precision, F-Measure
  #                              #summaryFunction = prSummary,
  #                              # prSummary needs calculated class probs
  #                              classProbs = T)
  # fit.ridge <- train(Fraud ~ ., 
  #                data=data.agg.np[train,],
  #                method = 'glmnet', 
  #                trControl = trainControl,
  #                family = "binomial",
  #                tuneGrid=expand.grid(
  #                   .alpha=0,
  #                   .lambda=10^seq(10,-2,length=100)
  #                   )
  # ) 
  # plot(fit.ridge)
  # pred.ridge <- predict(fit.ridge, data.agg.np[!train,], type = "prob")
  # 
  # plot(roc(data.agg.np[!train, FraudIndx], pred.ridge[,2]))
  
  # Lasso regression (UNSURE IF THESE SHOULD BE IN THE BINOMIAL FAMILY, prediction outputs weird values)
  lambda.grid <- exp(seq(-5,-2,length=100)) # grid of all lambdas to be tested
  data.agg.np.modelmatrix <- model.matrix(Fraud ~ ., data.agg.np)[,-1]
  fit.lasso <- glmnet(x = data.agg.np.modelmatrix[train,], y = as.matrix(data.agg.np[train,FraudIndx]), family = "binomial",
                      alpha=1, lambda=lambda.grid)
  cv.fit.lasso <- cv.glmnet(x = data.agg.np.modelmatrix[train,-FraudIndx],y = data.agg.np[train,FraudIndx],alpha=1,
                            family = "binomial", lambda=lambda.grid, type.measure = "class") # type.measure = class gives misclassification error
  plot(fit.lasso, xvar = "dev", label = TRUE)
  plot(cv.fit.lasso)
  lasso.coef <- coef(cv.fit.lasso, s = "lambda.min")
  
  # lasso error
  # type = response gives probabilities
  # type = class gives predicition of either 1 or 0
  lasso.predict <- predict(fit.lasso, s=cv.fit.lasso$lambda.min, newx = data.agg.np.modelmatrix[!train,], type = "response") 
  lasso.predict <- as.numeric(lasso.predict)
  lasso.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = lasso.predict), main = "Penalised regression ROC")
  lines(ridge.rocplot, col = "red")
  legend(0.4, 0.4, legend = c("Lasso", "Ridge"), col = c("Black", "Red"), lty = 1)
  
  
# LDA and QDA
  fitlda = train(Fraud ~ ., data=data.agg.np[train,], method="lda",
                 preProcess = c("center", "scale"),
                 metric = "Accuracy",
                 trControl = trainControl(method = "cv"))
  pred.lda <- predict(fitlda, data.agg.np[!train,], type = "prob")
  lda.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.lda[,1]), main = "LDA ROC")
  
  
  # THIS QDA NOT WORKING!
  trCtrl = trainControl(method = "cv", number = 10)
  fitqda = train(Fraud ~ ., data=data.agg.np[train,], method="qda", 
                 preProcess = c("center", "scale"),
                 metric = "Accuracy",
                 trControl = trCtrl)
  
  pred.qda <- predict(fitqda, data.agg.np[!train,], type = "prob")
  qda.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.qda[,1]), main = "QDA ROC")
  
  
  
  # KNN
  ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
  knnFit <- train(as.factor(Fraud) ~ ., data = data.agg.np[train,], method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
  
  #Output of kNN fit
  knnFit
  plot(knnFit, type = "S")
  pred.knn <- predict(knnFit, newdata = data.agg.np[!train,], type = "prob")[,2]
  predVal.knn <- predict(knnFit, newdata = data.agg.np[!train,])
  
  confusionMatrix(predVal.knn, as.factor(data.agg.np[!train,]$Fraud))
  KNN.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.knn), main = "KNN ROC")
  
  
  
  FraudIndx <- which(colnames(data.agg.np) == "Fraud")

# tree based methods
  # normal tree
  
  fit.tree <- train(Fraud ~ ., 
                    data=data.agg.np[train,], 
                    method="rpart", 
                    trControl = trainControl(method = "cv"), 
                    tuneLength = 10)
  plot(fit.tree)
  fit.tree$bestTune
  plot(fit.tree$finalModel, uniform=TRUE,
       main="Classification Tree")
  text(fit.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
  
  
  
  pred.tree <- predict(fit.tree, data.agg.np[!train, ], type = "prob")
  predVal.tree <- predict(fit.tree, newdata = data.agg.np[!train,])
  
  confusionMatrix(predVal.tree, as.factor(data.agg.np[!train,]$Fraud))
  plot(roc(data.agg.np[!train, FraudIndx], pred.tree[,2]))
  
  
  # boosting
  trCtrl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    repeats = 10)
  fit.boosttree <- train(Fraud ~ ., 
                    data=data.agg.np[train,], 
                    method="gbm", 
                    trControl = trCtrl, 
                    verbose = FALSE)
  
  
  
  plot(fit.boosttree)
  fit.boosttree$bestTune
  plot(fit.boosttree$finalModel, uniform=TRUE,
       main="Classification Tree")
  
  pred.boosttree <- predict(fit.boosttree, data.agg.np[!train, ], type = "prob")
  plot(roc(data.agg.np[!train, FraudIndx], pred.boosttree[,2]))
  
  sum.boost <-summary(fit.boosttree, cBars = 5)
  barplot(sum.boost$rel.inf[1:5], names.arg = sum.boost$var[1:5], cex.names = 0.7, main = "Most important boost variables")
  
  # bagging (the case where m = p)
  
  # random forests
  
  #10 folds repeat 3 times
  trCtrl <- trainControl(method='repeatedcv', 
                          number=10, 
                          repeats=3)
  #Metric compare model is Accuracy
  metric <- "Accuracy"
  #Number randomely variable selected is mtry
  mtry <- sqrt(ncol(data.agg.np))
  tunegrid <- expand.grid(.mtry=mtry)
  fit.forest <- train(Fraud~., 
                      data=data.agg.np[train,], 
                      method='rf', 
                      metric='Accuracy', 
                      tuneGrid=tunegrid, 
                      trControl=trCtrl)
  
  pred.forest <- predict(fit.forest, data.agg.np[!train, ], type = "prob")
  predVal.forest <- predict(fit.forest, data.agg.np[!train, ])
  normForest.conf <- confusionMatrix(predVal.forest, data.agg.np[!train, FraudIndx])
  
  roc.forest <- plot(roc(data.agg.np[!train, FraudIndx], pred.forest[,2]))
  lines(roc(response = data.agg$Fraud[!train], predictor = pred.boosttree[,2]), col = "Blue") # think i need to pick my lamda better
  lines(roc(data.agg.np[!train, FraudIndx], pred.tree[,2]), col = "Green")
  
  # SUPPORT VECTOR MACHINES FOR CLASSIFICATION PROBLEM
  train_control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE)
  fit.svm.lin <- train(Fraud ~., data = data.agg.np[train,], method = "svmLinear", 
                       trControl = train_control,  preProcess = c("center","scale"))
  pred.svm.lin <- predict(fit.svm.lin, newdata = data.agg.np[!train,], type = "prob")
  roc.svm.lin <- roc(response = data.agg$Fraud[!train], predictor = pred.svm.lin[,1])
  plot(roc.svm.lin)
  
  fit.svm.rad <- train(Fraud ~., data = data.agg.np[train,], method = "svmRadial", 
                       trControl = train_control,  preProcess = c("center","scale"))
  pred.svm.rad <- predict(fit.svm.rad, newdata = data.agg.np[!train,], type = "prob")
  roc.svm.rad <- roc(response = data.agg$Fraud[!train], predictor = pred.svm.rad[,1])
  plot(roc.svm.rad)
  # rad is better
  plot(roc.svm.rad, main = "Radial kernel vs Linear kernel - SVM")
  lines(roc.svm.lin, col = "red")
  legend(0.4, 0.4, legend = c("Radial", "Linear"), col = c("Black", "Red"), lty = 1)
  
  svm.rocplot <- plot(roc.svm.rad)
  
# investigating effects of polynomial regression on each variable (data is seperation of quasi seperation - should use ridge/lasso i think)
  # https://community.rstudio.com/t/logistic-regression-model-glm-fit-fitted-probabilities-numerically-0-or-1-occurred/9828
  setwd("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/DataB/CVPlots")
  for(i in 1:(length(data.agg.np))) {
    var <- colnames(data.agg.np[i])
    print(paste("CV polynomial regression plot of", var))
    if(class(data.agg.np[,var]) == "numeric") {
      cv.error <- rep(0, 5)
      for (i in 1:5) {
        fit.poly <- glm(Fraud ~ poly(get(var), i, raw = TRUE), data = data.agg.np[train,], family = "binomial")
        cv.error[i] <- cv.glm(data.agg.np[train,], fit.poly, K= 10)$delta[1] # uses boot
      }
      png(filename = paste0("CV error of ", var, ".png"))
      plot(1:5, cv.error, type = "l", main = paste("CV error of ", var), xlab = "polynomial degree")
      dev.off()
    }
  }
  setwd("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/DataB")
  
  # Saving plots of variables against each other to use KNN
  setwd("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/DataB/vsPlots")
  all.var <- colnames(data.agg)[-c(1:3, 7:9, 14:22, 29, 46:47)]
  for (var1 in all.var) {
    if(typeof(data.agg[,var1]) == "double") {
      for (var2 in all.var) {
        if(typeof(data.agg[,var2]) == "double" && (var1 != var2)) {
          title <- paste0("Plot of ", var1, " vs ", var2)
          print(paste0("Plot of ", var1, " vs ", var2, ".png"))
          p1 <- ggplot(data = data.agg, aes(x = get(var1), y = get(var2), color = factor(Fraud))) +
            geom_point() +
            xlab(var1) +
            ylab(var2) +
            guides(col = guide_legend("Fraud")) +
            ggtitle(title)
          ggsave(paste0("Plot of ", var1, " vs ", var2, ".png"), plot = p1)
          while (!is.null(dev.list()))  dev.off()
        }
      }
    }
  }
  setwd("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/DataB")
  
  
  
  # for classification problems-
    # logisitic regression
    # LDA
        # ^ linear boundaries
    # QDA
    # Decision trees
        # ^ nonlinear boundaries
  
  # These will be my most helpful
  # Compare two independent variables and find the boundary between these variables which seperates the classes (response)
  # This boundary can be either from -
  # KNN
  # Trees
  # support vector machines
  
  # logistic regression is preferred to SVM's when there are more categories overlapping. SVM's preferred when clearer seperation
  

  basicrocplots <- function() {
    plot(log.rocplot, col = 1, main = "No interaction term ROC curve")
    lines(lda.rocplot, col = 2)
    # lines(qda.rocplot, col = 3)
    lines(lasso.rocplot, col = 4)
    lines(ridge.rocplot, col = 5)
    lines(KNN.rocplot, col = 6)
    lines(svm.rocplot, col = 7)
    lines(roc.forest, col = 8)
    # lines(pcr.rocplot, col = 4)
    # lines(pls.rocplot, col = 5)
    legend(0.2, 0.6, legend = c("Log", "LDA", "Lasso", "Ridge", "KNN", "SVM", "Forest"), col = 1:8, cex = 0.6, lty = 1)
  }
  
  basiclogrocplots <- function() {
    plot(log.rocplot, col = 1, main = "No interaction term ROC curve")
    lines(lda.rocplot, col = 2)
    # lines(qda.rocplot, col = 3)
    lines(lasso.rocplot, col = 4)
    lines(ridge.rocplot, col = 5)
    lines(KNN.rocplot, col = 6)
    legend(0.2, 0.6, legend = c("Log", "LDA", "Lasso", "Ridge", "KNN"), col = 1:6, cex = 0.6, lty = 1)
  }
}
  # interaction terms to look at from the interesting plots
    # ChronicCond_MixedNum and InscClaimAmtReimbused
    # num_clms and Inpatient
  
    data.agg.inter <- data.agg.np %>%
    mutate("CondNumXInscClaimReimbursed" = ChronicCond_MixedNum * InscClaimAmtReimbursed) %>%
    mutate("numClmsXinpatient" = num_clms * Inpatient)
  
# log with interaction terms
    full.model <- glm(Fraud ~ ., family = "binomial", data = data.agg.inter, subset = train)
    nothing <- glm(Fraud ~ 1,family= "binomial", data = data.agg.inter)
    pred.full <- predict(full.model, data.agg.inter[!train,], type = "response")
    full.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.full), main = "Logisitic regression")
    
    backwards <- step(full.model)
    summary(backwards)
    pred.back <- predict(backwards, data.agg.inter[!train,], type = "response")
    back.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.back), main = "Logisitic regression")
    
    forwards <- step(nothing, scope=list(lower=formula(nothing),upper=formula(full.model)), direction="forward")
    pred.forw <- predict(forwards, data.agg.inter[!train,], type = "response")
    forw.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.forw), main = "Logisitic regression")
    
    plot(back.rocplot, col = "red", main = "Logisitic regression")
    lines(full.rocplot, col = "black")
    lines(forw.rocplot, col = "blue")
    legend(0.4, 0.4, legend = c("Backwards", "Full", "Forwards"), col = c("Red", "Black", "Blue"), lty = 1)
    
    bestlog.inter <- forwards
    log.rocplot.inter <- forw.rocplot
    
    
    # Ridge regression (UNSURE IF THESE SHOULD BE IN THE BINOMIAL FAMILY, prediction outputs weird values)
    lambda.grid <- exp(seq(-8,0,length=100)) # grid of all lambdas to be tested
    data.agg.inter.modelmatrix <- model.matrix(Fraud ~ ., data.agg.inter)[,-1]
    fit.ridge <- glmnet(x = data.agg.inter.modelmatrix[train,], y = as.matrix(data.agg.inter[train,FraudIndx]), family = "binomial",
                        alpha=0, lambda=lambda.grid)
    cv.fit.ridge <- cv.glmnet(x = data.agg.inter.modelmatrix[train,-FraudIndx],y = data.agg.inter[train,FraudIndx],
                              alpha=0, family = "binomial", lambda=lambda.grid, type.measure = "class") # type.measure = class gives misclassification error
    plot(fit.ridge, xvar = "dev", label = TRUE)
    plot(cv.fit.ridge)
    ridge.coef <- coef(cv.fit.ridge, s = "lambda.min")
    
    # ridge error
    # type = response gives probabilities
    # type = class gives predicition of either 1 or 0
    ridge.predict <- predict(fit.ridge, s=cv.fit.ridge$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "response") 
    ridge.predict <- as.numeric(ridge.predict)
    ridge.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = ridge.predict), main = "Ridge ROC")
    
    
    # Lasso regression (UNSURE IF THESE SHOULD BE IN THE BINOMIAL FAMILY, prediction outputs weird values)
    lambda.grid <- exp(seq(-8,-2,length=100)) # grid of all lambdas to be tested
    data.agg.inter.modelmatrix <- model.matrix(Fraud ~ ., data.agg.inter)[,-1]
    fit.lasso <- glmnet(x = data.agg.inter.modelmatrix[train,], y = as.matrix(data.agg.inter[train,FraudIndx]), family = "binomial",
                        alpha=1, lambda=lambda.grid)
    cv.fit.lasso <- cv.glmnet(x = data.agg.inter.modelmatrix[train,-FraudIndx],y = data.agg.inter[train,FraudIndx],alpha=1,
                              family = "binomial", lambda=lambda.grid, type.measure = "class") # type.measure = class gives misclassification error
    plot(fit.lasso, xvar = "dev", label = TRUE, xlim = c(0.35, 0.42))
    plot(cv.fit.lasso)
    lasso.coef <- coef(cv.fit.lasso, s = "lambda.min")
    
    # lasso error
    # type = response gives probabilities
    # type = class gives predicition of either 1 or 0
    lasso.predict <- predict(fit.lasso, s=cv.fit.lasso$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "response") 
    lasso.predict <- as.numeric(lasso.predict)
    lasso.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = lasso.predict), main = "Penalised regression ROC")
    lines(ridge.rocplot.inter, col = "red")
    lines(log.rocplot.inter, col = "blue")
    legend(0.4, 0.4, legend = c("Lasso", "Ridge", "Log Reg"), col = c("Black", "Red", "blue"), lty = 1)
    
    
    predVal.lasso <- predict(fit.lasso, s=cv.fit.ridge$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "class")
    predVal.ridge <- predict(fit.ridge, s=cv.fit.ridge$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "class") 
    predVal.log <- predict(forwards, data.agg.inter[!train,], type = "response")
    predVal.log <- ifelse(predVal.log >= 0.5, "Yes", "No")
    
    
    confusionMatrix(as.factor(predVal.lasso), as.factor(data.agg.np[!train,]$Fraud))
    confusionMatrix(as.factor(predVal.ridge), as.factor(data.agg.np[!train,]$Fraud))
    confusionMatrix(as.factor(predVal.log), as.factor(data.agg.np[!train,]$Fraud))
    
    
# LDA and QDA
    
    
    fitlda <- lda(Fraud ~ ., data = data.agg.inter, subset = train)
    pred.lda <- predict(fitlda, newdata = data.agg.inter[!train,])$posterior[,1]
    lda.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.lda), main = "LDA ROC")
    
    # too much collinearality above, changed to only the coefficients from the forwards
    fitqda <- qda(as.formula(paste("Fraud ~", paste(names(coef(forwards)[-1]), collapse = " + "))), data = data.agg.inter, subset = train)
    pred.qda <- predict(fitqda, newdata = data.agg.inter[!train,])$posterior[,1]
    qda.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.qda), main = "QDA ROC")
    
    
# svm with interaction terms
    # note svm gets worse
    train_control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE)
    fit.svm.lin.inter <- train(Fraud ~., data = data.agg.inter[train,], method = "svmLinear", 
                  trControl = train_control,  preProcess = c("center","scale"))
    pred.svm.lin.inter <- predict(fit.svm.lin.inter, newdata = data.agg.inter[!train,], type = "prob")
    roc.svm.lin.inter <- roc(response = data.agg$Fraud[!train], predictor = pred.svm.lin.inter[,1])
    plot(roc.svm.lin.inter)
    
    fit.svm.rad.inter <- train(Fraud ~., data = data.agg.inter[train,], method = "svmRadial", 
                           trControl = train_control,  preProcess = c("center","scale"))
    pred.svm.rad.inter <- predict(fit.svm.rad.inter, newdata = data.agg.inter[!train,], type = "prob")
    roc.svm.rad.inter <- roc(response = data.agg$Fraud[!train], predictor = pred.svm.rad.inter[,1])
    plot(roc.svm.rad.inter)
    lines(roc.svm.lin.inter, col = "red")
    
    svm.rocplot.inter <- roc.svm.lin.inter # linear kernel is better
    
# knn with interaction terms
    # data.agg.inter$Fraud <- as.factor(data.agg.inter$Fraud)
    # levels(data.agg.inter$Fraud)[1] <- "No"
    # levels(data.agg.inter$Fraud)[2] <- "Yes"
    ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
    knnFit.inter <- train(Fraud ~ ., data = data.agg.inter[train,], method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    
    #Output of kNN fit
    knnFit.inter
    plot(knnFit.inter, type = "S")
    pred.knn.inter <- predict(knnFit.inter, newdata = data.agg.inter[!train,], type = "prob")[,2]
    predVal.knn.inter <- predict(knnFit.inter, newdata = data.agg.inter[!train,])
    
    confusionMatrix(predVal.knn.inter, as.factor(data.agg.inter[!train,]$Fraud))
    KNN.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.knn.inter), main = "KNN ROC")
    
    

    interrocplots <- function() {
      plot(log.rocplot.inter, col = 1, main = "Interaction term ROC curve")
      lines(lda.rocplot.inter, col = 2)
      # lines(qda.rocplot.inter, col = 3)
      lines(lasso.rocplot.inter, col = 4)
      lines(ridge.rocplot.inter, col = 5)
      lines(KNN.rocplot.inter, col = 6)
      lines(svm.rocplot.inter, col = 7)
      lines(roc.forest, col = 8)
      lines(plsda.roc, col = 9)
      # lines(pcr.rocplot, col = 4)
      # lines(pls.rocplot, col = 5)
      legend(0.2, 0.6, legend = c("Log", "LDA", "QDA", "Lasso", "Ridge", "KNN", "SVM", "Forest", "plsda"), col = 1:9, cex = 0.6, lty = 1)
    }
    
    interlogrocplots <- function() {
      plot(log.rocplot.inter, col = 1, main = "With Interaction term ROC curve")
      lines(lda.rocplot.inter, col = 2)
      lines(qda.rocplot.inter, col = 3)
      lines(lasso.rocplot.inter, col = 4)
      lines(ridge.rocplot.inter, col = 5)
      lines(KNN.rocplot.inter, col = 6)
      legend(0.2, 0.6, legend = c("Log", "LDA", "QDA", "Lasso", "Ridge", "KNN"), col = 1:6, cex = 0.6, lty = 1)
    }
    
    rocs <- list(log.rocplot.inter, lda.rocplot.inter, #qda.rocplot.inter, 
              lasso.rocplot.inter, ridge.rocplot.inter, KNN.rocplot.inter, svm.rocplot.inter, roc.forest,
              plsda.roc)
    aucs <- c()
    for(i in 1:length(rocs)) {
      aucs[i] <- rocs[[i]]$auc
    }
    best_model <- rocs[which.max(aucs)]
    
    # forest gives best AUC    
    
    # find distance from (1,1) at each point
    forestDist <- data.frame("threshold" = roc.forest$thresholds)
    forestDist$Distance <- sqrt((1 - roc.forest$specificities)^2 + (1-roc.forest$sensitivities)^2)
    forestDist$Specificities <- roc.forest$specificities
    forestDist$Sensitivities <- roc.forest$sensitivities
    forestDist <- melt(forestDist, id.vars = "threshold", 
                       variable.name = "Resampled",
                       value.name = "Data")
    ggplot(forestDist, aes(x = threshold, y = Data, color = Resampled)) +
      geom_line(size = 1.5)
    
####################################################################################################
    ####################################################################################################
    ####################################################################################################
    ####################################################################################################
    
# up sampling
    imbal_train <- data.agg.inter[train,]
    imbal_test <- data.agg.inter[!train,]
    table(imbal_train$Fraud)
    
    imbal_train_tree <-data.agg.np[train,]

# SMOTE sampling
    smote_train <- SMOTE(Fraud ~ ., data = imbal_train)
    smote_train_tree <- SMOTE(Fraud ~ ., data = imbal_train_tree)
    table(smote_train$Fraud)

# logistic regression    
    full.model <- glm(Fraud ~ ., family = "binomial", data = smote_train)
    nothing <- glm(Fraud ~ 1,family= "binomial", data = smote_train)
    pred.full <- predict(full.model, data.agg.inter[!train,], type = "response")
    full.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.full), main = "Logisitic regression")
    
    backwards <- step(full.model)
    summary(backwards)
    pred.back <- predict(backwards, data.agg.inter[!train,], type = "response")
    back.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.back), main = "Logisitic regression")
    
    forwards <- step(nothing, scope=list(lower=formula(nothing),upper=formula(full.model)), direction="forward")
    pred.forw <- predict(forwards, data.agg.inter[!train,], type = "response")
    forw.rocplot <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.forw), main = "Logisitic regression")
    
    plot(back.rocplot, col = "red", main = "Logisitic regression")
    lines(full.rocplot, col = "black")
    lines(forw.rocplot, col = "blue")
    legend(0.4, 0.4, legend = c("Backwards", "Full", "Forwards"), col = c("Red", "Black", "Blue"), lty = 1)
    
    bestlog.inter <- backwards
    log.rocplot.inter <- back.rocplot
    
    
    # Ridge regression (UNSURE IF THESE SHOULD BE IN THE BINOMIAL FAMILY, prediction outputs weird values)
    lambda.grid <- exp(seq(-10,0,length=100)) # grid of all lambdas to be tested
    data.agg.inter.modelmatrix <- model.matrix(Fraud ~ ., smote_train)[,-1]
    fit.ridge <- glmnet(x = data.agg.inter.modelmatrix, y = as.matrix(smote_train[,FraudIndx]), family = "binomial",
                        alpha=0, lambda=lambda.grid)
    cv.fit.ridge <- cv.glmnet(x = data.agg.inter.modelmatrix[,-FraudIndx],y = smote_train[,FraudIndx],
                              alpha=0, family = "binomial", lambda=lambda.grid, type.measure = "class") # type.measure = class gives misclassification error
    plot(fit.ridge, xvar = "dev", label = TRUE)
    plot(cv.fit.ridge)
    ridge.coef <- coef(cv.fit.ridge, s = "lambda.min")
    
    # ridge error
    # type = response gives probabilities
    # type = class gives predicition of either 1 or 0
    ridge.predict <- predict(fit.ridge, s=cv.fit.ridge$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "response") 
    ridge.predict <- as.numeric(ridge.predict)
    ridge.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = ridge.predict), main = "Ridge ROC")
    
    
    # Lasso regression (UNSURE IF THESE SHOULD BE IN THE BINOMIAL FAMILY, prediction outputs weird values)
    lambda.grid <- exp(seq(-8,-2,length=100)) # grid of all lambdas to be tested
    data.agg.inter.modelmatrix <- model.matrix(Fraud ~ ., smote_train)[,-1]
    fit.lasso <- glmnet(x = data.agg.inter.modelmatrix, y = as.matrix(smote_train[,FraudIndx]), family = "binomial",
                        alpha=1, lambda=lambda.grid)
    cv.fit.lasso <- cv.glmnet(x = data.agg.inter.modelmatrix[,-FraudIndx],y = smote_train[,FraudIndx],alpha=1,
                              family = "binomial", lambda=lambda.grid, type.measure = "class") # type.measure = class gives misclassification error
    plot(fit.lasso, xvar = "dev", label = TRUE, xlim = c(0.4, 0.6))
    plot(cv.fit.lasso)
    lasso.coef <- coef(cv.fit.lasso, s = "lambda.min")
    
    # lasso error
    # type = response gives probabilities
    # type = class gives predicition of either 1 or 0
    lasso.predict <- predict(fit.lasso, s=cv.fit.lasso$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "response") 
    lasso.predict <- as.numeric(lasso.predict)
    lasso.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = lasso.predict), main = "Penalised regression ROC")
    lines(ridge.rocplot.inter, col = "red")
    lines(log.rocplot.inter, col = "blue")
    legend(0.4, 0.4, legend = c("Lasso", "Ridge", "Log Reg"), col = c("Black", "Red", "blue"), lty = 1)
    
    
    predVal.lasso <- predict(fit.lasso, s=cv.fit.ridge$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "class")
    predVal.ridge <- predict(fit.ridge, s=cv.fit.ridge$lambda.min, newx = data.agg.inter.modelmatrix[!train,], type = "class") 
    predVal.log <- predict(forwards, data.agg.inter[!train,], type = "response")
    predVal.log <- ifelse(predVal.log >= 0.5, "Yes", "No")
    
    
    lasso.conf <- confusionMatrix(as.factor(predVal.lasso), as.factor(data.agg.np[!train,]$Fraud))
    ridge.conf <- confusionMatrix(as.factor(predVal.ridge), as.factor(data.agg.np[!train,]$Fraud))
    log.conf <- confusionMatrix(as.factor(predVal.log), as.factor(data.agg.np[!train,]$Fraud))
    
    
    
    
# tree
    smotefit.tree <- train(Fraud ~ ., 
                      data=smote_train_tree, 
                      method="rpart", 
                      trControl = trainControl(method = "cv"), 
                      tuneLength = 10)
    plot(smotefit.tree, xlim = c(0, 0.05), ylim = c(0.84, 0.87), main = "Choosing Tree Cp through CV")
    smotefit.tree$bestTune
    par(mar = c(2, 5, 5, 1))
    plot(smotefit.tree$finalModel, uniform=TRUE,
         main="Classification Tree")
    text(smotefit.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
    
    
    pred.tree <- predict(smotefit.tree, data.agg.np[!train, ], type = "prob")
    predVal.tree <- predict(smotefit.tree, newdata = data.agg.np[!train,])
    
    tree.conf <- confusionMatrix(predVal.tree, as.factor(data.agg.np[!train,]$Fraud))
    tree.rocplot.inter <- plot(roc(data.agg.np[!train, FraudIndx], pred.tree[,2]))
    
  # boosted tree
    trCtrl <- trainControl(## 10-fold CV
      method = "repeatedcv",
      number = 10,
      repeats = 10)
    smotefit.boosttree <- train(Fraud ~ ., 
                           data=smote_train_tree, 
                           method="gbm", 
                           trControl = trCtrl, 
                           verbose = FALSE)
    
    
    
    plot(smotefit.boosttree)
    smotefit.boosttree$bestTune
    plot(smotefit.boosttree$finalModel, uniform=TRUE,
         main="Classification Tree")
    
    pred.boosttree <- predict(smotefit.boosttree, data.agg.np[!train, ], type = "prob")
    boosttree.rocplot.inter <- plot(roc(data.agg.np[!train, FraudIndx], pred.boosttree[,2]))
    
    predVal.boosttree <- predict(smotefit.boosttree, newdata = data.agg.np[!train,])
    boost.conf <- confusionMatrix(predVal.boosttree, as.factor(data.agg.np[!train,]$Fraud))
    
    
    sum.boost <-summary(smotefit.boosttree, cBars = 5)
    barplot(sum.boost$rel.inf[1:5], names.arg = sum.boost$var[1:5], cex.names = 0.7, main = "Most important boost variables", col = c(1:5))
    
 # random forest
    trCtrl <- trainControl(method='repeatedcv', 
                           number=10, 
                           repeats=3)
    #Metric compare model is Accuracy
    metric <- "Accuracy"
    #Number randomely variable selected is mtry
    mtry <- sqrt(ncol(data.agg.np))
    tunegrid <- expand.grid(.mtry=mtry)
    smotefit.forest <- train(Fraud~., 
                        data=smote_train_tree, 
                        method='rf', 
                        metric='Accuracy', 
                        tuneGrid=tunegrid, 
                        trControl=trCtrl)
    
    
    pred.forest <- predict(smotefit.forest, data.agg.np[!train, ], type = "prob")
    
    predVal.ranForest <- predict(smotefit.forest, newdata = data.agg.np[!train,])
    RF.conf <- confusionMatrix(predVal.ranForest, as.factor(data.agg.np[!train,]$Fraud))
    
    
    roc.forest.inter <- plot(roc(data.agg.np[!train, FraudIndx], pred.forest[,2]), main = "ROC: Tree model and extensions")
    lines(roc(response = data.agg$Fraud[!train], predictor = pred.boosttree[,2]), col = "Blue") # think i need to pick my lamda better
    lines(roc(data.agg.np[!train, FraudIndx], pred.tree[,2]), col = "Green")
    legend(0.6, 0.4, legend = c("Tree", "Boosted Tree", "Random Forest"), col = c("Green", "Blue", "black"), lty = 1, lwd = 2)
    
    par(mar = c(10, 4, 4, 1))
    barplot(smotefit.forest$finalModel$importance[,1], las = 3, main = "Variable Importance in Random Forest", cex.names = 0.8, ylab = "Mean Decrease Gini")
    
    plot(roc.forest.inter, main = "RF ROC: SMOTE vs Original")
    lines(roc.forest, col = "red")
    legend(0.4, 0.4, legend = c("SMOTE", "Original"), col = c("black", "red"), lty = 1)
    
    
# svm with interaction terms & smote
    # note svm gets worse
    train_control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE)
    smotefit.svm.lin.inter <- train(Fraud ~., data = smote_train, method = "svmLinear", 
                                    tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                               trControl = train_control,  preProcess = c("center","scale"))
    pred.svm.lin.inter <- predict(smotefit.svm.lin.inter, newdata = data.agg.inter[!train,], type = "prob")
    predVal.svm.lin <- predict(smotefit.svm.lin.inter, newdata = data.agg.inter[!train,])
    plot(smotefit.svm.lin.inter)
    
    roc.svm.lin.inter <- roc(response = data.agg$Fraud[!train], predictor = pred.svm.lin.inter[,1])
    plot(roc.svm.lin.inter)
    
    smotefit.svm.rad.inter <- train(Fraud ~., data = smote_train, method = "svmRadial", 
                               trControl = train_control,  tuneLength = 10,
                               preProcess = c("center","scale"))
    pred.svm.rad.inter <- predict(smotefit.svm.rad.inter, newdata = data.agg.inter[!train,], type = "prob")
    roc.svm.rad.inter <- roc(response = data.agg$Fraud[!train], predictor = pred.svm.rad.inter[,1])
    predVal.svm.rad <- predict(smotefit.svm.rad.inter, newdata = data.agg.inter[!train,])
    plot(roc.svm.rad.inter, main = "Support Vector Machines: Kernel effects")
    lines(roc.svm.lin.inter, col = "red")
    legend(0.4, 0.4, legend = c("Radial", "Linear"), col = c("Black", "red"), lty = 1)
    
    predVal.smotesvm <- predict(smotefit.svm.lin.inter, newdata = data.agg.inter[!train,])
    
    svm.lin.conf <- confusionMatrix(predVal.svm.lin, as.factor(data.agg.np[!train,]$Fraud))
    svm.rad.conf <- confusionMatrix(predVal.svm.rad, as.factor(data.agg.np[!train,]$Fraud))
    
    svm.rocplot.inter <- roc.svm.lin.inter # linear kernel is better
    
    plot(smotefit.svm.lin.inter, main = "CV accuracy for Cost parameter")
    
    
    # knn with interaction terms
    # data.agg.inter$Fraud <- as.factor(data.agg.inter$Fraud)
    # levels(data.agg.inter$Fraud)[1] <- "No"
    # levels(data.agg.inter$Fraud)[2] <- "Yes"
    ctrl <- trainControl(method="repeatedcv",repeats = 3,
                         classProbs=TRUE,
                         summaryFunction = twoClassSummary)
    knnFit.inter <- train(Fraud ~ ., data = smote_train, 
                          method = "knn", 
                          trControl = ctrl, 
                          preProcess = c("center","scale"), 
                          tuneLength = 20)
    
    #Output of kNN fit
    knnFit.inter
    plot(knnFit.inter, type = "S", main = "KNN ROC across number of neighbours")
    pred.knn.inter <- predict(knnFit.inter, newdata = data.agg.inter[!train,], type = "prob")[,2]
    predVal.knn.inter <- predict(knnFit.inter, newdata = data.agg.inter[!train,])
    
    KNN.conf <- confusionMatrix(predVal.knn.inter, as.factor(data.agg.inter[!train,]$Fraud))
    KNN.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.knn.inter), main = "KNN ROC")
    
    
    
    # LDA and QDA
     # without pca
    control <- trainControl(method="repeatedcv", number=3, repeats = 3, verbose = TRUE, search = "grid")
    tuneLength <- 10
    metric <- "Accuracy"
    
    fitlda <- train(Fraud~., data=smote_train, method="lda", metric=metric, 
          preProc=c("center", "scale"), trControl=control, tuneLength = tuneLength)
    
    pred.lda <- predict(fitlda, newdata = data.agg.inter[!train,], type = "prob")
    predVal.lda <- predict(fitlda, newdata = data.agg.inter[!train,])
    
    lda.conf <- confusionMatrix(predVal.lda, as.factor(data.agg.inter[!train,]$Fraud))
    lda.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.lda[,1]), main = "LDA ROC")
    ldamod <- fitlda$finalModel
    
    largediff <- (ldamod$means[1,] - ldamod$means[2,])
    largediff[abs(largediff) > 0.5] # those with have the largest effect on the mean
    
    plot(ldamod$means[1, abs(largediff) > 0.5], type = "p", ylim = c(min(largediff), max(largediff)), lwd = 5,
         xlab = "Predictors", ylab = "Contribution to Fraud probability",
         xaxt = "n",
         main = "Fraudulent prediction changes")
    points(ldamod$means[2, abs(largediff) > 0.5], col = "red", lwd = 5) 
    axis(1, at = 1:length(names(largediff[abs(largediff) > 0.5])), labels = names(largediff[abs(largediff) > 0.5]), cex.axis = 0.6)
    legend(5, 0.1, legend = c("No", "Yes"), col = c("Black", "Red"), pch = 1)
    
# lda (with pca)
    control <- trainControl(method="repeatedcv", number=3, repeats = 3, verbose = TRUE, search = "grid")
    tuneLength <- 10
    metric <- "Accuracy"
    
    # remove zero variance column race
    
    fitlda <- train(Fraud~., data=smote_train[,-41], method="lda", metric=metric, 
                    preProc=c("pca"), trControl=control, tuneLength = tuneLength)
    
    pred.lda <- predict(fitlda, newdata = data.agg.inter[!train,], type = "prob")
    predVal.lda <- predict(fitlda, newdata = data.agg.inter[!train,])
    
    ldapca.conf <- confusionMatrix(predVal.lda, as.factor(data.agg.inter[!train,]$Fraud))
    ldapca.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.lda[,1]), main = "LDA ROC")
    ldamod <- fitlda$finalModel
    
    largediff <- (ldamod$means[1,] - ldamod$means[2,])
    largediff[abs(largediff) > 0.5] # those with have the largest effect on the mean
    
    plot(ldamod$means[1, abs(largediff) > 0.5], type = "p", ylim = c(min(largediff), max(largediff)), lwd = 5,
         xlab = "Predictors", ylab = "Contribution to Fraud probability",
         xaxt = "n",
         main = "Fraudulent prediction changes")
    points(ldamod$means[2, abs(largediff) > 0.5], col = "red", lwd = 5) 
    axis(1, at = 1:length(names(largediff[abs(largediff) > 0.5])), labels = names(largediff[abs(largediff) > 0.5]), cex.axis = 0.6)
    legend(5, 0.1, legend = c("No", "Yes"), col = c("Black", "Red"), pch = 1)
    
    
# qda (only with pca)
    fitqda <- train(as.formula(paste("Fraud ~", paste(names(coef(backwards)[-c(1, 14:16)]), collapse = " + "))),
                               data=smote_train, method="qda", metric=metric, 
                    preProc=c("pca"), trControl=control, tuneLength = tuneLength)
    
    pred.qda <- predict(fitqda, newdata = data.agg.inter[!train,], type = "prob")
    predVal.qda <- predict(fitqda, newdata = data.agg.inter[!train,])
    
    qda.conf <- confusionMatrix(predVal.qda, as.factor(data.agg.inter[!train,]$Fraud))
    qda.rocplot.inter <- plot(roc(response = data.agg$Fraud[!train], predictor = pred.qda[,1]), main = "Discriminate Analysis Comparisons")
    lines(lda.rocplot.inter, col = "red")
    lines(ldapca.rocplot.inter, col = "blue")
    legend(0.4, 0.4, legend = c("QDA", "LDA", "LDA:PCA"), col = c("Black", "red", "Blue"), lty = 1)
    
    
    plotrocplotbest <- function() {
      plot(log.rocplot.inter, col = 1, main = "ROC: Best plots")
      lines(roc.forest.inter, col = 2)
      lines(KNN.rocplot.inter, col = 3)
      lines(lda.rocplot.inter, col = 4)
      lines(svm.rocplot.inter, col = 5)
      legend(0.4, 0.6, legend = c("Log", "RF: Linear", "KNN", "LDA", "SVM"), col = 1:5, cex = 0.6, lty = 1)
    }
    
    
    
    plotlogrocinter <- function() {
      plot(log.rocplot.inter, col = 1, main = "With Interaction term ROC curve")
      lines(ldapca.rocplot.inter, col = 2)
      lines(lasso.rocplot.inter, col = 3)
      lines(ridge.rocplot.inter, col = 4)
      lines(KNN.rocplot.inter, col = 5)
      legend(0.2, 0.6, legend = c("Log", "LDA", "Lasso", "Ridge", "KNN"), col = 1:5, cex = 0.6, lty = 1)
    }
    
    interrocplots <- function() {
      plot(log.rocplot.inter, col = 1, main = "Interaction term ROC curve")
      lines(lda.rocplot.inter, col = 2)
      # lines(qda.rocplot.inter, col = 3)
      lines(lasso.rocplot.inter, col = 4)
      lines(ridge.rocplot.inter, col = 5)
      lines(KNN.rocplot.inter, col = 6)
      lines(svm.rocplot.inter, col = 7)
      lines(roc.forest, col = 8)
      lines(plsda.roc, col = 9)
      # lines(pcr.rocplot, col = 4)
      # lines(pls.rocplot, col = 5)
      legend(0.2, 0.6, legend = c("Log", "LDA", "QDA", "Lasso", "Ridge", "KNN", "SVM", "Forest", "plsda"), col = 1:9, cex = 0.6, lty = 1)
    }
    
    interlogrocplots <- function() {
      plot(log.rocplot.inter, col = 1, main = "With Interaction term ROC curve")
      lines(lda.rocplot.inter, col = 2)
      lines(qda.rocplot.inter, col = 3)
      lines(lasso.rocplot.inter, col = 4)
      lines(ridge.rocplot.inter, col = 5)
      lines(KNN.rocplot.inter, col = 6)
      legend(0.2, 0.6, legend = c("Log", "LDA", "QDA", "Lasso", "Ridge", "KNN"), col = 1:6, cex = 0.6, lty = 1)
    }
    
# # forest
#     mtry <- sqrt(ncol(data.agg.np))
#     tunegrid <- expand.grid(.mtry=mtry)
#     smote_out_forest <- train(Fraud~., 
#                               data=smote_train, 
#                               method='rf', 
#                               metric='Accuracy', 
#                               tuneGrid=tunegrid, 
#                               trControl=ctrl)
#     
#     
# # KNN
#     smote_out_knn <- train(Fraud ~ ., data = smote_train, 
#                            method = "knn", trControl = ctrl, 
#                            preProcess = c("center","scale"), 
#                            tuneLength = 20)
    
    
    # https://shiring.github.io/machine_learning/2017/04/02/unbalanced 
    
#################################################################################################
#################################################################################################
#################################################################################################
# Classifying new data    

    
    
    
    
    
    provider_eval <- read.csv("Medicare_Provider_Eval_PartB.csv", stringsAsFactors = FALSE)
    eval <- read.csv("Medicare_Outpatient_Inpatient_Beneficiary_Eval_PartB.csv", stringsAsFactors = FALSE)
    
  
    eval <- left_join(eval, fraudStatus)
    
    # Cleaning eval
    
    # note: these columns are all NA - remove them all
    length(eval$ClmProcedureCode_6)
    sum(is.na(eval$ClmProcedureCode_6))
    length(eval$ClmProcedureCode_5)
    sum(is.na(eval$ClmProcedureCode_5))
    
    eval <- eval[,colSums(is.na(eval))<nrow(eval)] # removes full na columns
    
    
    # changing those with dates to date type
    date_vars = c(
      "ClaimStartDt",
      "ClaimEndDt",
      "AdmissionDt",
      "DischargeDt",
      "DOB",
      "DOD"
    )
    for (var in date_vars) {
      eval[,var] =  as.Date(eval[,var])
    }
    
    # changing ClmProcedureCodes, race, state and county to character variables
    # this avoids averaging them in later analysis
    char_vars = c(
      as.character(vars_select(colnames(eval), starts_with('Clm'))),
      "Race",
      "State",
      "County"
    )
    for (var in char_vars) {
      eval[,var] = as.character(eval[,var])
    }
    
    # Changing fraud from "yes"/"no" to 1/0
    # can easily count
    # Checking no NA's in Fraud column
    eval$RenalDiseaseIndicator <- ifelse(eval$RenalDiseaseIndicator=="Y", 1, 0)
    
    # Changing claimType from "yes"/"no" to 1/0
    # can easily count
    # Checking no NA's in claimType column
    if(sum(is.na(eval$ClaimType)) == 0) {
      print("Claimtype entries complete")
    }
    eval$ClaimType <- ifelse(eval$ClaimType=="Inpatient", 1, 0)
    # rename column to inpatient
    names(eval)[names(eval) == "ClaimType"] <- "Inpatient"
    
    
    
    # Creating new variables for modelling
    # num chronic conditions
    eval <- eval %>%
      mutate("ChronicCond_MixedNum" = 22 - rowSums(.[38:48])) # Hardcoded values for Chronic diseases
    eval$ChronicCond_MixedNum <- as.integer(eval$ChronicCond_MixedNum)
    
    # if code changes
    eval <- eval %>%
      mutate("changedCode" = (ifelse(ClmAdmitDiagnosisCode == ClmDiagnosisCode_1, 0, 1)))
    eval$changedCode <- as.integer(eval$changedCode)
    
    # Adding in the claim length in days
    eval <- eval %>%
      mutate(ClaimLength = ClaimEndDt - ClaimStartDt)
    eval$ClaimLength <- as.numeric(eval$ClaimLength)
    
    
    # sanity check on variables
    variables = colnames(eval)
    for (var in variables) {
      print(var)
      print(summary(na.omit(eval[,var])))
      
    }
    
    # calculating age (in years) at start of claim
    # ASSUMPTION MADE - since there are many providers with no claim start dt
    # the minimum start date was chosen of all claims
    # Its assumed that a year difference doesnt have a significant effect
    eval$Age <- as.numeric((eval$ClaimStartDt - eval$DOB)/365.25)
    
    # Creates categorical variables for all variables.
    # If they contain a NA value, it is set to the baseline value of a regression (e.g. usually 0)
    # and the NA categorical value is set to 1
    
    eval <- eval %>% mutate(hos_stay_len =  as.numeric(DischargeDt - AdmissionDt)) %>%
      mutate(AttendingPhysicianNA = is.na(AttendingPhysician)) %>%
      mutate(OperatingPhysicianNA = is.na(OperatingPhysician)) %>%
      mutate(OperatingPhysicianNA = is.na(OperatingPhysician)) %>%
      mutate(ClmDiagnosisCodeNA = is.na(ClmDiagnosisCode_1)) %>%
      mutate(DODNA = !is.na(DOD)) %>%
      mutate(changedCodeNA = is.na(changedCode) & Inpatient == 0) %>%
      mutate(ClmAdmitDiagnosisCodeNA = is.na(ClmAdmitDiagnosisCode) &  Inpatient == 0)%>%
      mutate(ClmProcedureCodeNA = is.na(ClmProcedureCode_1) & Inpatient == 1)
    
    
    
    # INPATIENTS MAY NOT HAVE A START DATE - CHECK THE LENGTH
    for(i in 1:length(eval)) {
      if(any(is.na(eval[,i]))) { # if there is an na value in column
        # indicatorVarName <- paste0(colnames(eval)[i], "NA")
        #
        # eval[[indicatorVarName]] <- as.integer(is.na(eval[,i]))
        #
        # replaces an na value in a numeric column with the minimum value
        if(class(eval[, i]) != "character") { # checks to see if character first -> doesnt make sense to take a minimum of a char
          eval[,i][is.na(eval[, i])] <- mean(na.omit(eval[,i]))
          
          
          # better way to impute these. Create a linear regression model to approximate the value
          # use caret preProcess
        }
      }
    }
    

    # Checks if physician is fraudulent
      eval$FraudPhys <- match(as.character(eval$AttendingPhysician), FraudyPhys)
      eval$FraudPhys[is.na(eval$FraudPhys)] <- 0
      
      # Do the same as above for county and state
      
      eval$FraudCounty <- match(as.character(eval$County), FraudyCounty)
      eval$FraudCounty[is.na(eval$FraudCounty)] <- 0
      
      eval$FraudState <- match(as.character(eval$State), FraudyState)
      eval$FraudState[is.na(eval$FraudState)] <- 0
    
    # Finds average claims per beneficiary for each provider
    # introduce counties, race and state here
    eval.bene <- eval %>%
      group_by(BeneID, ProviderID, Race) %>%
      summarise("NumClaims" = n())
    eval.bene <- eval.bene %>%
      group_by(ProviderID) %>%
      summarise("AvgClaimsPerBene" = mean(NumClaims), "ModeRace" = mlv(Race, method = 'mfv', na.rm = TRUE)[1])
    eval.bene$ModeRace <- as.factor(eval.bene$ModeRace)
    
    # Aggregating data per provider
    # Lose a lot of variables in this step (e.g. physicians etc). Need to think of a way to retain them
    # Bootstrap thoughts
    # Could potentially think of doing bootstrap to get std error of the averages
    # would also give p-values on how reliable each average is
    eval.agg <- aggregate(eval, by = list(eval$ProviderID), FUN = mean) # Takes the mean of all the other columns for each provider
    eval.agg <- subset(eval.agg, select = -c(ProviderID))
    names(eval.agg)[names(eval.agg) == "Group.1"] <- "ProviderID"
    eval.agg$ProviderID <- as.character(eval.agg$ProviderID) # changing provider ID to character variable again
    eval.agg2 <- aggregate(eval, by = list(eval$ProviderID), FUN = length) # Takes the number of claims per provider id
    eval.agg <- eval.agg %>% mutate(num_clms = eval.agg2$BeneID) # mixes the two evalsets
    eval.agg$num_clms <- as.double(eval.agg$num_clms) # Changes to double for later loop
    eval.agg <- left_join(eval.agg, eval.bene) # adds average claims per benficiary
    
    # Removes full NA columns
    eval.agg <- eval.agg[,colSums(is.na(eval.agg))<nrow(eval.agg)]
    # changes Fraud class to integer
    eval.agg.np$Fraud <- as.integer(eval.agg.np$Fraud)
    
    eval.agg.np <- eval.agg[,-1] # no providers
    eval.agg.np <- eval.agg.np %>% # changes date to days since 2008-11-27 (GIVES SOME WEIRD VALUES)
      mutate("StartDtdays_from_271108" = as.numeric(ClaimStartDt - as.Date("2008-11-27"))) %>%
      mutate("EndDtdays_from_271108" = as.numeric(ClaimEndDt - as.Date("2008-11-27")))
    eval.agg.np <- eval.agg.np[,-which(sapply(eval.agg.np, class) == "Date")]
    
    # removes Claim start date, claim end date and some other variables deemed inappropriate for aggregated eval
    # eval.agg <- eval.agg %>%
    # dplyr::select(-c(ClaimStartDt, ClaimEndDt, AdmissionDt, DischargeDt, DOD, changedCode, ClaimLength, DOB))
    # 
    # eval.agg.np <- eval.agg[,-1] # no providers
    
    # Chosen model here is forest
    eval_prob <- predict(smotefit.forest, eval.agg.np, type = "prob")[,2]
    
    eval.agg$FraudProb <- eval_prob
    eval.agg <- eval.agg[order(-eval.agg$FraudProb),]
    top350 <- eval.agg[1:350,]
    top350 <- top350[,1]
    
    write.csv(top350,"C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/top350/top350.csv", col.names = FALSE)
    
# POST EXAM CHECKING NUMBER CORRECT
    mine <- read.csv("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/top350/top350.csv")
    result <- read.csv("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/top350/Fraudulent_providers.csv")

    length(intersect(result$ï..ID..Yes., mine$ProviderID))/length(result$Fraud..Yes.)   
    
    