# ACTL3142 Assignment

# packages
library(dplyr)
library(tidyverse)
library(EnvStats) # For epdfPlot
library(lubridate) # For "year" function
library(ggplot2) # For plots
library(scales) # For changing axis on plots
library(ggpubr) # for arranging ggplot2
library(Hmisc) # for reducing correlation matrix

# DATA IS FROM 2008 to 2009

setwd("C:/Users/Charlie/Documents/University/year3/T2/ACTL3142/Assignment/Data")

beneficiary <- read.csv("Medicare_Beneficiary.csv", stringsAsFactors = FALSE)
inpatient <- read.csv("Medicare_Inpatient.csv", stringsAsFactors = FALSE)
outpatient <- read.csv("Medicare_Outpatient.csv", stringsAsFactors = FALSE)
provider <- read.csv("Medicare_Provider.csv", stringsAsFactors = FALSE)

# file.choose()

inpatient <- inpatient %>%
  mutate(Inpatient = 1)

# Left join
data <- full_join(inpatient, outpatient)
data <- left_join(data, beneficiary, by = "BeneID")

# change all na's to 0 for outpatients
data$Inpatient[is.na(data$Inpatient)] <- 0
data$Inpatient <- as.integer(data$Inpatient)

data <- left_join(data, provider, by = "ProviderID") # when a providerID isnt in the other data it 

data <- data %>%
  mutate("changedCode" = (ifelse(ClmAdmitDiagnosisCode == ClmDiagnosisCode_1, 0, 1)))
data$changedCode <- as.integer(data$changedCode)

data <- data %>%
  mutate("ChronicCond_MixedNum" = 22 - rowSums(.[27:37])) # Hardcoded values for Chronic diseases
data$ChronicCond_MixedNum <- as.integer(data$ChronicCond_MixedNum)

# Average number of the number of mixed chronic conditions 
# for Fraudulent and non fraudulent patients
data %>%
  group_by(Fraud) %>%
  summarise(mean(ChronicCond_MixedNum))

    # CHECKING ALL RANGES
range(na.omit(as.Date(data$DOB)))

max_age <- data %>%
  filter(DOB == "1909-01-01")
length(max_age$BeneID)
# There is 240 people with the maximum age

# TRYING TO CHECK THE RANGE OF ALL COLUMNS
for (i in ncol(data) ) {
  range(na.omit(as.numeric((data[,i]))))
}

# changing those with dates to date type
date_vars = c(
  "ClaimStartDt",
  "ClaimEndDt",
  "AdmissionDt",
  "DischargeDt",
  "DOB",
  "DOD")


for (var in date_vars) {
  data[,var] =  as.Date(data[,var])
}
##

# loops through summaries
variables = colnames(data)
variables


for (var in variables) {
  print(var)
  print(summary(na.omit(data[,var])))
  
}
#

data <- data %>%
  mutate(age = as.numeric( (data$ClaimStartDt - data$DOB) / 365.25) )

# Changing Yes/No of Fraud to 1 or 0
data$Fraud[data$Fraud == "Yes"] <- 1
data$Fraud[data$Fraud == "No"] <- 0
data$Fraud <- as.integer(data$Fraud)

data.in <- data %>%
  filter(data$Inpatient == 1)
 
data.out <- data %>%
  filter(data$Inpatient == 0)


# CHECKING IN RANGES
max_age_in <- data.in %>%
  filter(DOB == "1909-01-01")
length(max_age_in$BeneID)
# There is 240 people with the maximum age

# CHECKING OUT RANGES
max_age_out <- data.out %>%
  filter(DOB == "1909-01-01")
length(max_age_out$BeneID)
# There is 240 people with the maximum age

# Claim date - DOB = AGE

# Does age play a role in which claims are fraudulent?



# Deletes fully NA Columns
data.out <- data.out[,colSums(is.na(data.out))<nrow(data.out)]
data.in <- data.in[,colSums(is.na(data.in))<nrow(data.in)]


## make bins and find avg % Fraud
## Note: People are all born on the first day of every month
data.age <- data %>%
  group_by(group = cut(age, breaks = c(-Inf, seq(0, max(age), 5), Inf))) %>%
  summarise('Fraud %' = mean(Fraud), "Count" = n())

plot(data.age$group, data.age$`Fraud %`)

# plotting the probability distribution of ages (Uses epdfPlot which is from EnvStats)
epdfPlot(data$age, discrete = TRUE)

# epdfPlot of those who commit fraud based on age
data.fraud <- data %>%
  filter(Fraud == 1)
epdfPlot(data.fraud$age, discrete = TRUE)



summary(na.omit(data[,"DOB"]))

DOB_1909 <- data %>% filter(DOB == "1909-01-01")

mean(DOB_1909$Fraud)

pbinom(length(DOB_1909[which(DOB_1909$Fraud == 1),"Fraud"]),nrow(DOB_1909), FraudRate)

hist(data$DOB)

plot(demp(data$DOB))



# full pdf
epdfPlot(year(data$DOB), discrete=TRUE, xaxt="n")
axis(1, at = seq(1905, 1985, by = 5), las=2)

# not fraud
epdfPlot(year(data[which(data$Fraud == 0),"DOB"]), discrete=TRUE, xaxt="n")
axis(1, at = seq(1905, 1985, by = 5), las=2)

# fraud
epdfPlot(year(data[which(data$Fraud == 1),"DOB"]), discrete=TRUE, xaxt="n")
axis(1, at = seq(1905, 1985, by = 5), las=2)

epdfPlot(yday(data[which(year(data$DOB) == 1909 ),'DOB']), discrete=TRUE, xaxt="n")
axis(1, at = seq(0, 365, by = 5), las=2)


###################


all_var = colnames(data)
#E PDF plots
for (var in all_var) {
  print(var)
  if(typeof(data[,var]) == "character") { # skips character variables
    
  } else if (typeof(data[,var]) == "integer") {
    # full pdf
    png(filename = paste0("Empirical DF of ", var, ".png"))
    epdfPlot(data[,var], discrete=TRUE, main = paste0("Empirical DF of ", var), xlab = var)
    dev.off()
    # not fraudulent
    png(filename = paste0("Empirical DF of ", var, ", not fraud.png"))
    epdfPlot(data[which(data$Fraud == 0),var], discrete=TRUE, main = paste0("Empirical DF of ", var, ", not fraud"), xlab = var, epdf.col = "red")
    dev.off()
    # fraudulent
    png(filename = paste0("Empirical DF of ", var, ", fraud.png"))
    epdfPlot(data[which(data$Fraud == 1),var], discrete=TRUE, main = paste0("Empirical DF of ", var, ", fraud"), xlab = var, epdf.col = "blue")
    dev.off()
    
  } else if (typeof(data[,var]) == "double") {
    # full pdf
    png(filename = paste0("Empirical PDF of ", var, ".png"))
    epdfPlot(as.numeric(na.omit(data[,var])), main = paste0("Empirical PDF of ", var), xlab = var)
    dev.off()
    # not fraud
    png(filename = paste0("Empirical PDF of ", var, ", not fraud.png"))
    epdfPlot(as.numeric(na.omit(data[which(data$Fraud == 0),var])), main = paste0("Empirical PDF of ", var, ", not fraud"), xlab = var, epdf.col = "red")
    dev.off()
    # fraudulent
    png(filename = paste0("Empirical PDF of ", var, ", fraud.png"))
    epdfPlot(as.numeric(na.omit(data[which(data$Fraud == 1),var])), main = paste0("Empirical PDF of ", var, ", fraud"), xlab = var, epdf.col = "blue")
    dev.off()
  }
  
}

###################

# Shows distribution of Fraud across each month
data.claimMonth <- data %>%
  mutate("month" = format(ClaimStartDt, "%m")) %>%
  group_by(as.integer(month)) %>%
  summarise("Fraud %" = mean(Fraud), "Count" = n(), "Spending" = sum(na.omit(DeductibleAmtPaid)))
ggplot(data = data.claimMonth, aes(x = `as.integer(month)`, y = `Fraud %`)) + 
  geom_col(width = 0.2) +
  coord_cartesian(ylim = c(0.325, 0.375)) +
  scale_x_continuous(breaks = c(1:12)) +
  xlab("Month of Claim")
 

# plot the 
# total cost = deductible I think
fraud.data <- data %>%
  filter(Fraud == 1) %>%
  mutate("month" = format(ClaimStartDt, "%m")) %>%
  group_by(as.integer(month)) %>%
  summarise("Spending" = sum(na.omit(DeductibleAmtPaid)), "Count" = n(), "fraud" = 1)
ggplot(data = fraud.data, aes(x = `as.integer(month)`, y = `Spending`)) + 
  geom_col(width = 0.2) +
  scale_x_continuous(breaks = c(1:12))

notfraud.data <- data %>%
  filter(Fraud == 0) %>%
  mutate("month" = format(ClaimStartDt, "%m")) %>%
  group_by(as.integer(month)) %>%
  summarise("Spending" = sum(na.omit(DeductibleAmtPaid)), "Count" = n(), "fraud" = 0)
ggplot(data = notfraud.data, aes(x = `as.integer(month)`, y = Spending)) + 
  geom_col(width = 0.2) +
  scale_x_continuous(breaks = c(1:12)) + 
  scale_y_continuous(labels = comma) # using scales package

# plot two together
monSpending.data <- full_join(fraud.data, notfraud.data)
monSpending.data$fraud <- as.integer(monSpending.data$fraud)
ggplot(data = monSpending.data, aes(x = `as.integer(month)`, y = Spending, group = factor(fraud), fill = factor(fraud))) +
  geom_col(width = 0.75, position = position_dodge()) +
  scale_x_continuous(breaks = c(1:12))



monSpending.data <- monSpending.data %>%
  mutate(perPatient = Spending/Count)
ggplot(data = monSpending.data, aes(x = `as.integer(month)`, y = perPatient, group = factor(fraud), fill = factor(fraud))) +
  geom_col(width = 0.75, position = position_dodge()) +
  scale_x_continuous(breaks = c(1:12)) +
  xlab("Month at Start of Claim") +
  ylab("Deductible per Patient") +
  guides(fill=guide_legend(title="Fraud"))



# Adding in the claim duration in days
data <- data %>%
  mutate(ClaimDuration = ClaimEndDt - ClaimStartDt)

data$ClaimDuration <- as.numeric(data$ClaimDuration)

# % of fraud claims based on duration
data.claimDur <- data %>%
  group_by("Claim Duration (days)" = cut(ClaimDuration, breaks = c(0, 1, seq(5, max(ClaimDuration), 5), Inf), right = FALSE)) %>%
  summarise("Fraud %" = mean(Fraud), "Count" = n())
plot(data.claimDur$`Claim Duration (days)`, data.claimDur$`Fraud %`)

# number of fraud claims based on duration
fraud.data.claimDur <- data %>%
  filter(Fraud == 1) %>%
  group_by("Claim Duration (days)" = cut(ClaimDuration, breaks = c(0, 1, seq(5, max(ClaimDuration), 5), Inf), right = FALSE)) %>%
  summarise("Fraud" = mean(Fraud), "Count" = n())
notfraud.data.claimDur <- data %>%
  filter(Fraud == 0) %>%
  group_by("Claim Duration (days)" = cut(ClaimDuration, breaks = c(0, 1, seq(5, max(ClaimDuration), 5), Inf), right = FALSE)) %>%
  summarise("Fraud" = mean(Fraud), "Count" = n())
all.data.claimDur <- full_join(fraud.data.claimDur, notfraud.data.claimDur)
ggplot(data = all.data.claimDur, aes(x = `Claim Duration (days)`, y = Count, group = factor(Fraud), fill = factor(Fraud))) +
  geom_col(width = 0.75, position = position_dodge())

# Based on wether the physician is operating
# This can either remove physician blame or highlight their fraudulent behaviour
fraud.data.phys <- data %>%
  filter(Fraud == 1) %>%
  group_by(AttendingPhysician) %>%
  summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
  mutate(rank = rank(-Count, ties.method = 'first')) %>%
  filter(rank <= 10) 
fraud.data.phys$AttendingPhysician <- gsub("[a-zA-Z]", "", fraud.data.phys$AttendingPhysician) # removes all character
fraud.data.phys <- fraud.data.phys[order(fraud.data.phys$rank),] # ordering it as ranked
fraud.data.phys$AttendingPhysician <- factor(fraud.data.phys$AttendingPhysician, fraud.data.phys$AttendingPhysician) # adding in as factors
# ^ Used to plot in order

notfraud.data.phys <- data %>%
  filter(Fraud == 0) %>%
  group_by(AttendingPhysician) %>%
  summarise("Count" = n(), "Fraud" = mean(Fraud)) %>%
  mutate(rank = rank(-Count, ties.method = 'first')) %>%
  filter(rank <= 10) 
notfraud.data.phys$AttendingPhysician <- gsub("[a-zA-Z]", "", notfraud.data.phys$AttendingPhysician) # removes all character
notfraud.data.phys <- notfraud.data.phys[order(notfraud.data.phys$rank),] # ordering it as ranked
notfraud.data.phys$AttendingPhysician <- factor(notfraud.data.phys$AttendingPhysician, notfraud.data.phys$AttendingPhysician) # adding in as factors

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



# Could do depending on the Codes
fraud.data.AdmitCode <- data %>%
  filter(Fraud == 1) %>%
  group_by(ClmAdmitDiagnosisCode) %>%
  summarise("Count" = n(), "Fraud" = mean(Fraud))
fraud.data.AdmitCode <- na.omit(fraud.data.AdmitCode) %>% # removing NA's Note: vast majority were NA's
  mutate(rank = rank(-Count, ties.method = 'first')) %>%
  filter(rank <= 10) 
fraud.data.AdmitCode$ClmAdmitDiagnosisCode <- gsub("[a-zA-Z]", "", fraud.data.AdmitCode$ClmAdmitDiagnosisCode) # removes all character
fraud.data.AdmitCode <- fraud.data.AdmitCode[order(fraud.data.AdmitCode$rank),] # ordering it as ranked
fraud.data.AdmitCode$ClmAdmitDiagnosisCode <- factor(fraud.data.AdmitCode$ClmAdmitDiagnosisCode, fraud.data.AdmitCode$ClmAdmitDiagnosisCode) # adding in as factors
# ^ Used to plot in order

notfraud.data.AdmitCode <- data %>%
  filter(Fraud == 0) %>%
  group_by(ClmAdmitDiagnosisCode) %>%
  summarise("Count" = n(), "Fraud" = mean(Fraud)) 
notfraud.data.AdmitCode <- na.omit(notfraud.data.AdmitCode) %>% # removing NA's Note: vast majority were NA's
  mutate(rank = rank(-Count, ties.method = 'first')) %>%
  filter(rank <= 10) 
notfraud.data.AdmitCode$ClmAdmitDiagnosisCode <- gsub("[a-zA-Z]", "", notfraud.data.AdmitCode$ClmAdmitDiagnosisCode) # removes all character
notfraud.data.AdmitCode <- notfraud.data.AdmitCode[order(notfraud.data.AdmitCode$rank),] # ordering it as ranked
notfraud.data.AdmitCode$ClmAdmitDiagnosisCode <- factor(notfraud.data.AdmitCode$ClmAdmitDiagnosisCode, notfraud.data.AdmitCode$ClmAdmitDiagnosisCode) # adding in as factors
# ^ Used to plot in order

data.AdmitCode <- full_join(fraud.data.AdmitCode, notfraud.data.AdmitCode)
fraud.data.AdmitCode.plot <- ggplot(data = fraud.data.AdmitCode, aes(x = ClmAdmitDiagnosisCode, y = Count)) + 
  geom_col(fill = "blue") +
  ylab("") + 
  xlab("") +
  ggtitle("Fraud") +
  coord_cartesian(ylim = c(0, max(data.AdmitCode$Count)))
notfraud.data.AdmitCode.plot <- ggplot(data = notfraud.data.AdmitCode, aes(x = ClmAdmitDiagnosisCode, y = Count)) + 
  geom_col(fill = "red") + 
  ylab("") + 
  xlab("") +
  ggtitle("Not Fraud") +
  coord_cartesian(ylim = c(0, max(data.AdmitCode$Count)))
AdmitCode.plot <- ggarrange(fraud.data.AdmitCode.plot, notfraud.data.AdmitCode.plot)
annotate_figure(AdmitCode.plot,
                top = text_grob("Most common diagnosis codes"),
                left = "Count",
                bottom = "Admit Diagnosis Code"
)

fraud.data.chngeCode <- data %>%
  filter(Fraud == 1) %>%
  group_by(changedCode) %>%
  summarise("Count" = n())

notfraud.data.chngeCode <- data %>%
  filter(Fraud == 0) %>%
  group_by(changedCode) %>%
  summarise("Count" = n())

# ^ SHOULD GRAPH THIS

# aggregating by mean
data.agg <- aggregate(data, by = list(data$ProviderID), FUN = mean) # Takes the mean of all the other columns for each provider
data.agg2 <- aggregate(data, by = list(data$ProviderID), FUN = length) # Takes the number of claims per provider id
data.agg <- data.agg %>% mutate(num_clms = data.agg2$BeneID) # mixes the two datasets
data.agg$num_clms <- as.double(data.agg$num_clms) # Changes to double for later loop

# aggregating by variance
rm(var)
data.aggvar <- aggregate(data, by = list(data$ProviderID), FUN = var)
data.aggvar$Fraud <- data.agg$Fraud # adding the Fraud toggle variable back in

# Removes full NA columns
data.agg <- data.agg[,colSums(is.na(data.agg))<nrow(data.agg)]
data.aggvar <- data.aggvar[,colSums(is.na(data.aggvar))<nrow(data.aggvar)]

# Makes pairwise scatter plots of all the different variables

# check age vs chronic conditions
all.var <- colnames(data.agg)[-c(1:3, 5, 7, 10:11, 13:17, 19:28, 33, 37)]
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


# Gets the intersecting names of mean dataset and variance dataset
intersect.var <- intersect(colnames(data.aggvar),colnames(data.agg))

# Finds the data when divided by the count of provider
data.aggvar2 <- data.aggvar2
for(i in colnames(data.aggvar)[-1]) {
  data.aggvar2[, i] <- mapply('/', data.aggvar[,i], data.agg2$BeneID)
}


for (variance.var in intersect.var) {
  mean.var <- variance.var
  if(typeof(data.agg[,variance.var]) == "double") {
      if(typeof(data.aggvar[,variance.var]) == "double") {
        title <- paste0("Plot of ", variance.var)
        print(paste0("Plot of ", variance.var, ".png"))
        p1 <- ggplot(data = data.agg, aes(x = get(mean.var), y = data.aggvar[,variance.var], color = factor(Fraud))) +
          geom_point() +
          xlab("Mean") +
          ylab("Variance") +
          guides(col = guide_legend("Fraud")) +
          ggtitle(title)
        ggsave(paste0("Plot of ", variance.var, ".png"), plot = p1)
        while (!is.null(dev.list()))  dev.off()
    }
  }
}

# Correlation and covariance matrices
cov.data <- select_if(data, is.numeric) # selects only numeric inputs
cor.data <- cor(cov.data, use = "complete.obs")
cov.data <- cov(cov.data, use= "complete.obs") # calculates the covariance matrix

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


# Finds highly correlated variables
res2<-rcorr(as.matrix(cor.data)) # uses Hmisc library
high.cor <- flattenCorrMatrix(res2$r, res2$P) %>%
  filter(abs(cor) > 0.5)

all.var.agg = colnames(data.aggvar)

for (var in all.var.agg) {
  print(var)
  try(
    if(typeof(data.agg[,var]) == "character"){ # If data is character variable, skip it
      
    } else if(typeof(data.agg[,var]) == "integer") {
      # Full pdf
      png(filename = paste0("Empirical DF of ", var, ".png"))
      epdfPlot(data.agg[,var], discrete=TRUE, main = paste0("Empirical DF of ", var), xlab = var)
      dev.off()
      # not fraudulent
      png(filename = paste0("Empirical DF of ", var, ", not fraud.png"))
      epdfPlot(data.agg[which(data.agg$Fraud == 0),var], discrete=TRUE, main = paste0("Empirical DF of ", var, ", not fraud"), xlab = var, epdf.col = "red")
      dev.off()
      # fraudulent
      png(filename = paste0("Empirical DF of ", var, ", fraud.png"))
      epdfPlot(data.agg[which(data.agg$Fraud == 1),var], discrete=TRUE, main = paste0("Empirical DF of ", var, ", fraud"), xlab = var, epdf.col = "blue")
      dev.off()
      
    } else if(typeof(data.agg[,var]) == "double") {
      # Full pdf
      png(filename = paste0("Empirical PDF of ", var, ".png"))
      epdfPlot(na.omit(data.agg[,var]), main = paste0("Empirical PDF of ", var), xlab = var)
      dev.off()
      # not fraudulent
      png(filename = paste0("Empirical PDF of ", var, ", not fraud.png"))
      epdfPlot(na.omit(data.agg[which(data.agg$Fraud == 0),var]), main = paste0("Empirical PDF of ", var, ", not fraud"), xlab = var, epdf.col = "red")
      dev.off()
      # fraudulent
      png(filename = paste0("Empirical PDF of ", var, ", fraud.png"))
      epdfPlot(na.omit(data.agg[which(data.agg$Fraud == 1),var]), main = paste0("Empirical PDF of ", var, ", fraud"), xlab = var, epdf.col = "blue")
      dev.off()
    }
  )
}

# For aggregate data with variance
all.var.aggvar = colnames(data.aggvar)

for (var in all.var.aggvar) {
  print(var)
  try(
    if(typeof(data.aggvar[,var]) == "character"){ # If data is character variable, skip it
      
    } else if(typeof(data.aggvar[,var]) == "integer") {
      # Full pdf
      png(filename = paste0("Empirical DF of ", var, ".png"))
      epdfPlot(data.aggvar[,var], discrete=TRUE, main = paste0("Empirical DF of ", var), xlab = var)
      dev.off()
      # not fraudulent
      png(filename = paste0("Empirical DF of ", var, ", not fraud.png"))
      epdfPlot(data.aggvar[which(data.aggvar$Fraud == 0),var], discrete=TRUE, main = paste0("Empirical DF of ", var, ", not fraud"), xlab = var, epdf.col = "red")
      dev.off()
      # fraudulent
      png(filename = paste0("Empirical DF of ", var, ", fraud.png"))
      epdfPlot(data.aggvar[which(data.aggvar$Fraud == 1),var], discrete=TRUE, main = paste0("Empirical DF of ", var, ", fraud"), xlab = var, epdf.col = "blue")
      dev.off()
      
    } else if(typeof(data.aggvar[,var]) == "double") {
      # Full pdf
      png(filename = paste0("Empirical PDF of ", var, ".png"))
      epdfPlot(na.omit(data.aggvar[,var]), main = paste0("Empirical PDF of ", var), xlab = var)
      dev.off()
      # not fraudulent
      png(filename = paste0("Empirical PDF of ", var, ", not fraud.png"))
      epdfPlot(as.numeric(na.omit(data.aggvar[which(data.aggvar$Fraud == 0),var])), main = paste0("Empirical PDF of ", var, ", not fraud"), xlab = var, epdf.col = "red")
      dev.off()
      # fraudulent
      png(filename = paste0("Empirical PDF of ", var, ", fraud.png"))
      epdfPlot(as.numeric(na.omit(data.aggvar[which(data.aggvar$Fraud == 1),var])), main = paste0("Empirical PDF of ", var, ", fraud"), xlab = var, epdf.col = "blue")
      dev.off()
    }
  )
}


# Investigation of a single binary variable
#num clms < 10
epdfPlot(data.agg[(which(data.agg$num_clms < 10 & data.agg$num_clms > 5)),]$ChronicCond_Depression, discrete = TRUE)

mean(data$ChronicCond_Depression)# 1.564939 implies 44% do have depression

nrow(data.agg[(which(data.agg$num_clms == 10)),])
nrow(data.agg[(which(data.agg$num_clms == 10 & data.agg$ChronicCond_Depression <= 1)),])

epdfPlot(data.agg[(which(data.agg$num_clms == 10)),]$ChronicCond_Depression, discrete = TRUE, main = "DF of Depression with 10 claims", xlab = "Mean value of ChronicCond_Depression")

# MonthA
data.monA <- data %>%
  filter(NoOfMonths_PartACov == 0)
mean(data.monA$Fraud)

# MonthB
data.monB <- data %>% 
  filter(NoOfMonths_PartBCov == 0)
mean(data.monB$Fraud)
  
mean(data$Fraud)


# 0 deduction amount paid
data.deduc <- data %>% 
  filter(DeductibleAmtPaid == 0)
mean(data.deduc$Fraud)

# Insurance claim reimbursed = 0
data.reimburse <- data %>%
  filter(InscClaimAmtReimbursed == 0)
mean(data.reimburse$Fraud)

# IP reimbursement <= 0 
data.ip.reimburse <- data %>%
  filter(IPAnnualReimbursementAmt <= 0)
mean(data.ip.reimburse$Fraud)

# IP deduction = 0
data.ip.deduction <- data %>%
  filter(IPAnnualDeductibleAmt == 0)
mean(data.ip.deduction$Fraud)

# OP reimbursement <= 0
data.op.reimburse <- data %>%
  filter(OPAnnualReimbursementAmt <= 0)
mean(data.op.reimburse$Fraud)

# Hypothesis testing wether OP is different
n <- length(data.op.reimburse$Fraud)
p <- mean(data$Fraud)
x <- mean(data.op.reimburse$Fraud)
prop.test(x = x, n = n, p = p)
# Out reimbursement when negative implies that it follows a different distribution


fraud.counties <- data %>%
  filter(Fraud == 1) %>% 
  group_by(County) %>% 
  summarise(n = n()) %>% 
  filter(n >3000)

## Aggregating by providers (see if larger ones are more fraudulent)
data.prov <- data %>%
  group_by(ProviderID) %>%
  summarise(Fraud = mean(Fraud), Customers = n())
data.prov <- data.prov %>%
  group_by(group = cut(Customers, breaks = c(-Inf, seq(0, max(Customers), 50), Inf))) %>%
  summarise('Fraud %' = mean(Fraud), "Count" = n())
plot(data.prov$group, data.prov$`Fraud %`)
# From plot, larger providers are more likely to be fraudulent
# this might be because customers then are the ones that are fraudulent
# and the larger the cliental the more likely this will occur.


# Now have all data merged together with a toggle variable for inpatients vs outpatients

# Find if there is a higher probability of a provider causing insurance fraud 
# (As most times its the providers who do fraud - they have the incentive
# https://medium.com/better-programming/healthcare-fraud-detection-with-python-5a7a6738b5b2
# ^ good info in this link. It summarised-
# sometimes adjudicating cost of fraud is greater than cost of fraud itself

# Some questions that could be worth investigatin-
# Does age play a role in which claims are fraudulent?
# Month over month, are there any patterns of when fraud occurs?
# Do fraudulent providers make more per claim than non-fraudulent providers?
# Do fraudulent providers make more per patient than non-fraudulent providers (e.g., per member per month, or PMPM)

# aggregating data per provider - 
unique(data$ProviderID)

# Number of na values in provider-
length(data$ProviderID) - length(data$ProviderID[is.na(data$ProviderID)])





# In beneficiary 

# change chronicCondition columns into a single column ranging from 1-11 for each of the conditions and a seperate 12 for multiple
# include renal disease indicator in the above as well

# In inpatient

    # NOTE: CHECKING THAT ClmAdmitDiagnosisCode is same as final could be important for Fraud!!!
    # Need to check against all the other CLM data as well and if that leads to fraud

# In outpatient

    # Do same for the codes as inpatient

# Provider




