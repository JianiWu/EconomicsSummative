#Load libraries needed for graphing, statistical tests, and machine learning
library(ggplot2)
library(rpart)
library(dplyr)
library(caTools)
library(randomForest)
library(caret)
library(doSNOW)
library(rpart.plot)
library(fabricatr)

#Read the files with data
Vaccinations <- read.csv("COVID-19 Vaccinations.csv")
Economics <- read.csv("Fraser Institute World Economic Freedom Test.csv")

#Create variable of number of days vaccination program in place
Days <- c()
for(i in 1:nrow(Economics)){
  x <- sum(Vaccinations$iso_code == Economics[i,1])
  Days <- c(Days, x)
}
Economics <- cbind(Economics, Days)

#Calculate average number of vaccinations per hundred people per day
Rate <- c()
for(i in 1:nrow(Economics)){
  x <- Vaccinations %>%
    filter(iso_code == Economics[i,1]) %>%
    slice_head(n = 1) %>%
    select(total_vaccinations_per_hundred)
  y <- Vaccinations %>%
    filter(iso_code == Economics[i,1]) %>%
    slice_tail(n = 1) %>%
    select(total_vaccinations_per_hundred)
  z <- (y - x)/Days[i]
  Rate <- c(Rate, z)
}
#Add Rate as a variable to Economics dataframe
Economics$Rate <- Rate
#Change from list to vector
Economics$Rate <- as.vector(unlist(Economics$Rate))

mean(Economics$Rate)
sd(Economics$Rate)

#Create violin plot
Economics$X <- rep(c("X"), times = 114)
ggplot(Economics, aes(X, Rate)) +
  geom_violin(fill = "darkolivegreen1") +
  geom_boxplot(fill = "darkolivegreen4") +
  ylab("Rate of Vaccination per Hundred per day")

#Assign median value to "NA"s
Economics$Transfers.and.subsidies[is.na(Economics$Transfers.and.subsidies)] <- median(Economics$Transfers.and.subsidies, na.rm = TRUE)
Economics$Government.investment[is.na(Economics$Government.investment)] <- median(Economics$Government.investment, na.rm = TRUE)
Economics$Top.marginal.income.and.payroll.tax.rate[is.na(Economics$Top.marginal.income.and.payroll.tax.rate)] <- median(Economics$Top.marginal.income.and.payroll.tax.rate, na.rm = TRUE)
Economics$State.ownership.of.assets[is.na(Economics$State.ownership.of.assets)] <- median(Economics$State.ownership.of.assets, na.rm = TRUE)
Economics$Military.interference.in.rule.of.law.and.politics[is.na(Economics$Military.interference.in.rule.of.law.and.politics)] <- median(Economics$Military.interference.in.rule.of.law.and.politics, na.rm = TRUE)

summary(lm(Economics$Rate ~ Economics$Economic.Freedom.Summary.Index)) #*, **
summary(lm(Economics$Rate ~ Economics$Rank)) #***, **
summary(lm(Economics$Rate ~ Economics$Government.consumption)) #***, ***
summary(lm(Economics$Rate ~ Economics$Transfers.and.subsidies)) #***, *
summary(lm(Economics$Rate ~ poly(Economics$Government.investment, 2))) #***, *, *
summary(lm(Economics$Rate ~ Economics$Top.marginal.income.tax.rate))
summary(lm(Economics$Rate ~ Economics$Top.marginal.income.and.payroll.tax.rate))
summary(lm(Economics$Rate ~ Economics$Top.marginal.tax.rate))
summary(lm(Economics$Rate ~ Economics$State.ownership.of.assets))
summary(lm(Economics$Rate ~ Economics$Size.of.Government))
summary(lm(Economics$Rate ~ Economics$Judicial.independence))
summary(lm(Economics$Rate ~ Economics$Impartial.courts)) #*, ***
summary(lm(Economics$Rate ~ poly(Economics$Protection.of.property.rights, 1))) #***, **
summary(lm(Economics$Rate ~ poly(Economics$Military.interference.in.rule.of.law.and.politics, 1))) #***, *
summary(lm(Economics$Rate ~ Economics$Integrity.of.the.legal.system))
summary(lm(Economics$Rate ~ Economics$Legal.enforcement.of.contracts))
summary(lm(Economics$Rate ~ Economics$Regulatory.restrictions.on.the.sale.of.real.property))
summary(lm(Economics$Rate ~ Economics$Reliability.of.police))
summary(lm(Economics$Rate ~ Economics$Gender.Legal.Rights.Adjustment))
summary(lm(Economics$Rate ~ Economics$Legal.System...Property.Rights))
summary(lm(Economics$Rate ~ Economics$Money.growth))
summary(lm(Economics$Rate ~ Economics$Standard.deviation.of.inflation))
summary(lm(Economics$Rate ~ Economics$Inflation..Most.recent.year))
summary(lm(Economics$Rate ~ Economics$Freedom.to.own.foreign.currency.bank.accounts))
summary(lm(Economics$Rate ~ Economics$Sound.Money))
summary(lm(Economics$Rate ~ Economics$Revenue.from.trade.taxes....of.trade.sector.))
summary(lm(Economics$Rate ~ Economics$Mean.tariff.rate)) #**, ***
summary(lm(Economics$Rate ~ Economics$Standard.deviation.of.tariff.rates))
summary(lm(Economics$Rate ~ Economics$Tariffs))
summary(lm(Economics$Rate ~ Economics$Non.tariff.trade.barriers))
summary(lm(Economics$Rate ~ Economics$Compliance.costs.of.importing.and.exporting))
summary(lm(Economics$Rate ~ Economics$Regulatory.trade.barriers))
summary(lm(Economics$Rate ~ Economics$Financial.Openness))
summary(lm(Economics$Rate ~ Economics$Capital.controls)) #*, **
summary(lm(Economics$Rate ~ Economics$Freedom.of.foreigners.to.visit))
summary(lm(Economics$Rate ~ Economics$Controls.of.the.movement.of.capital.and.people))
summary(lm(Economics$Rate ~ Economics$Freedom.to.Trade.Internationally)) #*, ***
summary(lm(Economics$Rate ~ Economics$Ownership.of.banks))
summary(lm(Economics$Rate ~ Economics$Private.sector.credit))
summary(lm(Economics$Rate ~ Economics$Interest.rate.controls.negative.real.interest.rates.))
summary(lm(Economics$Rate ~ Economics$Credit.market.regulations))
summary(lm(Economics$Rate ~ Economics$Hiring.regulations.and.minimum.wage))
summary(lm(Economics$Rate ~ Economics$Hiring.and.firing.regulations))
summary(lm(Economics$Rate ~ Economics$Centralized.collective.bargaining))
summary(lm(Economics$Rate ~ Economics$Hours.Regulations))
summary(lm(Economics$Rate ~ Economics$Mandated.cost.of.worker.dismissal))
summary(lm(Economics$Rate ~ Economics$Conscription))
summary(lm(Economics$Rate ~ Economics$Labor.market.regulations))
summary(lm(Economics$Rate ~ Economics$Administrative.requirements))
summary(lm(Economics$Rate ~ Economics$Regulatory.Burden))
summary(lm(Economics$Rate ~ Economics$Starting.a.business))
summary(lm(Economics$Rate ~ Economics$Impartial.Public.Administration))
summary(lm(Economics$Rate ~ Economics$Licensing.restrictions))
summary(lm(Economics$Rate ~ Economics$Tax.compliance))
summary(lm(Economics$Rate ~ Economics$Business.regulations)) #*, ***
summary(lm(Economics$Rate ~ Economics$Regulation))

#Graphs for linear regression
#Summary
ggplot(Economics, aes(Economic.Freedom.Summary.Index, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Rank
ggplot(Economics, aes(Rank, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Government consumption
ggplot(Economics, aes(Government.consumption, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Transfers and subsidies
ggplot(Economics, aes(Transfers.and.subsidies, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Government investment
ggplot(Economics, aes(Government.investment, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 2), colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Impartial courts
ggplot(Economics, aes(Impartial.courts, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Protection of property.rights
ggplot(Economics, aes(Protection.of.property.rights, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Military interference in rule of law and politics
ggplot(Economics, aes(Military.interference.in.rule.of.law.and.politics, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Mean tariff rate
ggplot(Economics, aes(Mean.tariff.rate, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Capital controls
ggplot(Economics, aes(Capital.controls, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Freedom to Trade Internationally
ggplot(Economics, aes(Freedom.to.Trade.Internationally, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")
#Business regulations
ggplot(Economics, aes(Business.regulations, Rate, colour = Rate)) +
  geom_point(size = 2) +
  scale_colour_gradient(low = "darkseagreen", high = "darkseagreen3") +
  geom_smooth(method = "lm", colour = "darkseagreen4") +
  ylim(-0.1, 1.3) +
  ylab("Rate of Vaccination per Hundred per day")

#Assign quintile value to Rate and add variable to Economics dataframe
Economics$Half <- split_quantile(Economics$Rate, type = 2)

#Train a Random Forest with the parameters as Government.consumption & Transfers.and.subsidies
RfTrain1 <- Economics[c("Government.consumption", "Transfers.and.subsidies")]
RfLabel <- as.factor(Economics$Half)
set.seed(2)
#Generate the Random Forest
Rf1 <- randomForest(x = RfTrain1, y = RfLabel, importance = TRUE, ntree = 1000)
Rf1
#Result: OOB estimate of error rate: 22.81%

#Train a Random Forest with the parameters as Government.consumption & Impartial.courts
RfTrain2 <- Economics[c("Government.consumption", "Impartial.courts")]
RfLabel <- as.factor(Economics$Half)
set.seed(2)
#Generate the Random Forest
Rf2 <- randomForest(x = RfTrain2, y = RfLabel, importance = TRUE, ntree = 1000)
Rf2
#Result: OOB estimate of error rate: 26.32%

#Assign median value to "NA"s
Economics$Capital.controls[is.na(Economics$Capital.controls)] <- median(Economics$Capital.controls, na.rm = TRUE)

#Train a Random Forest with the parameters as Government.consumption, Transfers.and.subsidies & Freedom.to.Trade.Internationally
RfTrain3 <- Economics[c("Government.consumption", "Transfers.and.subsidies", "Freedom.to.Trade.Internationally")]
RfLabel <- as.factor(Economics$Half)
set.seed(2)
#Generate the Random Forest
Rf3 <- randomForest(x = RfTrain3, y = RfLabel, importance = TRUE, ntree = 1000)
Rf3
#Result: OOB estimate of error rate: 15.79%

#Train a Random Forest with the parameters as Economic.Freedom.Summary.Index, Impartial.courts, Mean.tariff.rate, Freedom.to.Trade.Internationally
RfTrain4 <- Economics[c("Economic.Freedom.Summary.Index", "Impartial.courts", "Mean.tariff.rate", "Freedom.to.Trade.Internationally")]
RfLabel <- as.factor(Economics$Half)
set.seed(2)
#Generate the Random Forest
Rf4 <- randomForest(x = RfTrain4, y = RfLabel, importance = TRUE, ntree = 1000)
Rf4
#Result: OOB estimate of error rate: 17.54%

#Train a Random Forest with the parameters as Economic.Freedom.Summary.Index, Impartial.courts, Government.investment, Freedom.to.Trade.Internationally
RfTrain5 <- Economics[c("Economic.Freedom.Summary.Index", "Impartial.courts", "Government.investment", "Freedom.to.Trade.Internationally")]
RfLabel <- as.factor(Economics$Half)
set.seed(2)
#Generate the Random Forest
Rf5 <- randomForest(x = RfTrain5, y = RfLabel, importance = TRUE, ntree = 1000)
Rf5
#Results: OOB estimate of error rate: 21.05%

#Visualize importance of variables in best-performing Random Forests
varImpPlot(Rf1)
varImpPlot(Rf3)
varImpPlot(Rf4)
varImpPlot(Rf5)

RpartCV <- function(Seed, Training, Labels, Ctrl){
  Cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(Cl)
  set.seed(Seed)
  RpartCV <- train(x = Training, y = Labels, method = "rpart", tuneLength = 30,
                   trControl = Ctrl)
  stopCluster(Cl)
  return(RpartCV)
}

set.seed(2)
CvFolds <- createMultiFolds(RfLabel, k = 10, times = 10)
Ctrl2 <- trainControl(method ="repeatedcv", number = 10, repeats = 10,
                      index = CvFolds)

#Grab features for Rf1
Features <- c("Government.consumption", "Transfers.and.subsidies")
RpartTrain1 <- Economics[1:114, Features]
Rpart1CV1 <- RpartCV(1, RpartTrain1, RfLabel, Ctrl2)
Rpart1CV1
#Plot
prp(Rpart1CV1$finalModel, type = 0, extra = 1, under = TRUE)
#Results: Accuracy = 0.7812879, Kappa = 0.5612546

#Grab features for Rf3
Features <- c("Government.consumption", "Transfers.and.subsidies", "Freedom.to.Trade.Internationally")
RpartTrain3 <- Economics[1:114, Features]
Rpart3CV1 <- RpartCV(2, RpartTrain3, RfLabel, Ctrl2)
Rpart3CV1
#Plot
prp(Rpart3CV1$finalModel, type = 0, extra = 1, under = TRUE)
#Results: Accuracy = 0.7722879, Kappa = 0.5437047

#Grab features for Rf4
Features <- c("Economic.Freedom.Summary.Index", "Impartial.courts", "Mean.tariff.rate", "Freedom.to.Trade.Internationally")
RpartTrain4 <- Economics[1:114, Features]
Rpart4CV1 <- RpartCV(2, RpartTrain4, RfLabel, Ctrl2)
Rpart4CV1
#Plot
prp(Rpart4CV1$finalModel, type = 0, extra = 1, under = TRUE)
#Results: Accuracy = 0.7876212, Kappa = 0.5753259

#Grab features for Rf5
Features <- c("Economic.Freedom.Summary.Index", "Impartial.courts", "Government.investment", "Freedom.to.Trade.Internationally")
RpartTrain5 <- Economics[1:114, Features]
Rpart5CV1 <- RpartCV(2, RpartTrain5, RfLabel, Ctrl2)
Rpart5CV1
#Plot
prp(Rpart5CV1$finalModel, type = 0, extra = 1, under = TRUE)
#Results: Accuracy = 0.7396061, Kappa = 0.47817232
