setwd("C:\\Users\\silvi\\OneDrive\\Desktop\\Thesis R")
#Combining the datasets
Data_control <- read.csv("C:\\Users\\silvi\\OneDrive\\Desktop\\Thesis R\\control.csv", header = TRUE)
Data_experiment <- read.csv("C:\\Users\\silvi\\OneDrive\\Desktop\\Thesis R\\experiment.csv", header = TRUE)
Data_control$Exp <- NA
Data_control$Exp <- 0
Data_experiment$Exp <- NA
Data_experiment$Exp <- 1
DataTotal <- rbind(Data_control,Data_experiment)
rm(Data_control)
rm(Data_experiment)
DataTotal
summary(DataTotal)
table(DataTotal$Gender[DataTotal$Exp==1])
table(DataTotal$TimeSinceLast)
summary(DataTotal$TimeSinceLast)

DataTotal$Education
DataTotal$Exp
#Package list
library(psych)
library(dplyr)
library(xlsx)
library(foreign)
library(Hmisc)
library(Scale)
library(ggplot2)
#Creating the final dataset empty
Final <- DataTotal[,c(2:8,90)]
#Creating the dataset for the dependent variable
Data <- NA
Data$Dep <- DataTotal$Dependent
Data$Exp <- DataTotal$Exp
#recoding the data for the dependent variable and adding it to the final
Data$Dep10 <-Data$Dep*10 
Data <- as.data.frame(Data)
Data$DepFinal <-        ifelse(Data$Dep10==70,1,
                        ifelse(Data$Dep10==60,2,
                        ifelse(Data$Dep10==50,3,
                        ifelse(Data$Dep10==40,4,
                        ifelse(Data$Dep10==30,5,
                        ifelse(Data$Dep10==20,6,
                        ifelse(Data$Dep10==10,7,
                        ifelse(Data$Dep10==80,8, 
                        ifelse(Data$Dep10==90,9,
                        ifelse(Data$Dep10==100,10,
                        ifelse(Data$Dep10==110, 11, 12)))))))))))
Final$Dependent <- as.numeric(Data$DepFinal)
rm(Data)
#Recoding the control and experimental data into factors
Final$Exp <- ifelse(Final$Exp==0,"Control","Experiment")
Final$Gender <- as.factor(Final$Gender)
summary(Final$Gender)
Final$Gender[Final$Gender=="Woman"] <- "Female"
summary(Final$Gender)
Final <- as.data.frame(Final)
#Creating and scoring the scales
#Before all of this I need to order the scales: Strongly agree has to be 1 and the others go forward...
############Further scale analysis graphics Check for proportions for scales in R in the file
#######################
#For the purpose of observing what exactly the participants agreed with we first create a new file with only one vector of the questions asked. Due to complications in the code this part of the data processing was done in google sheets. The document itself contains the calculation and will be made available on the selected repository. For the purpose of creating the plots we have used a specific methodology of plotting and it will be cited as normally


#the emotional smoking scale
#############################
ScaleRestraint <- DataTotal[,20:42]
#at 41,13 is an NA, imputing with median
ScaleRestraint[41,13] <- mean(na.omit((ScaleRestraint[,13])))
EmotionalScale <- ScaleRestraint[,11:23]
psych::alpha(EmotionalScale)

ScaleEmoptionalScaled <- Scale(data=ScaleRestraint[,c(10:23)], col_names = paste("q",1:14,sep=""))
ScaleEmotionProcessed <- PreProc(ScaleEmoptionalScaled)
ScaleEmotionItemAnalysis <- ItemAnalysis(ScaleEmotionProcessed)
ReportTable(ScaleEmotionItemAnalysis)
#Best item selection for the restrained smoking scale
#Best items are then used using the code below
EmotionBest <- ChooseBest(ScaleEmotionItemAnalysis)
ScaleEmoptionalScaledBest <- Scale(data=ScaleRestraint[,EmotionBest], col_names = EmotionBest)
ScaleEmotionProcessedBest <- PreProc(ScaleEmoptionalScaled)
ScaleEmotionItemAnalysisBest <- ItemAnalysis(ScaleEmotionProcessed)

print(RestrainedBest)
Final$EmotionalSmoking <- GetScores(ScaleEmotionItemAnalysisBest)
################################

#The restrained Smoking Scale
#####################################################
#For the self control scale the reversed items are SC2 and SC7
#The first item Restrained eating is from 1 to 10, Emotional eating is 11 to 23
#Before we actually score the items we need to test which items are ok and which are not. We do that by testing the cronbach alphas and observing which items should be removed
psych::alpha(ScaleRestraint[,1:10])
ScaleRestraintScaled <- Scale(data=ScaleRestraint[,c(1:3,5:8,10)], col_names = paste("q",1:8,sep=""))
ScaleRestraintProcessed <- PreProc(ScaleRestraintScaled)
ScaleRestraintItemAnalysis <- ItemAnalysis(ScaleRestraintProcessed)
ReportTable(ScaleRestraintItemAnalysis)

RestraintBest <- ChooseBest(ScaleRestraintItemAnalysis)
Final$Restrain <- GetScores(ScaleRestraintItemAnalysis)

#Best item selection for the restrained smoking scale
#For the record, I have no idea if the best items are saved into the final score or if it selects all of the items
#RestraintBest <- ChooseBest(ScaleRestraintItemAnalysis)
#ScaleRestraintScaledBest <- Scale(data=ScaleRestraint[,c(1:3,5:8,10)], col_names = RestraintBest)
#ScaleRestraintProcessedBest <- PreProc(ScaleRestraintScaledBest)
#ScaleRestraintItemAnalysisBest <- ItemAnalysis(ScaleRestraintProcessedBest)


#Final$RestrainedSmoking <- GetScores(ScaleRestraintItemAnalysisBest)
######Procedure for restarained smoking scale and the data on the items and why they were removed
#We can increase the Restraint scale to a higher level of .76 by removing either item 4,5,or 9
##We remove 4 and create the final scale
FinalRestraint <- (ScaleRestraint[,c(1:3,5:10)])
#We observe the second item and its cronbach alpha results and find them to be satisfactory with no items needing to be removed. Inputing the full results of the scale confirms each of these dimensions to be negatively correlated which confirms that they indeed measure different psychometric items. 
alpha(ScaleRestraint[,11:23])
alpha(ScaleRestraint)






#The smoking motivation scales, pharma and non-pharma
####################

#Smoking motivation scales
ScaleMotivation <- DataTotal[,43:63]
psych::alpha(ScaleMotivation)
ScaleMotivation <- ScaleMotivation[,c(1:11,13:17,19:21)]
psych::alpha(ScaleMotivation)
#Division into the parts
Pharma <- ScaleMotivation[,c(1:6,12:14,17,18)]
NotPharma <- ScaleMotivation[,c(7:11,15,16 )]
psych::alpha(Pharma)
psych::alpha(NotPharma)

#The pharmacological motivation scales output
ScalePharmaScaled <- Scale(data=Pharma, col_names = paste("q",1:11,sep=""))
ScalePharmaProcessed <- PreProc(ScalePharmaScaled)
ScalePharmaItemAnalysis <- ItemAnalysis(ScalePharmaProcessed)
ReportTable(ScalePharmaItemAnalysis)
#Best item selection for the restrained smoking scale
#For the record, I have no idea if the best items are saved into the final score or if it selects all of the items
PharmaBest <- ChooseBest(ScalePharmaItemAnalysis)
print(PharmaBest)
Final$Pharma <- GetScores(ScalePharmaItemAnalysis)

#The non-pharmacological motivation scales output
ScaleNotPharmaScaled <- Scale(data=NotPharma, col_names = paste("q",1:7,sep=""))
ScaleNotPharmaProcessed <- PreProc(ScaleNotPharmaScaled)
ScaleNotPharmaItemAnalysis <- ItemAnalysis(ScaleNotPharmaProcessed)
ReportTable(ScaleNotPharmaItemAnalysis)
#Best item selection for the restrained smoking scale
#For the record, I have no idea if the best items are saved into the final score or if it selects all of the items
NotPharmaBest <- ChooseBest(ScaleNotPharmaItemAnalysis)
print(NotPharmaBest)
Final$NotPharma <- GetScores(ScaleNotPharmaItemAnalysis)


#######################################
ScaleImpulsivity <- DataTotal[,64:76]
ScaleImpulsivity <- as.data.frame(ScaleImpulsivity)
psych::alpha(ScaleImpulsivity, keys=c(1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1))
#The reversed items are 3,4,11,12,2,10,5,13


ScaleImpulsivityScaled <- Scale(ScaleImpulsivity,reverse=c(2,3,4,5,10,11,12,13))
ScaleImpulsivityProcessed <- PreProc(ScaleImpulsivityScaled)
ScaleImpulsivityItemAnalysis <- ItemAnalysis(ScaleImpulsivityProcessed)
ReportTable(ScaleImpulsivityItemAnalysis)
RestrainedBest <- ChooseBest(ScaleRestraintItemAnalysis)
print(RestrainedBest)
Final$Impulsivity <- GetScores(ScaleImpulsivityItemAnalysis)
names(Final)
#SelfControl
ScaleSelfControl <- DataTotal[,77:89]
#2 and seven are reverse scored
psych::alpha(ScaleSelfControl, keys=c(1,-1,1,1,1,1,-1,1,1,1,1,1,1))
ScaleSelfControlScaled <- Scale(ScaleSelfControl,reverse=c(2,7))
ScaleSelfControlProcessed <- PreProc(ScaleSelfControlScaled)
ScaleSelfControlItemAnalysis <- ItemAnalysis(ScaleSelfControlProcessed)
ReportTable(ScaleSelfControlItemAnalysis)
RestrainedBest <- ChooseBest(ScaleSelfControlItemAnalysis)
print(RestrainedBest)
Final$SelfControl <- GetScores(ScaleSelfControlItemAnalysis)
names(Final)
#Saving the document
FinalScales <- Final[,1:16]
write.csv2(FinalScales, "finalscales.csv")


#Item analysis is ok now, I just need to add the reverse items inside and continue with the anova
#Do not forget to find visuals for these items
      #for stacked scale visuals use the code from here
#https://strengejacke.wordpress.com/2013/07/17/plotting-likert-scales-net-stacked-distributions-with-ggplot-rstats/
#http://rnotr.com/likert/ggplot/barometer/likert-plots/
summary(DataTotal$Gender[DataTotal$Exp==1])


##Fncy plots for anova and ancova

First <- qplot( x=factor(Exp) , y=Dependent , data=DataTotal , geom=c("boxplot","jitter"),xlab="Experimental Conditions", ylab = "Need for Smoking", main="Need for smoking dependent on the experimental conditions")
First
#Analysis of anova from
#"http://www.sthda.com/english/wiki/one-way-anova-test-in-r"
fit <- aov(data=Final, Dependent~as.factor(Exp))
summary(fit)
#this summary uses the linear model
summary.lm(fit)
#This is the turkey test
TukeyHSD(fit)
#This checkes the homogenity of the variance, observations 15 and 50 are classified as outliers
plot(fit,1)
      #To confirm homogenity we can also test with Levene's test
library(car)
leveneTest(Dependent~Exp, data=FinalWOO)
#testing for data normality
plot(fit,2)
      #It does not really look completely normal so we need to check for normality using the shapiro test
residuals <- residuals(object=fit)
shapiro.test(residuals)
      #The shapiro test confirms normality is violated so we remove the outliers and re-test
FinalWOOutliers <- Final[-c(15,32,60),]
fit2 <- aov(data=FinalWOOutliers, Dependent~Exp)
summary(fit2)
summary.lm(fit2)
plot(fit2,1)
plot(fit2,2)
residuals <- residuals(object=fit2)
shapiro.test(residuals)

boxplot(Final$Dependent ~Final$Exp)

#ANCOVA
#Ancova from http://www.flutterbys.com.au/stats/tut/tut7.5a.html
names(Final)
AncovaGender <- aov(data=Final, Dependent~Exp*Gender)
summary.lm(AncovaGender)
#Total ANCOVA
Final$EmotionalSmoking <- as.numeric(as.character(unlist(Final$EmotionalSmoking[[1]])))
Final$RestrainedSmoking <- as.numeric(as.character(unlist(Final$RestrainedSmoking[[1]])))
Final$Pharma <- as.numeric(as.character(unlist(Final$Pharma[[1]])))
Final$NotPharma <- as.numeric(as.character(unlist(Final$NotPharma[[1]])))
Final$Impulsivity <- as.numeric(as.character(unlist(Final$Impulsivity[[1]])))
Final$SelfControl <- as.numeric(as.character(unlist(Final$SelfControl[[1]])))
Final$PharmaDummy <- ifelse(Final$Pharma>=Final$NotPharma,1,0)

Final$PharmaDummy <- as.factor(Final$PharmaDummy)
AncovaTotal <- aov(data=Final, Dependent~Exp+Gender)
Final$Gender
summary.lm(AncovaTotal)
par(mfrow=c(1,1))
names(Final)

anovaConditions <- aov(data=Final, Dependent~Exp)
summary.lm(anovaConditions)
summary(anovaConditions)
employmentAncova <- aov(data=Final, Dependent~Exp+employment)
summary.lm(employmentAncova)

#Gender two way anova
anovaGender <- aov(data=Final, Dependent~Exp*Gender)
summary.lm(anovaGender)
summary(anovaGender)
#Results tests
#This is the turkey test
TukeyHSD(anovaGender)
#This checkes the homogenity of the variance, observations 15 and 50 are classified as outliers
plot(anovaGender,1)
#To confirm homogenity we can also test with Levene's test
library(car)
FinalWOOutliers <- Final[-c(15,32,60),]
leveneTest(Dependent~Exp, data=Final)
#testing for data normality
par(mfrow=c(2,2))
plot(anovaGender)
#It does not really look completely normal so we need to check for normality using the shapiro test
residuals <- residuals(object=anovaGender)
shapiro.test(residuals)
#Results are reported


#Time since last analysis
TimeSinceLastAncova <- aov(data=Final, Dependent~Exp*TimeSinceLast)
summary.lm(TimeSinceLastAncova)
summary(TimeSinceLastAncova)
#Results tests
#This is the turkey test
TukeyHSD(anovaGender)
#This checkes the homogenity of the variance, observations 15 and 50 are classified as outliers
plot(anovaGender,1)
#To confirm homogenity we can also test with Levene's test
library(car)
FinalWOOutliers <- Final[-c(15,32,60),]
leveneTest(Dependent~Exp, data=Final)
#testing for data normality
par(mfrow=c(2,2))
plot(anovaGender)
#It does not really look completely normal so we need to check for normality using the shapiro test
residuals <- residuals(object=anovaGender)
shapiro.test(residuals)




#Emotional reasons for smoking analysis
names(Final)
#Error at 13,33,44, imputing median
Final$Restrain[c(13,33,44),] <- median(Final$Restrain[-c(13,33,44),])
Final$Restrain <- unlist(Final$Restrain, use.names = FALSE)
Final$Restrain <- as.integer(Final$Restrain)
library(corrplot)
corrplot(Final)
emotionalAncova <- aov(data=Final, Dependent~Exp*EmotionalSmoking)
restraintAncova <- aov(data=Final, Dependent~Restrain*Exp)
summary(restraintAncova)
summary.lm(restraintAncova)
alias(restraintAncova)
?alias
summary(restraint2)
summary.lm(emotionalAncova)
summary(emotionalAncova)
restraint2 <- aov(data=Final, Dependent~Exp*Pharma*Restrain)
#Results tests
#This is the turkey test
forplot <- TukeyHSD(emotionalAncova, which="color")
plot(forplot)
#This checkes the homogenity of the variance, observations 15 and 50 are classified as outliers
plot(emotionalAncova)
#testing for data normality
par(mfrow=c(2,2))
plot(emotionalAncova)
#It does not really look completely normal so we need to check for normality using the shapiro test
residuals <- residuals(object=emotionalAncova)
shapiro.test(residuals)
#Najbitnije
#############
#Pharmacological reasons for smoking
PharmaAncova <- aov(data=Final, Dependent~Exp*Pharma)
summary.lm(PharmaAncova)
summary(PharmaAncova)
#testing for data normality
par(mfrow=c(2,2))
plot(PharmaAncova)
#It does not really look completely normal so we need to check for normality using the shapiro test
residuals <- residuals(object=PharmaAncova)
shapiro.test(residuals)


NotPharmaAncova <- aov(data=Final, Dependent~NotPharma)
summary.lm(NotPharmaAncova)
summary(NotPharmaAncova)

PharmaAll <- aov(data=Final, Dependent~Exp*Pharma*NotPharma)
FinalBox <- Final
Vektor <- FinalBox$Pharma
Vektor <- cut(Vektor,2)
FinalBox$PharmaFactor <- Vektor
FinalBox <- na.omit(FinalBox)
PharmaBox <- aov(data=FinalBox, Dependent~Exp*PharmaFactor)
summary(PharmaBox)
summary.lm(PharmaBox)
#statistics
TukeyHSD(PharmaBox)
#This checkes the homogenity of the variance, observations 15 and 50 are classified as outliers
par(mfrow=c(2,2))
plot(PharmaBox)

#To confirm homogenity we can also test with Levene's test
library(car)
FinalWOOutliers <- Final[-c(15,32,60),]
leveneTest(Dependent~Exp*PharmaFactor, data=Final)
#testing for data normality
par(mfrow=c(2,2))
plot(anovaGender)
#It does not really look completely normal so we need to check for normality using the shapiro test
residuals <- residuals(object=PharmaBox)
shapiro.test(residuals)




#Interaction effect graph
library(plyr)
FinalBox$PharmaFactor<- revalue(FinalBox2$PharmaFactor, c("(-1.12,-0.336]"="High", "(-0.336,0.452]"="Low"))
ggline(FinalBox, x = "Exp", y = "Dependent", color = "PharmaFactor",
       add = c("mean_se", "dotplot"), legend.title = "Pharmacological \n reasons for smoking" , legend="right",
       palette = c("#00AFBB", "#E7B800"))

interaction.plot(x.factor = FinalBox$Exp,
                 trace.factor = FinalBox$PharmaFactor,
                 response = FinalBox$Dependent,
                 fun=mean,
                 type="b",
                 col=c("black","red","green"),
                 pch=c(19, 17, 15), 
                 fixed=TRUE,  
                 leg.bty = "o")

PharmaFactor <- aov(data=FinalBox, Dependent~Exp*as.factor(Pharma))
summary(PharmaAll)
summary.lm(PharmaAll)
names(FinalBox)
library(gplots)
library(mediation)
FinalBox3 <- FinalBox
summary(FinalBox3)
FinalBox3$Exp <- as.integer(as.factor(FinalBox$Exp))
FinalBox3$PharmaFactor <- as.integer(as.factor(PharmaFactor))
mediator <- lm(Exp~Pharma+Dependent, data=FinalBox3)
summary(mediator)
mediator2 <- lm(data=Final3, Exp~RestrainFactor +Dependent)
summary(mediator2)
#The summary shows all of the variables inside, we can notice the location variables. etc etc

as.integer(as.factor(FinalBox$Exp))
as.integer(as.factor(FinalBox$PharmaFactor))
dependentVariable <- lm(Dependent~Exp*Pharma, data=FinalBox3)
summary(dependentVariable)
Final3 <- Final
Final3$Exp<- as.integer(as.factor(Final3$Exp))
dependentVariable2 <- lm(data=Final3, Dependent~Exp+RestrainFactor)
summary(dependentVariable2)

names(FinalBox)
Mediation <- mediation::mediate(mediator, dependentVariable, treat="Exp", mediator="Pharma",robustSE = TRUE, sins=100, boot=TRUE)
Mediation2 <- mediation::mediate(mediator2, dependentVariable2,treat="Exp", mediator="Restrain",robustSE = TRUE, sins=100, boot=TRUE)
summary(Mediation2)
par(mfrow=c(1,1))
plot.mediate(Mediation2)
??Mediation
pairs.panels(Final4)
summary(Mediation)
#Restraint additional 
par(mfrow=c(2,2))
plot(restraintAncova)
residuals <- residuals(object=restraintAncova)
shapiro.test(residuals)
names(Final3)
Vektor <- Final3$Restrain
Vektor <- cut(Vektor,2)
Vektor
Final3$RestrainFactor <- Vektor
Final3$RestrainFactor<- revalue(Final3$RestrainFactor, c("(-1,-0.5]"="Low", "(-0.5,0.001]"="High"))


library(ggpubr)
names(Final3)
ggline(Final3, x = "Exp", y = "Dependent", color = "RestrainFactor",
       add = c("mean_se", "dotplot"), legend.title = "Restrain" , legend="right",
       palette = c("#00AFBB", "#E7B800"))
summary(Final3)
interaction.plot(x.factor = Final3$Exp,
                 trace.factor = Final3$RestraintFactor,
                 response = Final3$Dependent,
                 fun=mean,
                 type="b",
                 col=c("black","red","green"),
                 pch=c(19, 17, 15), 
                 fixed=TRUE,  
                 leg.bty = "o")



Impulsivity <- aov(data=Final, Dependent~Exp*Impulsivity)
summary(Impulsivity)
summary.lm(Impulsivity)
par(mfrow=c(2,2))
plot(Impulsivity)


SelfControl <- aov(data=Final, Dependent~SelfControl*Exp)
summary.lm(SelfControl)
summary(SelfControl)
plot(SelfControl)
#Final model
FinalModel <- aov(data=FinalBox3, Dependent~Exp*Pharma+SelfControl)
summary(FinalModel)
summary.lm(FinalModel)
plot(FinalModel)


#The ggplot
library(ggpubr)

ggplot(Final, aes(x = Exp, y = Dependent, color=Exp, palette="jco", add="jitter")) + geom_boxplot() +
      labs(title="Need for smoking levels as observed through the experimental and control conditions", color="Condition")+
      ylab("Need For smoking")

#Better ggplot


#This is cool but it is for paired observations which ours are not.
####################
#Getting the data on the dependent variable
Before <- Final[which(Final$Exp=="Control"),]
After <- Final[which(Final$Exp=="Experiment"),]
#Switching to the graph needed
Control <- Before$Dependent
Experiment <- After$Dependent
d <- data.frame(Control=Control, Experiment=Experiment)

ggpaired(d, cond1 = "Control", cond2 = "Experiment",
         fill = "condition", palette = "jco")

#The correlation plot
library(ggcorrplot)
Final2 <- as.data.frame(sapply(Final[,c(7,9:14)], as.numeric))
p.mat <- cor_pmat(Final2)
ggcorrplot(p.mat)

plot(AncovaTotal)
summary.lm(AncovaTotal)
plot(cor(Final2, method = "pearson", use = "complete.obs"))
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(test, histogram=TRUE, pch=19)
names(Final3)
Final4 <- Final3[,c(2,8,9,10,11,12,13,14,16)]
names(Final4)
plot(cor(test, method = "pearson", use = "complete.obs"))
test <- data.matrix(Final4, rownames.force = NA)

chart.Correlation(Final4, histogram=TRUE, pch=19)

#Linear model
total <- lm(data=Final, Dependent~.)
summary(total)

#Control analysis
names(Final3)
Gender <- aov(data=Final3, Exp~Gender)
summary(Gender)
EmotionalSmoking <- aov(data=Final3, Exp~EmotionalSmoking)
summary(EmotionalSmoking)
Pharma <- aov(data=Final3, Exp~Pharma)
summary(Pharma)
NotPharma <- aov(data=Final3, Exp~NotPharma)
summary(NotPharma)
Impulsivity <- aov(data=Final3, Exp~Impulsivity)
summary(Impulsivity)
SelfControl <- aov(data=Final3, Exp~SelfControl)
summary(SelfControl)
Restrain <- aov(data=Final3, Exp~Restrain)
summary(Restrain)
Age <- aov(data=Final3, Exp~Age)
summary(Age)
