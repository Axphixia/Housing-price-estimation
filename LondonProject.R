#############################################
# Housing price estimation in London        #
#############################################

##################################
#### Installing required packages 
##################################

################## Packages
if(!require(classInt)) install.packages("classInt", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(rgeos)) install.packages("rgeos", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")

library(classInt)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(MASS)
library(readxl)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(tidyverse)
library(PerformanceAnalytics)

# Accesing our dataset 
LonDat <- read.csv("data/LonDat.csv",stringsAsFactors=FALSE)
print(dim(LonDat))
head(LonDat)
str(LonDat)
summary(LonDat)

# Data variables
par(mar = c(1, 1, 1, 1))
datavar <- read_excel("data/variables.xlsx", sheet = "Data")
plot.new()
grid.table(datavar, rows=NULL)

#Checking for correlation
Corvar <- cor(LonDat[,c(4,23:31)])
head(round(Corvar,2))

# Correlation Analysis Plot
corrplot(Corvar, method="color", tl.col="black", tl.srt=45, type = "upper", order="hclust", sig.level = 0.01)
chart.Correlation(Corvar, histogram=TRUE, pch=19, gap=4)
col<- colorRampPalette(c("#1f235a", "white", "#ff0101"))(20)
heatmap(x = Corvar, col = col, symm = TRUE)

#Data_Wrangling
par(mar = c(1, 1, 1, 1))
datavar <- read_excel("data/variables.xlsx", sheet = "Variables")
plot.new()
grid.table(datavar, rows=NULL)

# Converting dummies to factors
Dummy <- function(mat,lev1="Level1") {
  mat <- as.matrix(mat)
  factor((mat %*% (1:ncol(mat))) + 1,
         labels = c(lev1, colnames(mat)))
}

Age      <- Dummy(LonDat[,5:9],"SXX")
Type     <- Dummy(LonDat[,10:12],"Others")
Garage   <- Dummy(LonDat[,13:14],"Aparcam")
Bedrooms <- Dummy(LonDat[,18:21],"HabUna")

Project <- data.frame(LonDat[,c(2:4,15:17,22,23,26)],Age,Type,Garage,Bedrooms)
summary(Project)

#Defining final columns
Project$AlPro <- factor(Project$AlPro)
Project$Acond <- factor(Project$Acond)
Project$DoBa <- factor(Project$DoBa)
Project$CXXI <- factor(Project$CXXI)

levels(Project$AlPro) <- c("no", "yes")
levels(Project$Acond) <- c("no", "yes")
levels(Project$DoBa) <- c("no", "yes")
levels(Project$CXXI) <- c("no", "yes")

#Final Columns: show
head(Project)

#Outlier
boxplot(LonDat$PrCompra,xlab = "All properties", ylab = "Purchase Price",col = "steelblue", lwd = 0.5)
LonDat <- LonDat[LonDat$PrCompra < 600000,]

# We can see Properties higher than 600k - Outlier to be checked
boxplot(LonDat$PrCompra,xlab = "Properties under 600k ", ylab = "Purchase Price",col = "steelblue", lwd = 0.5)

#Purchase price vs Floor Plot
par(mar = c(1, 1, 1, 1))
plot(LonDat[,c("MCC","PrCompra")],pch=3,cex=0.5,xlab = "Floor Area", ylab = "Purchase Price")
lines(lowess(LonDat[,c("MCC","PrCompra")]),col="steelblue",lwd = 2)

#House Prices distribution per District
par(mar = c(1, 1, 1, 1))
nClass = 10
Palette <- rev(brewer.pal(nClass,"Blues"))
Classes <- classIntervals(LonDat$PrCompra,nClass,"quantile")
Colours <- findColours(Classes,Palette)
plot(LonDat$Longitud,LonDat$Latitud,pch=16,cex=0.5,col=Colours,asp=1,xlab = "Longitude", ylab = "Latitude")
title("House prices distribution per District")

#Model_Price_Floor
Long <- Project$Longitud/1000
Lat <- Project$Latitud/1000
mod1 <- lm(PrCompra~Long+Lat,data=Project)
AIC(mod1)

mod2 <- lm(PrCompra~Long+Lat+I(Long^2)+I(Lat^2)+I(Long*Lat),data=Project)
AIC(mod2)

summary(mod1) # Price decreases when move East. A bit lower by moving South

summary(mod2) # A lower AIC means higher price as we move West

stepAIC(mod2)

#Purchase Price Plot
ppp1 <- ggplot(Project, aes(x=PrCompra)) + geom_histogram(bins = 100, color="black", fill="steelblue")+
  geom_vline(aes(xintercept=mean(PrCompra)),
             color="red", linetype="dashed", size=1)

ppp2 <- ggplot(Project, aes(x=PrCompra)) + 
  geom_histogram(bins = 100, aes(y=..density..), colour="black", fill="steelblue")+
  geom_density(alpha=.5, fill="#e2e9f2")

ppp3 <- ggplot(Project, aes(x=log(PrCompra))) + 
  geom_histogram(bins = 100, aes(y=..density..), colour="black", fill="steelblue")+
  geom_density(alpha=.5, fill="#e2e9f2")

grid.arrange(ppp1, ppp2, ppp3, nrow=3)

#Predictors Plot
pp5 <- ggplot(Project, aes(x=log(PrCompra), color=Project$Acond)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp6 <- ggplot(Project, aes(x=log(PrCompra), color=Project$AlPro)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp7 <- ggplot(Project, aes(x=log(PrCompra), color=Project$CXXI)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp8 <- ggplot(Project, aes(x=log(PrCompra), color=Project$Age)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp9 <- ggplot(Project, aes(x=log(PrCompra), color=Project$Type)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp10 <- ggplot(Project, aes(x=log(PrCompra), color=Project$Garage)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp11 <- ggplot(Project, aes(x=log(PrCompra), color=Project$Bedrooms)) +
  geom_freqpoly()+
  theme(legend.position = "top")

pp12 <- ggplot(Project, aes(x=log(PrCompra), color=Project$DoBa)) +
  geom_freqpoly()+
  theme(legend.position = "top")

#Predictors_Comparison1
grid.arrange(pp8, pp9, nrow=2)

#Predictors_Comparison2
grid.arrange(pp10, pp11, nrow=2)

#Predictors_Comparison3
grid.arrange(pp5, pp12, nrow=2)

#Predictors_Comparison4
grid.arrange(pp6, pp7, nrow=2)

#AIC
AICs <- rep(NA,10)
Models <- vector("list",10)
Vars <- colnames(Project)[4:13]
for(i in 1:10) {
  Models[[i]] <- lm(formula(paste0("PrCompra~",Vars[i])),data=Project)
  AICs[i] <- AIC(Models[[i]])
}
print(AICs)

minAIC <- which.min(AICs)
print(AICs[minAIC])
print(Vars[minAIC])
summary(Models[[minAIC]])

#Evidence in favor of the model
delta <- AICs - min(AICs)  # Differences
evidence <- exp(-0.5*delta)/sum(exp(-0.5*delta))# Probabilities
evidence

#AIC_Results
par(mar = c(1, 1, 1, 1))
names(AICs) <- Vars                        
sAICs <- sort(AICs)                         
print(sAICs)
plot(sAICs,xaxt="n", axes = FALSE, type = "b", pch = 21, cex = 2, lwd = 2, bg = "grey", col = "steelblue")                      
axis(1,labels=names(sAICs),at=1:length(Vars),las=2,cex.axis=.75)
axis(2)
for(i in 2:length(Vars)){                    
  cat(paste(names(sAICs)[i],sAICs[i]-sAICs[i-1],"\n"))
}

#Linear_Coeficients
linmodel <- lm(log(PrCompra)~ AlPro+ Acond+ DoBa+ CXXI+ MCC+ ProfC+ Age+ Type+ Garage+ Bedrooms, data=Project)
summary(linmodel)

#Subset_Both
ld_model <- lm(PrCompra~AlPro+Acond+DoBa+CXXI+MCC+ProfC+Age+Type+Garage+Bedrooms, data = Project)
step <- stepAIC(ld_model, direction="both")
step$anova

#Subset_Forward
library(leaps)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(Project), replace = T, prob = c(0.6,0.4))
train <- Project[sample, ]
test <- Project[!sample, ]

#Best subsets with Forward selection
par(mar = c(1, 1, 1, 1))
ld_orgnl_frwd <- regsubsets(PrCompra~AlPro+Acond+DoBa+CXXI+MCC+ProfC+Age+Type+Garage+Bedrooms, data = train, method = "forward",nvmax = 10)
ld_frwd <- regsubsets(log(PrCompra)~AlPro+Acond+DoBa+CXXI+MCC+ProfC+Age+Type+Garage+Bedrooms, data = train, method = "forward",nvmax = 10)
results <- summary(ld_orgnl_frwd)

par(mar = c(1, 1, 1, 1))
plot(ld_orgnl_frwd,scale="adjr2",col = col)
title(main= "Best subsets Forward selection plot for Price")
plot(ld_frwd,scale="adjr2" ,col = col)
title(main= "Best subsets Forward selection plot for Log Price")

#Best subsets with Backward selection
par(mar = c(1, 1, 1, 1))
ld_orgnl_bkwd <- regsubsets(PrCompra~AlPro+Acond+DoBa+CXXI+MCC+ProfC+Age+Type+Garage+Bedrooms, data = train, method = "backward",nvmax = 10)
ld_bkwd <- regsubsets(log(PrCompra)~AlPro+Acond+DoBa+CXXI+MCC+ProfC+Age+Type+Garage+Bedrooms, data = train, method = "backward",nvmax = 10)
results <- summary(ld_bkwd)
plot(ld_orgnl_bkwd,scale="adjr2",col = col)
title(main= "Best subsets Backward selection plot for Price")
par(mar = c(1, 1, 1, 1))
plot(ld_bkwd,scale="adjr2",col = col)
title(main= "Best subsets Backward selection plot for Log Price")

#Subset Forward Plot BICF
par(mar = c(1, 1, 1, 1))
tibble(predictors = 1:10,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  ggtitle("Plot BIC & Cp for PrCompra") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")

#Subset Backward Plot BICB
par(mar = c(1, 1, 1, 1))
tibble(predictors = 1:10,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  ggtitle("Plot BIC & Cp for log(PrCompra)") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")

#Subset Results
which.min(results$cp)

#Plotting using models and required number of variables
coef(ld_model,10)
coef(ld_frwd,10)
coef(ld_bkwd,10)

#Cross Validation with test data
cross_test <- model.matrix(log(PrCompra) ~AlPro+Acond+DoBa+CXXI+MCC+ProfC+Age+Type+Garage+Bedrooms, data = test)

validation_errors <- vector("double", length = 10)

val_error <- function(myModel){
  for(i in 1:10) {
    coef_x <- coef(myModel, id = i) # extract coefficients for model size i
    pred_x <- cross_test[ , names(coef_x)] %*% coef_x 
    validation_errors[i] <- mean((test$PrCompra - pred_x)^2)
  }
  plot(validation_errors, type = "b",col = col)
}

#Cross Validation Forward
par(mar = c(1, 1, 1, 1))
val_error(myModel = ld_orgnl_frwd)
title(main = "CV for PrCompra forward best subset selection")

#Cross Validation Backward
par(mar = c(1, 1, 1, 1))
val_error(myModel = ld_orgnl_bkwd)
title(main = "CV for PrCompra backward best subset selection")

#Cross Validation Forward Log
par(mar = c(1, 1, 1, 1))
val_error(myModel = ld_frwd)
title(main = "CV for Log PrCompra forward best subset selection")

#Cross Validation Backward Log
par(mar = c(1, 1, 1, 1))
val_error(myModel = ld_orgnl_bkwd)
title(main = "CV for log PrCompra backward best subset selection")

#Final Model
fin_mod <- lm(PrCompra~MCC+Bedrooms+Type+DoBa+Garage+AlPro+Acond+Age+ProfC,data=Project)
summary(fin_mod)

#Geospatial Analysis
LonDis <- readOGR(dsn="London",layer="London",stringsAsFactors=FALSE) # Loading London districts data
LonHe <- SpatialPointsDataFrame(Project[,1:2],Project) # Creating SPDF
proj4string(LonHe) <- CRS(proj4string(LonDis)) # Copy CRS (Coordinate Reference System)                       
LonHeLonDis <- over(LonHe,LonDis) # Spatial joining points and polygons
#dim(LonHeLonDis)
#head(LonHeLonDis)
Project$district <- gsub("London District","",LonHeLonDis$NAME) # Add district names only to data

districts <- names(table(Project$district)) # Districts names
NB <- length(districts)

MapLon <- function(Var,nClass=9,dp=0,plotNames=FALSE){
  require(classInt)
  require(RColorBrewer)
  Classes <- classIntervals(Var,nClass,method="quantile",dataPrecision=dp)
  Palette <- brewer.pal(nClass,"Blues")
  Colours <- findColours(Classes,Palette)
  par(mar = c(1, 1, 1, 1))
  plot(LonDis,col=Colours)
  legend("bottomright",
         legend=names(attr(Colours,"table")),
         fill=attr(Colours,"palette"),
         cex=0.75,bty="n")
  box()
  if(plotNames) {
    xy <- coordinates(LonDis)
    text(xy[,1],xy[,2],DisName,col="black",cex=0.5)
  }
}

DisName <- gsub(" London Districts ","",LonDis$NAME)
xy <- coordinates(LonDis)
par(mar = c(1, 1, 1, 1))
plot(LonDis)
text(xy[,1],xy[,2],DisName,col="blue",cex=0.5)

data.frame(DisName,LonDis$NAME)                   
head(Project)                                
NB <- length(LonDis)                            
results <- matrix(0,NB,2)                   
for(i in 1:NB) {
  m.x <- lm(PrCompra~MCC,data=Project[Project$district == DisName[i],])
  results[i,] <- coef(m.x)
}

rownames(results) <- DisName                  
colnames(results) <- c("Intercept","FlorArea")

#Log(Price) by District
par(mar = c(1, 1, 1, 1))
boxplot(log(PrCompra)~district,data=Project,outpch=16,outcol="steelblue",outcex=0.75,xaxt="n",xlab = "",ylab = "Log(Price)")
axis(1,labels=districts,at=1:NB,cex.axis=0.75,las=2)
title("Log(Price) by District")

#Mapping Log(Price) by District
par(mar = c(1, 1, 1, 1))
MapLon(tapply(Project$PrCompra,Project$district,median),dp=3)
title("Log(Price) by District")

#FloorArea by District
par(mar = c(1, 1, 1, 1))
boxplot(MCC~district,data=Project,outpch=16,outcol="steelblue",outcex=0.75,xaxt="n",xlab = "",ylab = "MCC")
axis(1,labels=districts,at=1:NB,cex.axis=0.75,las=2)
title("Floor Area by District")

#Mapping FloorArea by District
par(mar = c(1, 1, 1, 1))
MapLon(results[,2]) # without District names
title("Floor Area by District")

#Standardized Residuals
fin_mod <- lm(PrCompra~MCC+Bedrooms+Type+DoBa+Garage+AlPro+Acond+Age+ProfC,data=Project)
summary(fin_mod)
Project$stdres <- stdres(fin_mod)

#Standardized Residuals Plot
par(mar = c(1, 1, 1, 1))
boxplot(stdres~district,data=Project,outpch=16,outcol="steelblue",outcex=0.75,xaxt="n",xlab = "",ylab = "stdres")
axis(1,labels=districts,at=1:NB,cex.axis=0.75,las=2)
title("Standardized Residual by district")

#Standardized Residuals Map
MapLon(tapply(Project$stdres,Project$district,median),dp=3)
title("Standardized Residual by district")

