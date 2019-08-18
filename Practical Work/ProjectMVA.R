#Libraries
library(mlbench)
library(dplyr)
library(missForest)
library(DMwR)
library(mice)
library(BaylorEdPsych)
library(mvnmle)
library(e1071)
library(Hmisc)
library(corrplot)
library(cowplot)
library(VIM)
library(ggplot2)
library(gridExtra) 
library(ROCR)
library(plyr) 
library(class) 
library(tree) 
library(randomForest)
library(chemometrics)
library(FactoMineR)
library(factoextra)
library(rpart.plot)
library(funModeling)
library(fpc)
library(cluster)
library(partykit)
library(caret)
library(ISLR)
library(ROCR)
library(rpart)
library(rattle)
library(caret)
# 1. symboling: -3, -2, -1, 0, 1, 2, 3. 
#Cars are initially assigned a risk factor symbol associated with its price. 
#Then, if it is more risky (or less), this symbol is adjusted by moving it up (or down) the scale. 
#Actuarians call this process "symboling". A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe. 
# 2. normalized-losses: continuous from 65 to 256. 
# 3. make: Name of car company
#   (alfa-romero, audi, bmw, chevrolet, dodge, honda, 
# isuzu, jaguar, mazda, mercedes-benz, mercury, 
# mitsubishi, nissan, peugot, plymouth, porsche, 
# renault, saab, subaru, toyota, volkswagen, volvo). 
# 4. fuel-type: Car fuel type (diesel, gas). 
# 5. aspiration: Aspiration used in a car (std, turbo). 
# 6. num-of-doors: Number of doors in a car (four, two). 
# 7. body-style: Body of car (hardtop, wagon, sedan, hatchback, convertible). 
# 8. drive-wheels: type of drive wheel (4wd, fwd, rwd). 
# 9. engine-location: Location of car engine (front, rear). 
# 10. wheel-base: Distance between wheels of car (continuous from 86.6 120.9). distance between wheels of car.
# 11. length: Length of Car (continuous from 141.1 to 208.1). 
# 12. width: Width of Car (continuous from 60.3 to 72.3.) 
# 13. height: Height of Car (continuous from 47.8 to 59.8). 
# 14. curb-weight: the weight of a car without occupants or baggage. (continuous from 1488 to 4066). 
# 15. engine-type: Type of engine (dohc, dohcv, l, ohc, ohcf, ohcv, rotor). 
# 16. num-of-cylinders: Number of cylinders placed in the car (eight, five, four, six, three, twelve, two). 
# 17. engine-size: Size of engine (continuous from 61 to 326.) 
# 18. fuel-system: 1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi. 
# bbl - BBL means the amount of holes that air enters the engine 
# the technical name is BARRELS! When you take off the air filter there 
# will be either 1 hole or 2 holes side by side or 4 holes 
# 2 in the front and 2 in the rear. Hence the values 1bbl, 2bbl, 4bbl
# idi - Indirect injection. Fuel is not directly injected into the 
# combustion chamber
# port injection
# spfi - Sequential Port fuel injection
# mpfi - Multipoint/port Fuel Injection
# 19. bore: (continuous from 2.54 to 3.94.) Bore is the diameter of each cylinder
# 20. stroke: (continuous from 2.07 to 4.17.) Stroke is the length that it travels when moving from bottom position to the top position.
# 21. compression-ratio: (continuous from 7 to 23.) the ratio of the maximum to minimum volume in the cylinder of an internal combustion engine. 
# 22. horsepower: (continuous from 48 to 288.) The power an engine produces is called horsepower. 
# 23. peak-rpm: (continuous from 4150 to 6600.) 
# RPM stands for revolutions per minute, and it's used as a measure of how fast any machine is operating at a given time. 
# In cars, rpm measures how many times the engine's crankshaft makes one full rotation every minute, 
# and along with it, how many times each piston goes up and down in its cylinder.
# 24. city-mpg: Mileage in city (continuous from 13 to 49.) 
# 25. highway-mpg: Mileage on highway (continuous from 16 to 54.)
# 26. price: Price of car (continuous from 5118 to 45400.)

#Data Reading
carData = read.csv("C:/Users/Henry/Desktop/MASTER/2n Semester/MVA/Project/Automobile_data.csv", 
                   header = TRUE, sep = ",", quote = "\"",
                   dec = ".", fill = TRUE)

colnames(carData) <-  
    c("Symboling","NormalizedLosses","Companies", "FuelType" , "Aspiration", "NumberOfDoors",
    "BodyStyleOfCar", "DriveWheel", "EngineLocation", "WheelBase", "LengthOfCar", 
    "WidthOfCar", "HeightOfCar", "CurbWeight", "EngineType", "CylinderNumber" , 
    "EngineSize", "FuelSystem" , "Bore", "Stroke", "CompressionRatio", 
    "Horsepower", "PeakRPM" , "CityMPG", "HighwayMPG", "Price" )

#Data Understanding
head(carData, n=5) # display first 10 rows of data
for (i in 1:ncol(carData)) { #Missing Values was representing as ? and it was converted as NA
  if(length(which(carData[,i] == "?"))>0) carData[which(carData[,i] == "?"),i] = NA
}
str(carData) #How many columns and rows do we have ? What are the data types of attributes ?
summary(carData) #Summarize the dataset
#Remove columns 'symboling' & 'normalized losses'
carData <- dplyr::select(carData, -Symboling, -NormalizedLosses)
#Data Transformation
carData = carData %>% mutate(Bore = as.numeric(as.character(Bore)),
                             Stroke = as.numeric(as.character(Stroke)),
                             Horsepower = as.integer(as.character(Horsepower)),
                             PeakRPM = as.integer(as.character(PeakRPM)),
                             Price = as.integer(as.character(Price)))
#We created new data frame from matrix which include only numeric columns
carMatrixNum = cbind(carData$WheelBase,carData$LengthOfCar,
                     carData$WidthOfCar,carData$HeightOfCar,carData$CurbWeight,
                     carData$EngineSize,carData$Bore,carData$Stroke,carData$CompressionRatio,
                     carData$Horsepower,carData$PeakRPM,carData$CityMPG,
                     carData$HighwayMPG,carData$Price)
colnames(carMatrixNum) = c("WheelBase","LengthOfCar",
                           "WidthOfCar","HeightOfCar","CurbWeight",
                           "EngineSize","Bore","Stroke","CompressionRatio","Horsepower",
                           "PeakRPM","CityMPG","HighwayMPG","Price")
carDataNum = data.frame(carMatrixNum)
#Data Preprocessing
#Missing Value Detection
apply(carData, 2, function(x) sum(is.na(x))) #Missing Value Detection
md.pattern(carDataNum) #Missing Value Visualization
aggr_plot <- aggr(carDataNum, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(carDataNum), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern")) #Missing Value Visualization
#Missing Value Imputation
carDataNum = missForest(carDataNum)
carDataNum = carDataNum$ximp
carDataNum$Horsepower = round(carDataNum$Horsepower)
carDataNum$PeakRPM = round(carDataNum$PeakRPM)
carDataNum$Price = round(carDataNum$Price)
carData$Horsepower = carDataNum$Horsepower
carData$PeakRPM = carDataNum$PeakRPM
carData$Bore = round(carDataNum$Bore, 2)
carData$Stroke = round(carDataNum$Stroke, 2)
carData$Price = carDataNum$Price
cd = carData
dataFac = cd %>% mutate_if(is.character, as.factor)
mfImp = missForest(dataFac);
carDataImp <- mfImp$ximp
sum(is.na(carDataImp))
carData$NumberOfDoors = carDataImp$NumberOfDoors
sum(is.na(carData))
#Outlier Detection
# Setting the theme of plots
plot_theme = theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14,face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12))
#Continuous Univariate plots 
CUV = function(yfeature, ylabel) {
  ggplot(carDataNum, aes(x = "", y = yfeature)) +
    geom_boxplot(fill = "#0000FF", outlier.colour = "red", outlier.shape = 1) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs( y = ylabel, title = paste(ylabel, "Distribution")) +
    plot_theme}

p1 = CUV(carDataNum$WheelBase, "Wheel Base" )
p2 = CUV(carDataNum$LengthOfCar, "Length of Car" )
p3 = CUV(carDataNum$WidthOfCar, "Width of Car")
p4 = CUV(carDataNum$HeightOfCar, "Height of Car" )
p5 = CUV(carDataNum$CurbWeight, "Curb Weight")
p6 = CUV(carDataNum$EngineSize, "Engine Size")
p7 = CUV(carDataNum$Bore, "Bore")
p8 = CUV(carDataNum$Stroke, "Stroke")
p9 = CUV(carDataNum$CompressionRatio, "Compression Ratio")
p10 = CUV(carDataNum$Horsepower, "Horse Power")
p11 = CUV(carDataNum$PeakRPM, "Peak RPM")
p12 = CUV(carDataNum$CityMPG, "City MPG")
p13 = CUV(carDataNum$HighwayMPG, "Highway MPG")
p14 = CUV(carDataNum$Price, "Price")

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9 ,p10, p11, p12, p13, p14)

#Multivariate Outlier Detection by Maholanobis (Robust) Distance
MDoutliers <- Moutlier(carDataNum, quantile = 0.975, plot = TRUE)
#5 top most ranking outliers for MRD
MRD_index_ordered <- order(MDoutliers$rd, decreasing=T)
round(MDoutliers$rd[MRD_index_ordered][1:4],3)
#Plot
index <- seq(1:length(carDataNum[,1]))
MRD_df <- cbind.data.frame(index, round(MDoutliers$rd,3), rownames(carDataNum))
MRD_plot <- plot(MDoutliers$rd, 
                 pch="o", 
                 cex=1, 
                 main="Potential MRD outliers\n by Mahalanobis robust distance (MRD)",
                 ylab="MRD Rank") 
abline(h = MDoutliers$cutoff, col="red")  # add cutoff line

#Outlier Treatment
oiWheelBase = which(carDataNum$WheelBase %in% boxplot.stats(carDataNum$WheelBase)$out)
oiLength = which(carDataNum$LengthOfCar %in% boxplot.stats(carDataNum$LengthOfCar)$out)
oiWidth = which(carDataNum$WidthOfCar %in% boxplot.stats(carDataNum$WidthOfCar)$out)
oiEngineSize = which(carDataNum$EngineSize %in% boxplot.stats(carDataNum$EngineSize)$out)
oiStroke = which(carDataNum$Stroke %in% boxplot.stats(carDataNum$Stroke)$out)
oiCompressionRatio = which(carDataNum$CompressionRatio %in% boxplot.stats(carDataNum$CompressionRatio)$out)
oiHorsePower = which(carDataNum$Horsepower %in% boxplot.stats(carDataNum$Horsepower)$out)
oiPeakRPM = which(carDataNum$PeakRPM %in% boxplot.stats(carDataNum$PeakRPM)$out)
oiCityMPG = which(carDataNum$CityMPG %in% boxplot.stats(carDataNum$CityMPG)$out)
oiHighwayMPG = which(carDataNum$HighwayMPG %in% boxplot.stats(carDataNum$HighwayMPG)$out)

carDataNum[oiWheelBase, ]$WheelBase = NA
carDataNum[oiLength, ]$LengthOfCar = NA
carDataNum[oiWidth, ]$WidthOfCar = NA
carDataNum[oiEngineSize, ]$EngineSize = NA
carDataNum[oiStroke, ]$Stroke = NA
carDataNum[oiCompressionRatio, ]$CompressionRatio = NA
carDataNum[oiHorsePower, ]$Horsepower = NA
carDataNum[oiPeakRPM, ]$PeakRPM = NA
carDataNum[oiCityMPG, ]$CityMPG = NA
carDataNum[oiHighwayMPG, ]$HighwayMPG = NA

summary(carDataNum)

carDataNum = missForest(carDataNum)
carDataNum = carDataNum$ximp
sum(is.na(carDataNum))

carDataNum$EngineSize = round(carDataNum$EngineSize)
carDataNum$Horsepower = round(carDataNum$Horsepower)
carDataNum$PeakRPM = round(carDataNum$PeakRPM)
carDataNum$CityMPG = round(carDataNum$CityMPG)
carDataNum$HighwayMPG = round(carDataNum$HighwayMPG)

carData$WheelBase = carDataNum$WheelBase
carData$LengthOfCar = carDataNum$LengthOfCar
carData$WidthOfCar = carDataNum$WidthOfCar
carData$EngineSize = carDataNum$EngineSize
carData$Stroke = carDataNum$Stroke
carData$CompressionRatio = carDataNum$CompressionRatio
carData$Horsepower = carDataNum$Horsepower
carData$PeakRPM = carDataNum$PeakRPM
carData$CityMPG = carDataNum$CityMPG
carData$HighwayMPG =carDataNum$HighwayMPGg
#####
#Explatory Data Analysis
#Univariate Analysis  

a1 = CUV(carDataNum$WheelBase, "Wheel Base" )
a2 = CUV(carDataNum$LengthOfCar, "Length of Car" )
a3 = CUV(carDataNum$WidthOfCar, "Width of Car")
a4 = CUV(carDataNum$HeightOfCar, "Height of Car" )
a5 = CUV(carDataNum$CurbWeight, "Curb Weight")
a6 = CUV(carDataNum$EngineSize, "Engine Size")
a7 = CUV(carDataNum$Bore, "Bore")
a8 = CUV(carDataNum$Stroke, "Stroke")
a9 = CUV(carDataNum$CompressionRatio, "Compression Ratio")
a10 = CUV(carDataNum$Horsepower, "Horse Power")
a11 = CUV(carDataNum$PeakRPM, "Peak RPM")
a12 = CUV(carDataNum$CityMPG, "City MPG")
a13 = CUV(carDataNum$HighwayMPG, "Highway MPG")
a14 = CUV(carDataNum$Price, "Price")

plot_grid(a1, a2, a3, a4, a5, a6, a7, a8, a9 ,a10, a11, a12, a13, a14)

#Correlation between numerical variables
correlations <- cor(carDataNum) # calculate a correlation matrix for numeric variables
print(correlations) # display the correlation matrix
corrplot(correlations)




##Multivariate Analysis (Continous Variables)

CONT = function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(carDataNum, aes(x = xfeature, y = yfeature)) +
    geom_point() +
    geom_smooth() +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    plot_theme
}

# Price vs <rest of continuous variables>
c1 = CONT(carDataNum$EngineSize, carDataNum$Price, 
          "Engine Size", "Price")
c2 = CONT(carDataNum$Horsepower, carDataNum$Price, 
          "Horse Power", "Price")
c3 = CONT(carDataNum$WidthOfCar, carDataNum$Price, 
          "Width Of Car", "Price")
c4 = CONT(carDataNum$LengthOfCar, carDataNum$Price, 
          "Length Of Car", "Price")
c5 = CONT(carDataNum$HeightOfCar, carDataNum$Price, 
          "Height Of Car", "Price")
c6 = CONT(carDataNum$CurbWeight, carDataNum$Price, 
          "CurbWeight", "Price")
c7 = CONT(carDataNum$WheelBase, carDataNum$Price, 
          "Wheelbase", "Price")
c8 = CONT(carDataNum$Bore, carDataNum$Price, 
          "Bore", "Price")
c9 = CONT(carDataNum$CityMPG, carDataNum$Price, 
          "CityMPG", "Price")
c10 = CONT(carDataNum$HighwayMPG, carDataNum$Price, 
           "HighwayMPG", "Price")
c11 = CONT(carDataNum$PeakRPM, carDataNum$Price, 
           "PeakRPM", "Price")
c12 = CONT(carDataNum$CompressionRatio, carDataNum$Price, 
           "Compression Ratio", "Price")
c13 = CONT(carDataNum$Stroke, carDataNum$Price, 
           "Stroke", "Price")



plot_grid(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)

#Univariate and Multivariate Analysis (Categorical Variables)  

#Sort categorical variables in descending order
cat.sort <- function(x){reorder(x,x,function(y){-length(y)})} ## Sorting function for categorical variables
cat.var <- which(sapply(carData, is.factor)) ## Find the categorical variables
for (i in cat.var){  ## Apply the sort function on each categorical variable
  carData[,i] <- cat.sort(carData[,i])   
}
attach(carData)

#Bar plots of categorical variables 

c1 <- ggplot(carData, aes(x=Companies)) + ggtitle("Companies") + xlab("Companies") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$Companies)))

plot(c1)

c2 <- ggplot(carData, aes(x=FuelType)) + ggtitle("FuelType") + xlab("FuelType") +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(carData$FuelType))) 

c3 <- ggplot(carData, aes(x=CylinderNumber)) + ggtitle("Cylinder Number") + xlab("Cylinder Number") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$CylinderNumber)))

c4 <- ggplot(carData, aes(x=Aspiration)) + ggtitle("Aspiration") + xlab("Aspiration") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$Aspiration)))

c5 <- ggplot(carData, aes(x=EngineLocation)) + ggtitle("Engine Location") + xlab("Engine Location") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$EngineLocation)))

c6 <- ggplot(carData, aes(x=EngineType)) + ggtitle("Engine Type") + xlab("Engine Type") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$EngineType)))

c7 <- ggplot(carData, aes(x=FuelSystem)) + ggtitle("Fuel System") + xlab("FuelSystem") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$FuelSystem)))

grid.arrange(c2, c3, c4, c5, c6, c7, ncol = 3)

c8 <- ggplot(carData, aes(x=NumberOfDoors)) + ggtitle("NumberOfDoors") + xlab("NumberOfDoors") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$NumberOfDoors)))

c9 <- ggplot(carData, aes(x=BodyStyleOfCar)) + ggtitle("Body Style Of Car") + xlab("Body Style Of Car") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$BodyStyleOfCar)))

c10 <- ggplot(carData, aes(x=DriveWheel)) + ggtitle("Drive Wheel") + xlab("Drive Wheel") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Percentage") + coord_flip() +  
  scale_x_discrete(limits = rev(levels(carData$DriveWheel)))

grid.arrange(c8, c9, c10, ncol = 3)

#Pie charts of categorical variables 

pc1 <- ggplot(carData, aes(x=factor(1), fill=Companies)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Companies")

plot(pc1)

pc2 <- ggplot(carData, aes(x=factor(1), fill=FuelType)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("FuelType") 

pc3 <- ggplot(carData, aes(x=factor(1), fill=CylinderNumber)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Cylinder Number")

pc4 <- ggplot(carData, aes(x=factor(1), fill=Aspiration)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Aspiration") 

pc5 <- ggplot(carData, aes(x=factor(1), fill=EngineLocation)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Engine Location")

pc6 <- ggplot(carData, aes(x=factor(1), fill=EngineType)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("EngineType") 

pc7 <- ggplot(carData, aes(x=factor(1), fill=FuelSystem)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("FuelSystem") 

grid.arrange(pc2, pc3, pc4, pc5, pc6, pc7, ncol = 3)

pc9 <- ggplot(carData, aes(x=factor(1), fill=NumberOfDoors)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Number Of Doors") 

pc10 <- ggplot(carData, aes(x=factor(1), fill=BodyStyleOfCar)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Body Style Of Car") 

pc11 <- ggplot(carData, aes(x=factor(1), fill=DriveWheel)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") +  
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) +  
  xlab("") + ylab("") + ggtitle("Drive Wheel") 

grid.arrange(pc9, pc10, pc11, ncol = 3)

#How does the categorical variables impact price? 

#Annova test
carData_ANNOVA =  aov(carData$Price ~ carData$Companies + carData$FuelType + carData$Aspiration +
                        carData$NumberOfDoors + carData$BodyStyleOfCar + carData$DriveWheel + 
                        carData$EngineLocation + carData$EngineType + carData$FuelSystem)

summary(carData_ANNOVA)

CAT = function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(carData, aes(x = xfeature, y = yfeature, fill = xfeature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = F) + 
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    plot_theme
}

x1 = CAT(carData$BodyStyleOfCar, carData$Price, 
                   "Body Style Of Car", "Price")
x2 = CAT(carData$FuelType, carData$Price,
                   "Fuel-Type",  "Price")
x3 = CAT(carData$NumberOfDoors, carData$Price,
                   "Number Of Doors", "Price")
x4 = CAT(carData$DriveWheel, carData$Price, 
                   "Drive Wheel", "Price")
x5 = CAT(carData$EngineType, carData$Price,
                   "Engine Type", "Price")

plot_grid(x1, x2, x3, x4, x5, ncol = 2)


y1 = CAT(carData$FuelSystem, carData$Price,
                   "Fuel System", "Price")
y2 = CAT(carData$Aspiration, carData$Price,
                   "Aspiration", "Price")
y3 = CAT(carData$EngineLocation, carData$Price,
                   "Engine Location",  "Price")
y4 = CAT(carData$CylinderNumber, carData$Price,
                   "Cylinder Number", "Price")

plot_grid(y1, y2, y3, y4, ncol = 2)

#High prices for : Rear engineloaction 

CAT(carData$Companies, carData$Price, 
             "Companies", "Price")
# High prices for companies: 
# BMW, Jaguar, Mercedes Benz

########
sapply(carDataNum, sd) # Calculate standard deviation for all attributes
skew <- apply(carDataNum, 2, skewness) # calculate skewness for each variable
print(skew) # display skewness, larger/smaller deviations from 0 show more skew
####


carData = subset(carData, select = -c(EngineLocation,Stroke))
carDataNum = subset(carDataNum, select = -c(Stroke))

##Principal Component Analysis
res.pca = PCA(carDataNum, scale.unit = TRUE, ncp = 5, quanti.sup = c(13), graph = TRUE)
print(res.pca)
#Variances of the principal components
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])
newEigenvalues = eigenvalues[ ,1] - mean(eigenvalues[,1])
barplot(newEigenvalues, names.arg=1:length(newEigenvalues), 
        main = "Scree Plot",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
sigDim = eigenvalues[ ,1][newEigenvalues > 0]
numSigDim = length(sigDim)


fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 60))
var <- get_pca_var(res.pca)
var
# var$coord: coordinates of variables to create a scatter plot
# var$cos2: represents the quality of representation for variables on the factor map. 
#It???s calculated as the squared coordinates: var.cos2 = var.coord * var.coord.
# var$contrib: contains the contributions (in percentage) of the variables to the principal components. 
#The contribution of a variable (var) to a given principal component is (in percentage) 
#: (var.cos2 * 100) / (total cos2 of the component).

#Variables factor map : The correlation circle
head(res.pca$var$coord) #Coordinates of variables on the principal components
head(res.pca$var$cos2) #Cos2 : quality of variables on the factor map
#Contributions of the variables to the principal components
#Variable contributions in the determination of a given principal component are (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
head(res.pca$var$contrib)
#Graph of variables
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var="contrib")
#Graph of individuals
head(res.pca$ind$coord) #Coordinates of individuals on the principal components
head(res.pca$ind$cos2) #Cos2 : quality of representation of individuals on the principal components
head(res.pca$ind$contrib) #Contribition of individuals to the princial components
plot(res.pca, choix = "ind")
fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)
#biplot of individuals and variables :
fviz_pca_biplot(res.pca,  geom = "text")
#dimension description
dimdesc(res.pca, axes = 1:3, proba = 0.05)

#CLUSTERING
profiling_num(carDataNum)
carDataScaled <- scale(carDataNum)
as_tibble(carDataScaled)
rownames(carDataScaled)
#Matrix and Visualization
distance <- get_dist(carDataScaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#kmeans application
k2 <- kmeans(carDataScaled, centers = 2, nstart = 25)
str(k2)
#Clusters Visualization
fviz_cluster(k2, data = carDataScaled)
#Visualization with Scatter Plot
carDataScaled %>% as_tibble() %>%
  mutate(clusters = k2$cluster, 
         companies = carData$Companies) %>%
  ggplot(aes(Companies, Price, color = factor(clusters), label = companies)) +
  geom_text()
#Visualization with Scatter Plot without scaling
carData %>% as_tibble() %>%
  mutate(clusters = k2$cluster, 
         companies = carData$Companies) %>%
  ggplot(aes(Companies, Price, color = factor(clusters), label = companies)) +
  geom_text()
# Clusterings with different k values
k2
k3 <- kmeans(carDataScaled , centers = 3, nstart = 25)
k4 <- kmeans(carDataScaled , centers = 4, nstart = 25)
k5 <- kmeans(carDataScaled , centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = carDataScaled) + ggtitle("k=2")
p2 <- fviz_cluster(k3, geom = "point", data = carDataScaled) + ggtitle("k=3")
p3 <- fviz_cluster(k4, geom = "point", data = carDataScaled) + ggtitle("k=4")
p4 <- fviz_cluster(k5, geom = "point", data = carDataScaled) + ggtitle("k=5")
grid.arrange(p1, p2, p3, p4, nrow = 2)
## Identfy for Optimal Cluster Number
### Optimal Cluster Number with Elbow Method 
fviz_nbclust(carDataScaled, kmeans, method = "wss")
### Optimal Cluster Number with Average Silhouette 
fviz_nbclust(carDataScaled, kmeans, method = "silhouette")
### Optimal Cluster Number with Gap Statistic 
#Calculation Gap Statistic 
gap_stat <- clusGap(carDataScaled, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
#Visualization
fviz_gap_stat(gap_stat)
#Final K Means
final <- kmeans(carDataScaled, 3, nstart = 24)
print(final)
#Descriptive statistics according to clusters
carDataNum %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#Data partition

train_index = createDataPartition(carData$Price,
                                  p = .8,
                                  list = FALSE,
                                  times = 1)
head(train_index)
train = carData[train_index, ]
test = carData[-train_index, ]
train_x = train %>% dplyr::select(-Price)
train_y = train$Price
test_x = test %>% dplyr::select(-Price)
test_y = test$Price

#single data set
training = data.frame(train_x, target = train_y)

#MODELLING
#CART
### Build Model
cart_tree <- rpart(Price ~ ., data = carData)

names(cart_tree)

plot(cart_tree, margin = 0.1)
text(cart_tree, cex = 0.6, col = "red")
prp(cart_tree, type = 4)
rpart.plot(cart_tree)
fancyRpartPlot(cart_tree)

#Pruned tree
plotcp(cart_tree)
tree_pruned <- prune(cart_tree, cp = 0.02)

plot(tree_pruned, margin = 0.1)
text(tree_pruned, cex = 0.6, col = "red")
prp(tree_pruned, type = 4)
rpart.plot(tree_pruned)
fancyRpartPlot(tree_pruned)

#Plot the importance of variables in the prediction.
impVar = tree_pruned$variable.importance

varImpPlot <- barplot(impVar,
                      main="Importance of variables",
                      xaxt="n",
                      xlab="Variables",
                      ylab="Importance",
                      las=1)
text(cex=1, x=varImpPlot-.25, y=-10, labels(impVar), xpd=TRUE, srt=35, col = "blue")

#Prediction 

head(predict( tree_pruned))

defaultSummary(data.frame(obs = carData$Price,
                          pred = predict(cart_tree)))

## Model Tuning
ctrl <- trainControl(method = "cv", number = 10)

tune_grid <- data.frame(
  cp = seq(0, 0.05, len = 25)
  
)

cart_tune <- train(Price~.,
                   method = "rpart",
                   trControl = ctrl,
                   tuneGrid = tune_grid,
                   preProc = c("center", "scale"), data = carData)

cart_tune

plot(cart_tune)

cart_tune$bestTune
cart_tune$finalModel