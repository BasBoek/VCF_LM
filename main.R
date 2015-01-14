# Team Bastie
# January 13, 2015

library(raster)

# Loading Landsat files
load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")
Gewata <- stack(GewataB1,GewataB2,GewataB3,GewataB4,GewataB5,GewataB7, vcfGewata)

# Histograms of Gewata show that in almost all bands outliers are present
hist(Gewata)

# Therefore, all values higher or lower than the mean - 5*sd are set to NA:
mean <- cellStats(x = Gewata, stat = 'mean')
sd3 <- 4*(cellStats(x = Gewata, stat = 'sd'))
lowerbound <- mean - sd3
upperbound <- mean + sd3
upperbound
Gewata[Gewata > upperbound] <- NA
Gewata[Gewata < lowerbound] <- NA

#New histograms
hist(Gewata)
hist(Gewata[vcfGewata])
vcfGewata

# plots that demonstrate the relationship between the Landsat bands and the VCF tree cover.
plot(Gewata[[1:6]], Gewata[[7]])

# we can conclude from the plots that band 4 does not seem to explain much of the variance in VCF, while the other bands seem to have a negative relationship with VCF.


# First, we creata a dataframe without rows containing NA values
GewataDF <- as.data.frame(Gewata)
GewataDF <- na.omit(GewataDF)
colnames(GewataDF) = c('band1', 'band2', 'band3', 'band4', 'band5', 'band7', 'VCF')

# Then we run the multilinear regression model...
VCFpred <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = GewataDF) 
summary(VCFpred)
VCFpred

# RMSE calculation
res <- model$residuals
RMSE <- mean((res**2)**0.5)












