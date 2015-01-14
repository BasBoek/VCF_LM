# Team Bastei
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
names(Gewata) <- c('band1', 'band2', 'band3', 'band4', 'band5', 'band7', 'VCF')

# Histograms of Gewata show that in almost all bands outliers are present
hist(Gewata)
dev.off()
# Therefore, all values higher or lower than the mean - 5*sd are set to NA:
mean <- cellStats(x = Gewata, stat = 'mean')
sd5 <- 5*(cellStats(x = Gewata, stat = 'sd'))
lowerbound <- mean - sd5
upperbound <- mean + sd5
Gewata[Gewata > upperbound] <- NA
Gewata[Gewata < lowerbound] <- NA

#New histograms
hist(Gewata)
dev.off()
hist(Gewata$VCF)

# plots that demonstrate the relationship between the Landsat bands and the VCF tree cover.
plot(Gewata[[1:6]], Gewata[[7]], maxpixels = 10000)

# we can conclude from the plots that band 4 does not seem to explain much of the variance in VCF, while the other bands seem to have a negative relationship with VCF.


# First, we creata a dataframe without rows containing NA values
GewataDF <- as.data.frame(Gewata)
GewataDF <- na.omit(GewataDF)

# Then we run the multilinear regression model...
modelLM <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data = GewataDF) 
summary(modelLM)
modelLM # model coefficients

# Make new VCF, based on prediction modelLM
covs <- Gewata[[1:6]]
VCFpred <- predict(covs, model = modelLM, na.rm = T)

# Plot 'old' VCF and predicted VCF. MAKE LEGEND THE SAME
opar <- par(mfrow=c(1, 2))
plot(Gewata[[7]], zlim = c(0,100))
plot(VCFpred, zlim = c(0,100))
par(opar)

# Calculate differences between original VCF and predicted VCF and RMSE
VCFres <- VCFpred - Gewata[[7]]
plot(VCFres, main = 'Residuals predicted - original')

VCFresDF <- as.data.frame(VCFres)
VCFresDF <- na.omit(VCFresDF)
RMSE <- (mean(VCFresDF**2))**0.5
RMSE  # Tadaah!

############################## CREATE TRAINING DATA ###################################

load("data/trainingPoly.rda")

# we can convert to integer by using the as.numeric() function, 
# which takes the factor levels
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, Gewata, field='Code')
# define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue")
# plot without a legend
plot(classes, col=cols, legend=FALSE)
# add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")

####################### END OF CREATION TRAINING DATA #################################

# looking at RMSE's of different classes
RS_VCFres <- (VCFres**2)
RMSEclass <- zonal(RS_VCFres, classes, fun='mean', digits=0, na.rm=TRUE)
RMSEclass[,2] <- RMSEclass[,2]**0.5
RMSEclass

## CONLCUSION: RMSE indicates that modelLM predicts VCF with the highest precision/accuracy for class 'forest'. Wetland is worst predicted of the three classes based on the same model.

