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
modelLM # shows model coefficients

# EXTRA: RMSE calculation (of the multilinear regression model itself) (Bonus??)
res <- modelLM$residuals
RMSE <- mean((res**2)**0.5)
RMSE # print RMSE

# Make new VCF, based on prediction modelLM
covs <- Gewata[[1:6]]
names(covs)
VCFpred <- predict(covs, model = modelLM, na.rm = T)

# Plot 'old' VCF and predicted VCF. MAKE LEGEND THE SAME
opar <- par(mfrow=c(1, 2))
plot(Gewata[[7]])
plot(VCFpred)
par(opar)

# legendVCF <- legend('topright', legend = c(1:100), col=rev( rainbow(99, start=0,end=100)))
# seq(from = 0, to = 100, by = 10)

VCFres <- VCFpred - Gewata[[7]]
plot(VCFres)

VCFresDF <- as.data.frame(VCFres)
VCFresDF <- na.omit(VCFresDF)
RMSE <- mean((VCFresDF**2)**0.5)
RMSE

