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
gewata <- stack(GewataB1,GewataB2,GewataB3,GewataB4,GewataB5,GewataB7, vcfGewata)

# remove outliers from vcf
vcfGewata[vcfGewata > 100] <- NA

# plots that demonstrate the relationship between the Landsat bands and the VCF tree cover.
pairs(gewata)
plot(gewata, vcfGewata)

'' we can conclude from the plots that blaba