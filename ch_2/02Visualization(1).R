### Chapter 2: Visualization

setwd("C:/Users/nra02001/Dropbox/STAT5405/Teaching")
## trees data
trees <- read.csv('Data/trees.csv', header=TRUE)
str(trees)

# Histogram, kernel density plot, normal density plot 
hist(trees$Height, main=NA, breaks=8, col='lightgreen', prob=T, border="white", xlab="height")
lines(density(trees$Height), col="blue", lwd=2) #kernel density plot
# normal density plot
x <- seq(min(trees$Height), max(trees$Height), 0.01)
curve(dnorm(x, mean=mean(trees$Height), sd=sd(trees$Height)), add=T, col="red", lty=2, lwd=2)
legend("topleft", c("Kernel Density", "Normal"), col=c("blue", "red"), lty=c(1,2), cex=0.8, bty = "n")

# Stem and leaf plot: scale controls the length of the plot
sort(trees$Volume)
stem(trees$Volume) 
stem(trees$Volume,scale = 1)  # default
stem(trees$Volume,scale = 2) 

# or use package aplpack
#install.packages("aplpack")
library(aplpack)
stem.leaf(trees$Volume,unit = 1) 
stem.leaf(trees$Volume,unit = 0.1) 

# More clear
sort(trees$Girth)
stem(trees$Girth, scale=2)
stem.leaf(trees$Girth, unit=0.1) # may be most meaningful




# Scatterplot
with(trees, {
  plot(Girth, Volume, main=NA, xlab= "Girth", ylab="Volume", pch=20, col=2, cex=1.5) 
  plot(Height, Volume, main=NA, xlab= "Height", ylab="Volume", pch=23, col=4) 
})


## tensile strength data
tensile <- read.csv('Data/tensile.csv', header=TRUE)
str(tensile$CWP) # int [1:25] 20 30 20 35 30 15 25 20 25 30 ...
tensile$CWP <- as.factor(tensile$CWP)
str(tensile$CWP) # Factor w/ 5 levels "15","20","25",..: 2 4 2 5 4 1 3 2 3 4 ...
par(mfrow=c(1,2))
boxplot(tensile$strength, xlab="cotton weight percentage", ylab="strength", border = "blue", col="lightblue")
boxplot(tensile$strength ~ tensile$CWP, xlab="cotton weight percentage", ylab="strength", border = "blue", col="lightblue")


## crabs data
crabs <- read.csv('Data/crabs.csv', header=TRUE)
# show the levels of all the variables in the crabs dataset
sapply(crabs, levels)
levels(crabs$color)
# Change the order of color
crabs$color <- factor(crabs$color, levels=c("light",  "medium", "dark", "darker"))
# dotplot
with(crabs, {
  y <- table(color, spine)
  dotchart(y, color = c("grey60","grey45","grey30","black"), pch=17, main=NA, xlab="Counts")
})
# Empirical Q-Q plot
with(crabs, {
  par(mfrow=c(1,2))
  qqplot(width[spine=="good"], width[spine=="bad"], plot.it = TRUE, xlab= "good spine condition", ylab="bad spine condition", main="Empirical Q-Q plot", pch=19, col=3, axes=F )
  axis(1); axis(2); grid()
  plot(ecdf(width[spine=="good"]), col=4, main="Empirical c.d.f.s")
  lines(ecdf(width[spine=="bad"]), col=2, pch=17)
  legend("bottomright",c("good","bad"),col=c(4,2), pch=c(19,17),bty="n")
  par(mfrow=c(1,1))
})
# Pie diagram
with(crabs, {
  spinetable <- table(spine)
  pie(spinetable, labels = rownames(spinetable), col=rainbow(length(spinetable)), main="A")
})
# Bar plot
with(crabs, {
  spinetable <- table(spine)
  barplot(spinetable, beside = F, col=c("red","yellow","orange"), ylab = "Counts", main ="B")
})


## birdkeeping data
birds <- read.csv('Data/birds.csv', header=TRUE)
birds$LC <- relevel(as.factor(birds$LC), ref = "NoCancer")
str(birds)
# spine plot
with(birds, {
  y <- table(LC, BK)
  spineplot(y, col = c("pink","orange"), main=NA)
})


## CTtowns data
CTtowns <- read.csv('Data/CTtowns2019.csv', header=TRUE)
library("car")
par(mfrow=c(1,2))
qqPlot(CTtowns$Value, main="A", ylab="Households", cex=0.6,pch=19, col="red", col.lines = "orange")
qqPlot(log10(CTtowns$Value), main="B", ylab="log10(Households)", cex=0.6,pch=19, col="green", col.lines = "lightblue")
par(mfrow=c(1,1))
# goodness of fit for CT towns data
library(stats)
shapiro.test(log(CTtowns$Value))
ks.test(log(CTtowns$Value), pnorm, mean(log(CTtowns$Value)),sd(log(CTtowns$Value)))
library(nortest)
ad.test(log(CTtowns$Value))
cvm.test(log(CTtowns$Value))
pearson.test(log(CTtowns$Value)) 

