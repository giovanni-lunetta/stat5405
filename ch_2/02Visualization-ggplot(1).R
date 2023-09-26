# Chapter 2: Visualization
setwd("C:/Users/nra02001/Dropbox/STAT5405/Teaching")

# Load libraries:
library(ggplot2)
library(gridExtra)

# trees data
trees <- read.csv('Data/trees.csv', header=TRUE)
str(trees)

##########################################################
# Histogram, kernel density plot, normal density plot 
##########################################################

# Using base R:
hist(trees$Height, main=NA, breaks=8, col='lightgreen'
     , prob=T, border="white", xlab="height",right = FALSE)
lines(density(trees$Height), col="blue", lwd=2)
x <- seq(min(trees$Height), max(trees$Height), 0.01)
curve(dnorm(x, mean=mean(trees$Height), sd=sd(trees$Height)), add=T, col="red", lty=2, lwd=2)
legend("topleft", c("Kernel Density", "Normal"), col=c("blue", "red"), lty=c(1,2), cex=0.8, bty = "n")

# Using ggplot2:
ggplot(trees, aes(x = Height)) + 
  stat_bin(breaks = seq(60, 90, by = 5),aes(y=after_stat(density)), colour = "black"
           ,fill = "lightgreen",closed = "left") + 
  scale_x_continuous(breaks = seq(60, 90, by = 5)) + 
  geom_density(aes(color="Kernel Density"),size=1) + # Adding kernel density
  geom_function(fun = dnorm, args = list(mean = mean(trees$Height), sd = sd(trees$Height))
                     ,aes(color="Normal"),linetype="dashed",size=1) + # Adding normal density plot
  scale_color_manual(name="",values=c("blue","red"))

#########################
# Stem and leaf plot:
########################

# Using Base R:
stem(trees$Volume)

# Using aplpack:
library(aplpack)
stem.leaf(trees$Volume,unit = 0.1)

#################
# Scatterplot:
#################

# Using base R:
with(trees, {
  plot(Girth, Volume, main=NA, xlab= "Girth", ylab="Volume", pch=20, col=2, cex=1.5) 
  plot(Height, Volume, main=NA, xlab= "Height", ylab="Volume", pch=23, col=4) 
})

# Using ggplot2:
s1 <- ggplot(trees, aes(x=Girth, y=Volume)) + geom_point(shape=15,col="darkred")
s2 <- ggplot(trees, aes(x=Height, y=Volume)) + geom_point(shape=2,color="darkblue")

grid.arrange(s1,s2,nrow=1)

######################
# Box plot:
######################

# tensile strength data
tensile <- read.csv('Data/tensile.csv', header=TRUE)
str(tensile$CWP) # int [1:25] 20 30 20 35 30 15 25 20 25 30 ...
tensile$CWP <- as.factor(tensile$CWP)
str(tensile$CWP) # Factor w/ 5 levels "15","20","25",..: 2 4 2 5 4 1 3 2 3 4 ...

# Using base R:
par(mfrow=c(1,2))
boxplot(tensile$strength, xlab="cotton weight percentage", ylab="strength", border = "blue", col="lightblue")
boxplot(tensile$strength ~ tensile$CWP, xlab="cotton weight percentage", ylab="strength", border = "blue", col="lightblue")

# Using ggplot2:
p1 <- ggplot(data = tensile,mapping = aes(y=strength,x=0)) +
  geom_boxplot(col="blue",fill="lightblue",width=0.1) + 
  xlab("cotton weight percentage") + 
  ylab("strength") + theme_light()

p2 <- ggplot(data = tensile,mapping = aes(y=strength,x=CWP)) +
  geom_boxplot(col="blue",fill="lightblue") + 
  xlab("cotton weight percentage") + 
  ylab("strength") + theme_light()

# Arrange the plots:
grid.arrange(p1, p2, nrow = 1)

##################
# Dot plot:
##################
# crabs data
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

# Using ggplot2:
tab <- table(crabs$color,crabs$spine)
df1 <- data.frame(tab)
colnames(df1) <- c("Color","Spine","Count")
p <-ggplot(df1, aes(x=Count, y=Spine,fill=Color)) +
  geom_dotplot(binaxis='y', stackdir='center') + 
  scale_fill_manual(name=c("light","medium","dark","darker")
                    ,values=c("antiquewhite","coral2","cyan4","black"))
p

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

################
# Pie diagram
###############

# Using base R
with(crabs, {
  spinetable <- table(spine)
  pie(spinetable, labels = rownames(spinetable), col=rainbow(length(spinetable)), main="A")
})

# Using ggplot2:
spinetable <- data.frame(table(crabs$spine))
colnames(spinetable) <- c("Spine","Count")
ggplot(spinetable, aes(x = "", y = Count, fill = Spine)) +
  geom_col() +
  coord_polar(theta = "y")

##############
# Bar plot
##############

# Using base R:
with(crabs, {
  spinetable <- table(spine)
  barplot(spinetable, beside = F, col=c("red","yellow","orange"), ylab = "Counts", main ="B")
})

spinetable <- data.frame(table(crabs$spine))
colnames(spinetable) <- c("Spine","Count")

# Using ggplot2
p<-ggplot(data=spinetable, aes(x=Count, y=Spine)) +
  geom_bar(stat="identity",fill="steelblue") + coord_flip()
p

####################
# spine plot
####################

# birdkeeping data
birds <- read.csv('Data/birds.csv', header=TRUE)
birds$LC <- relevel(as.factor(birds$LC), ref = "NoCancer")
str(birds)

# Using base R:
with(birds, {
  y <- table(LC, BK)
  spineplot(y, col = c("pink","orange"), main=NA)
})

# Using ggplot2:
library(ggmosaic)
ggplot(data = birds) +
  geom_mosaic(aes(x = product(BK, LC), fill=BK)) + labs(x = "LC",
                                                      y = "BK") 

# CTtowns data
CTtowns <- read.csv('Data/CTtowns2019.csv', header=TRUE)
library("car")
par(mfrow=c(1,2))
qqPlot(CTtowns$Value, main="A", ylab="Households", cex=0.6,pch=19, col="red", col.lines = "orange")
qqPlot(log10(CTtowns$Value), main="B", ylab="log10(Households)", cex=0.6,pch=19, col="green", col.lines = "lightblue")

# Using ggplot2:
p1 <- ggplot(CTtowns, aes(sample=Value))+stat_qq(col="orange")+
  stat_qq_line()+theme_light() + labs(y="Households",x = "Normal quantiles")+
  ggtitle("A")
p2 <- ggplot(CTtowns, aes(sample=log10(Value)))+
  stat_qq(col="green")+stat_qq_line()+theme_light()+
  labs(y="log10(Households)",x = "Normal quantiles")+
  ggtitle("B")

library(gridExtra)
grid.arrange(p1,p2,nrow=1)

# goodness of fit for CT towns data
library(stats)
shapiro.test(log(CTtowns$Value))
ks.test(log(CTtowns$Value), pnorm, mean(log(CTtowns$Value)),sd(log(CTtowns$Value)))
library(nortest)
ad.test(log(CTtowns$Value))
cvm.test(log(CTtowns$Value))
pearson.test(log(CTtowns$Value)) 
