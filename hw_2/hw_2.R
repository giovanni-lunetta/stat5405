library(ggplot2)
library(car)
library(PairedData)
data(PrisonStress)
str(PrisonStress)

# Filtering the data to only include inmates who got sport training
sport_data <- subset(PrisonStress, Group == "Sport")

# Calculate the differences between paired observations
differences <- sport_data$PSSbefore - sport_data$PSSafter

# Create a Q-Q plot with confidence bands
qqPlot(differences, main = "Q-Q Plot of Differences with Confidence Bands")

# Paired t-test
test_result <- t.test(sport_data$PSSbefore, sport_data$PSSafter, paired = TRUE, alternative = "greater")

test_result

# Boxplot for the stress levels at exit for both groups
ggplot(PrisonStress, aes(x=Group, y=PSSafter, fill=Group)) + geom_boxplot(show.legend = F)

# Descriptive statistics for the stress levels at exit for both groups
by(PrisonStress$PSSafter, PrisonStress$Group, summary)

ggplot(PrisonStress, aes(x=Group, y=PSSafter, fill=Group)) + geom_boxplot(show.legend = F)

# For Control Group
qqPlot(PrisonStress$PSSafter[PrisonStress$Group == "Control"], main="Q-Q Plot with Confidence Bands for Control Group")

# For Sport Group
qqPlot(PrisonStress$PSSafter[PrisonStress$Group == "Sport"], main="Q-Q Plot with Confidence Bands for Sport Group")

var_test_result <- var.test(PrisonStress$PSSafter[PrisonStress$Group == "Control"], 
                            PrisonStress$PSSafter[PrisonStress$Group == "Sport"])

var_test_result

result <- t.test(PSSafter ~ Group, data=PrisonStress, var.equal=TRUE)

# Print the results
print(result)

library(effsize)

# Extract data for Control and Sport groups
control_data <- PrisonStress[PrisonStress$Group == "Control",]$PSSafter
sport_data <- PrisonStress[PrisonStress$Group == "Sport",]$PSSafter

# Compute Cohen's d
result <- cohen.d(control_data, sport_data, pooled = TRUE)
print(result)

data(Loblolly)
str(Loblolly)

table_subset <- table(Loblolly$Seed, Loblolly$age %in% c(20, 25))
table_subset

# Extract heights of trees aged 20 and 25 years
height_20 <- Loblolly$height[Loblolly$age == 20]
height_25 <- Loblolly$height[Loblolly$age == 25]

par(mfrow=c(2,2))

# For height_20
hist(height_20, main = "Histogram for Height at Age 20", 
     xlab = "", col = "grey", border = "white")
qqPlot(height_20, main = NA, pch = 19, col = 2, cex = 0.7)

# For height_25
hist(height_25, main = "Histogram for Height at Age 25", 
     xlab = "Height")
qqPlot(height_25)

par(mfrow=c(1,1))

var_test_result <- var.test(height_20, height_25)
var_test_result

# Run paired t-test
paired_t_test_result <- t.test(height_20, height_25, paired = TRUE)
paired_t_test_result

# Boxplot for the heights of trees aged 20 and 25 years
ggplot(Loblolly[Loblolly$age %in% c(20, 25), ], aes(x = as.factor(age), y = height, fill = as.factor(age))) + 
  geom_boxplot(show.legend = F) + 
  labs(x = "Age", y = "Height") +
  ggtitle("Distribution of Heights for Trees Aged 20 and 25 Years")

# Calculate Cohen's d for paired data
d <- cohen.d(height_20, height_25, paired = TRUE)

# View the result
print(d)
