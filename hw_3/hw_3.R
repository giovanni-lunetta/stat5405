z_0_025 <- qnorm(0.025)
z_0_025

z_0_975 <- qnorm(0.975)
z_0_975

t_critical <- qt(0.975, df=22)
t_critical

chi_squared <- qchisq(0.95, df=17)
chi_squared

f_value <- qf(0.95, df1=4, df2=18)
f_value

library(vcd)
data(Arthritis)
Arthritis$Improved <- ifelse(Arthritis$Improved %in% c("Some", "Marked"), "Improved", "None")
(A.table <- xtabs( ~ Treatment + Improved, data = Arthritis))
(I.table <- table(Arthritis$Improved))
prop.table(I.table)
prop.table(I.table) * 100
mosaicplot(A.table, color = c("gray", "lightgreen"), main = "Mosaic plot for the modified arthritis data")
pi_1_1 = A.table["Treated", "Improved"] / sum(A.table["Treated", ])
pi_1_2 = A.table["Placebo", "Improved"] / sum(A.table["Placebo", ])
pi_1_1
pi_1_2
test <- prop.test(c(A.table["Treated", "Improved"], A.table["Placebo", "Improved"]), 
                  c(sum(A.table["Treated",]), sum(A.table["Placebo",])))
test$conf.int
test <- prop.test(c(A.table["Treated", "Improved"], A.table["Placebo", "Improved"]), 
                  c(sum(A.table["Treated",]), sum(A.table["Placebo",])))
test

library(vcd)
library(epitools)
data(Arthritis)

Arthritis$Improved <- ifelse(Arthritis$Improved %in% c("Some", "Marked"), "Improved", "None")
table_data <- table(Arthritis$Treatment, Arthritis$Improved)
omega1 <- table_data["Treated", "Improved"] / table_data["Treated", "None"]
omega2 <- table_data["Placebo", "Improved"] / table_data["Placebo", "None"]
print(paste("Omega1 (Treated group):", omega1))
print(paste("Omega2 (Placebo group):", omega2))
odds_ratio_data <- oddsratio(table_data)
print(odds_ratio_data$measure)

library(BSDA)
library(ACSWR)
library(car)
library(gridExtra)
library(ggplot2)

data(tensile)
str(tensile)

ggplot(tensile, aes(x=as.factor(CWP), y=Tensile_Strength, fill=as.factor(CWP))) + 
    geom_boxplot(show.legend = FALSE) +
    labs(x="CWP Levels", y="Tensile Strength") +
    theme_minimal() +
    ggtitle("Boxplots of Tensile Strength by CWP Levels")

summary(tensile$Tensile_Strength)
aggregate(Tensile_Strength ~ CWP, data=tensile, summary)

bartlett_result <- bartlett.test(Tensile_Strength ~ CWP, data=tensile)
print(bartlett_result)

library(lawstat)
levene_result_lawstat <- levene.test(tensile$Tensile_Strength, factor(tensile$CWP))
print(levene_result_lawstat)

anova_model <- aov(Tensile_Strength ~ factor(CWP), data=tensile)
summary(anova_model)

library(car)
aovfits <- fitted(anova_model)
aovres <- residuals(anova_model)
car::qqPlot(aovres, main = "Q-Q Plot of Residuals", pch = 19, col = 2, cex = 0.7)

shapiro.test(aovres)