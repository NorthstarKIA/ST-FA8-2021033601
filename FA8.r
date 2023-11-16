# Installation of Packages
install.packages("ggpubr")
install.packages("dplyr")

# Data Import
my_data <- read.csv(file.choose())
my_data <- PlantGrowth

# Data checking
set.seed(1234)
dplyr::sample_n(my_data, 10)

# Data leveling
levels(my_data$group)

library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# Plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 

# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)

# Summary of the analysis
summary(res.aov)

# Tukey's Test for multiple comparisons
TukeyHSD(res.aov, conf.level=.95) 
tukey.test <- TukeyHSD(res.aov, conf.level=.95) 
tukey.test
plot(tukey.test)


pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

# Homogeneity of variances
plot(res.aov, 1)

#Levene's Test
library(car)
leveneTest(weight ~ group, data = my_data)

#ANOVA Test
oneway.test(weight ~ group, data = my_data)

#Pairwise Test
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)

#Normality
plot(res.aov, 2)

# Residuals
aov_residuals <- residuals(object = res.aov )

# Shapiro-Wilk Test
shapiro.test(x = aov_residuals )

#Kruskal's Test
kruskal.test(weight ~ group, data = my_data)

#Dataset and Problem
#Plant growth analysis often uses data from consecutive damaging harvests that are carried out during the plant life cycle to compute growth rates. There are two primary methods for estimating growth rates: the classical method uses data from two consecutive harvests to calculate the mean values of growth rates using previously developed formulas. The functional method, on the other hand, fits mathematical functions throughout the growth data over time and differentiates the results to provide instantaneous growth rate values.
#Assumption I. You have one dependent variable that is measured at the continuous level.
  #The dependent variable being weight is at a continuous level.
#Assumption II. You have one independent variable that consists of two categorical, independent groups.
  #The categories of the independent variable are ctrl, trt1, and trt2
#Assumption III. You should have independence of observations.
  #Each of the given observations are independent from each other.
#Assumption IV.There should be no significant outliers in the three or more groups of your independent variable in terms of the dependent variable.
  #The outliers have been eliminated from the data, being 17, 15, and 4
#Assumption V. Your dependent variable should be approximately normally distributed for each group of the independent variable.
  #It can be seen that the variables are p > 0.5, as assessed by the Shapiro-Wilk normality test.
#Assumption VI. You have homogeneity of variances (i.e., the variance of the dependent variable is equal in each group of your independent variable).
  #According to the Levene's test, p = 0.3412, meaning there was homogeneity of variances of the dependent variables.

#Reporting
#A one-way ANOVA was conducted to see the growth of plants using the data from different criteria, such as color, weight, and different kinds of treatment. 
#The plants were separated into three levels of treatment, first being the control group, tr1, then trt2.
#The outliers (17, 15, and 4) were eliminated as they were affecting the normality, and homogeneity of variance; now that the outliers are eliminated, homogeneity of variances is achieved.
#As Shapiro-Wilk Test shows (p = 0.4379); and Levene's Test shows (p = 0.3412), homogeneity of variances is proved.
#Through the different types of treatment, it can be seen that the comparison between each other had significant differences. 
#trt1-ctrl saw a -0.371 difference in mean, 0.391 is the p adjusted
#trt2-ctrl saw a 0.494 difference in mean, 0.198 is the p adjusted
#trt2-trt1 saw a 0.865 difference in mean, 0.012 is the p adjusted
#Showing that the only significant adjusted value of p is only in between trt2 and trt1.



#
