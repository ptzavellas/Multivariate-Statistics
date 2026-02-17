library(psych)
library(Hmisc)
library(ggplot2)
library(MASS)
library(Morpho)
library(mvtnorm)
library(readxl)
library(klaR)
library(MVN)

## Part A ##

diet=read.csv('///Users//petros//Library//Mobile Documents//com~apple~CloudDocs//MSc Biostatistics//Multivariate Statistics//Assignments//Assignment 1//dietStudy.csv')
deaths=diet$death
diet=diet[,-7]


## 1 ##

## Univariate Descriptive Statistics ##

summary(diet) ## Means/Medians/Mins/Max

IQR=sapply(diet, IQR, na.rm = TRUE)
Sd=round(sapply(diet, sd, na.rm = TRUE),2)## SD of each variable
Cov= round(Sd^2,2) ## Covariance of each Variable

boxplot(diet,
        main = "Weekly Frequency of Food Consumption",
        ylab = "Times per Week",
        col = "grey",
        las = 2)  ## Boxplots of each Variable 

## Multivariate Descriptive Statistics ##

round(cor(diet),2) ## Correlation Matrix 

pairs.panels(diet,
             gap = 0,smooth = F,ellipses = F,
             pch = 21) ## Correlation matrix + Histograms + ScatterPlots



## 2 ##

# Testing correlations

KMO(diet)

cortest.bartlett(diet)
FA.PCA <- principal(diet,nfactors = 2,covar = F,rotate = "none")

# communality
round(rowSums(FA.PCA$loadings^2),3)

## 3 ## 

## fit a FA model with varimax rotation ##
FA.PCA1 <- principal(diet,nfactors = 2,covar = F,rotate ="Varimax" )

## fit a FA model with promax rotation ##
FA.PCA2 <- principal(diet,nfactors = 2,covar = F,rotate ="Promax" )

## fit a FA model with oblimin rotation ##
FA.PCA3 <- principal(diet,nfactors = 2,covar = F,rotate ="oblimin" )



plot(FA.PCA$loadings[,1], 
     FA.PCA$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)
text(FA.PCA$loadings[,1]-0.08, 
     FA.PCA$loadings[,2]+0.08,
     colnames(diet),
     col="blue")
abline(h = 0, v = 0)


plot(FA.PCA1$loadings[,1], 
     FA.PCA1$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax")
abline(h = 0, v = 0)
text(FA.PCA1$loadings[,1]-0.08, 
     FA.PCA1$loadings[,2]+0.08,
     colnames(diet),
     col="blue")
abline(h = 0, v = 0)

plot(FA.PCA2$loadings[,1], 
     FA.PCA2$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax")
abline(h = 0, v = 0)
text(FA.PCA2$loadings[,1]-0.08, 
     FA.PCA2$loadings[,2]+0.08,
     colnames(diet),
     col="blue")
abline(h = 0, v = 0)

plot(FA.PCA3$loadings[,1], 
     FA.PCA3$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Oblimin")
abline(h = 0, v = 0)
text(FA.PCA3$loadings[,1]-0.08, 
     FA.PCA3$loadings[,2]+0.08,
     colnames(diet),
     col="blue")
abline(h = 0, v = 0)


round(FA.PCA2$Phi,3)

## 4 ##

# Factor scores


FA.PCA2$scores

fa=as.data.frame(FA.PCA2$scores)
fa$deaths=deaths

model1=glm(deaths~RC1+RC2,family = binomial(link ="logit"),data = fa)
summary(model1)


ggplot(fa, aes(x = RC1, y = RC2, color = factor(deaths))) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_ellipse(level = 0.95, type = "norm", size = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_manual(values = c("green", "red"), labels = c("Alive", "Dead")) +
  labs(title = "Factor Scores by Mortality Status",
       x = "Factor 1 (Pescatarian)",
       y = "Factor 2 (Carnivore)",
       color = "Death") +
  theme_minimal(base_size = 14)


par(mfrow=c(1,2))

boxplot(RC1 ~ deaths, data = fa,
        main = "boxplot of factor1~deaths",
        xlab = "Death (0 = No, 1 = Yes)",
        ylab = "Factor1")

boxplot(RC2~ deaths, data = fa,
        main = "boxplot of factor2~deaths",
        xlab = "Death (0 = No, 1 = Yes)",
        ylab = "Factor1")

## 5 ## 
eigCor <- eigen(cor(diet))
round(cumsum(eigCor$values/sum(eigCor$values))*100,2)
eigCor$values
diet.fa <- factanal(diet, factors = 2)
diet.fa1 <- factanal(diet, factors = 2,rotation = "varimax")
diet.fa2 <- factanal(diet, factors = 2,rotation = "promax")


par(mfrow = c(1,1))
hist(diet$meat, 
     main = "Histogram of Meat",
     xlab = "Consumption of meat per week")
rowSums(diet.fa$loadings^2)
rowSums(diet.fa1$loadings^2)
rowSums(diet.fa2$loadings^2)


## Part Β ##

data1=read_excel("data1.xlsx") ## load Data

## 1 ##

hist(data1$X1,
     main = "Histogram of X1",
     xlab = "X1")
hist(data1$X2,
     main = "Histogram of X2",
     xlab = "X2")


ggplot(data1, aes(x = X1, y = X2, color = factor(group))) +
  geom_point(size = 2.5) +
  labs(x = "X1: log(δραστηριότητα AHF)",
       y = "X2: log(αντιγόνο τύπου-AHF)",
       color = "Ομάδα") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"),
                     labels = c("Μη Φορείς", "Φορείς")) +
  theme_minimal(base_size = 13)

# Means by group
m1 <- colMeans(data1[data1$group==1,c("X1","X2")])
m2 <- colMeans(data1[data1$group==2,c("X1","X2")])
abs(m2-m1)

## Correlation/Covariance matrix by group
r1 <- cor(data1[data1$group==1,c("X1","X2")])
r2 <- cor(data1[data1$group==2,c("X1","X2")]) 
s1 <- cov(data1[data1$group==1,c("X1","X2")]) 
s2 <- cov(data1[data1$group==2,c("X1","X2")]) 

## Descriptives 
sqrt(diag(s1))
sqrt(diag(s2))
max(data1[data1$group==1,"X1"])
max(data1[data1$group==1,"X2"])
max(data1[data1$group==2,"X1"])
max(data1[data1$group==2,"X2"])
min(data1[data1$group==1,"X1"])
min(data1[data1$group==1,"X2"])
min(data1[data1$group==2,"X1"])
min(data1[data1$group==2,"X2"])

## Number of items by group
n1 = sum(data1$group==1)
n2 = sum(data1$group==2)

## Pooled covariance matrix
Sp <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)

## 2 ##

## Model Fit
lda1 <- lda(group ~ X1 + X2, data = data1,prior = c(1/2,1/2))

lda1$scaling

## 3 ##

## Model prediction for observations based on the Fisher
pred=predict(lda1)
data1$pred=pred$class

## Confusion Matrix
tab=table(data1$group,data1$pred)

## Percentage of wrongful classification
a=sum(diag(tab))/sum(tab)
1-a

## 4 ##

## Predictions for extra data
data2=read_excel("data 2.xlsx") ## load Data
pred1=predict(lda1,data2)
data2$pred=pred1$class


## 5 ##

shapiro.test(data1$X1)

shapiro.test(data1$X2)


## multivariate normality test 
mvn_result <- mvn(data = data1[, c("X1", "X2")],mvn_test="mardia")
mvn_result$multivariateNormality




b <- -0.5 * t(m1 + m2) %*% lda1$scaling
a <- -lda1$scaling[1] / lda1$scaling[2]  # slope
intercept <- -b / lda1$scaling[2]

# Create decision line as data frame
x_vals <- seq(min(data1$X1), max(data1$X1), length.out = 100)
line_df <- data.frame(
  X1 = x_vals,
  X2 = a * x_vals + intercept
)

# Plot

ggplot() +
  # Original data (with group info)
  geom_point(data = data1, 
             aes(x = X1, y = X2, 
                 color = factor(group), 
                 shape = factor(group)), 
             size = 2.5) +
  
  # New observations
  geom_point(data = data2, 
             aes(x = X1, y = X2, 
                 color = "new", 
                 shape = "new"), 
             size = 3) +
  
  # Decision boundary line
  geom_line(data = line_df, aes(x = X1, y = X2), 
            color = "black", linetype = "dashed", size = 1) +
  
  # Axes and legend titles
  labs(x = "X1: log(δραστηριότητα AHF)",
       y = "X2: log(αντιγόνο τύπου-AHF)",
       color = "Κατηγορία",
       shape = "Κατηγορία") +
  
  # Unified legend: color
  scale_color_manual(values = c("1" = "blue", 
                                "2" = "red", 
                                "new" = "green"),
                     labels = c("1" = "Μη Φορείς", 
                                "2" = "Φορείς", 
                                "new" = "Νέες Παρατηρήσεις")) +
  
  # Unified legend: shape
  scale_shape_manual(values = c("1" = 16, 
                                "2" = 16, 
                                "new" = 17),
                     labels = c("1" = "Μη Φορείς", 
                                "2" = "Φορείς", 
                                "new" = "Νέες Παρατηρήσεις")) +
  
  theme_minimal(base_size = 13)




## 6 ## 


##lda2 <- lda(group ~ X1 + X2, data = data1,prior = c(3/4,1/4))
##pred1=predict(lda2)
##data1$pred2=pred1$class


Dens1=dmvnorm(data1[,c("X1","X2")],m1,Sp)
Dens2=dmvnorm(data1[,c("X1","X2")],m2,Sp)

data1$pred1=ifelse(Dens1>Dens2/3,1,2)


## Confusion Matrix
tab1=table(data1$group,data1$pred1)

## Percentage of wrongful classification
a1=sum(diag(tab1))/sum(tab1)
1-a1

## Prediction of new observations

Dens21=dmvnorm(data2[,c("X1","X2")],m1,Sp)
Dens22=dmvnorm(data2[,c("X1","X2")],m2,Sp)
data2$pred1=ifelse(Dens21>Dens22/3,1,2)



