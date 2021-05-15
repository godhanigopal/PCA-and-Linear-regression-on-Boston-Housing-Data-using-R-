library(MASS)
library(dplyr)
library(neuralnet)
library(NeuralNetTools)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(clusterSim)
library(glmnet)
library(factoextra)
library(ggfortify)


#Load data in a frame
df_Boston <- Boston

#check dimention
dim(df_Boston)

#check head and tail
head(df_Boston)
tail(df_Boston)

# structure of the data
str(df_Boston)

#summary statistics
summary(df_Boston)

# check for the NA/NULL values in data.
colSums(is.na(df_Boston))

#Check for duplicated values
sum(duplicated(df_Boston))

#checking the correlation between features
result_cor<-cor(df_Boston)
corrplot(result_cor,method = "number",type = "upper")

#visualize each predictor variable against medv
df_Boston %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(formula = y ~ x,method = "lm", se = TRUE, col = "red") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of each variables vs Median Value (medv)")

#z-score normalization
#type n1 - standardization - ((x-mean)/sd).
df_boston_norm_all<-data.Normalization(df_Boston, type = "n1", normalization = "column")

#check summary statistics after normalization
summary(df_boston_norm_all)

df_boston_norm_medv <-df_boston_norm_all $medv

#PCA
df_Boston_pca <- prcomp(df_boston_norm_all)

#summary of PCA components
summary(df_Boston_pca)

#Visualize eigenvalues (scree plot). 
fviz_eig(df_Boston_pca)

fviz_pca_biplot(df_Boston_pca, repel = TRUE,
                col.var = "red", # Variables color
                col.ind = "#696969"  # Individuals color,
)

#plotting PCA
autoplot(df_Boston_pca, data=df_Boston,loadings=TRUE,loadings.label=T, colour = 'medv' )

# each PC variance in %
plot(summary(df_Boston_pca)$importance[3,])

# Check how strongly a loading of variable contributes to pricipal components
fviz_pca_var(df_Boston_pca,axes = c(1, 2))
fviz_pca_var(df_Boston_pca,axes = c(3, 4))
fviz_pca_var(df_Boston_pca,axes = c(5, 6))
fviz_pca_var(df_Boston_pca,axes = c(7, 8))

## correlation matrix principal components
cor_pca <- cor(df_Boston_pca$x, method = "pearson")
corrplot(cor_pca, method = "number")

#Extracting PC from  output
principal_cps <- as.data.frame(df_Boston_pca$x)

#Creating matrix using first seven PC to feed in our model
pca7_mv_data <- cbind(df_boston_norm_medv,principal_cps[,1:7])
head(pca7_mv_data)
dim(pca7_mv_data)

#Scatter plots showing relationship between PCs and our target variable
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC1, xlab="medv",ylab="PC1")
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC2, xlab="medv",ylab="PC2")
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC3, xlab="medv",ylab="PC3")
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC4, xlab="medv",ylab="PC4")
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC5, xlab="medv",ylab="PC5")
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC6, xlab="medv",ylab="PC6")
plot(x=df_boston_norm_medv,y=pca7_mv_data$PC7, xlab="medv",ylab="PC7")


# split the data
df_Boston_train<-df_boston_norm_all[1:380,]
df_Boston_test<-df_boston_norm_all[381:506,]

df_Boston_train_pca<-pca7_mv_data[1:380,]
df_Boston_test_pca<-pca7_mv_data[381:506,]

# applying the linear regression to data with PCA
l_model_pca <- lm(df_boston_norm_medv ~ ., data = df_Boston_train_pca)
#check the model 
summary(l_model_pca)

# applying the linear regression to data with PCA
l_model_2 <- lm(medv ~ ., data = df_Boston_train)
summary(l_model_2)


#shows which predicotr variable are significant
#check for coeeficient siginificance
summary(l_model_pca)$coef
summary(l_model_2)$coef


#prediction using both the nodels
pred_medv_pca<-predict(l_model_pca, new.data=df_Boston_test_pca,interval = "prediction" )
pred_medv_noPCA <-predict(l_model_pca, new.data=df_Boston_test,interval = "prediction" )
