install.packages("lattice")
install.packages("ggplot2")
library(lattice)
library(ggplot2)
install.packages("sqldf")
install.packages("corrplot")
install.packages("caret")
install.packages("e1071")
library(sqldf)
library(corrplot)
library(caret)
library(e1071)
install.packages("rpart")
library(rpart)
a <- read.csv(file.choose())
a$diagnosis <- as.vector(a$diagnosis)
a$diagnosis[a$diagnosis == 'B'] <- 0
a$diagnosis[a$diagnosis == 'M'] <- 1
a <- a[, -33]
a$diagnosis <- as.numeric(a$diagnosis)

a1 <- sqldf("SELECT diagnosis, radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean,
            compactness_mean, concavity_mean, concave_points_mean, symmetry_mean, 
            fractal_dimension_mean FROM a")
corMatrix <- cor(a1)
corrplot(corMatrix, method="circle", type="lower", addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, sig.level = 0.05, insig = "blank")

a2 <- sqldf("SELECT diagnosis, radius_se, texture_se, perimeter_se, area_se, smoothness_se,
            compactness_se, concavity_se, concave_points_se, symmetry_se, fractal_dimension_se
            FROM a")
corMatrix2 <- cor(a2)
corrplot(corMatrix2, method="circle", type="lower", addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, sig.level = 0.05, insig = "blank")

a3 <- sqldf("SELECT diagnosis, radius_worst, texture_worst, texture_worst, 
            area_worst, smoothness_worst, compactness_worst, concavity_worst, concave_points_worst, 
            symmetry_worst, fractal_dimension_worst FROM a")
corMatrix3 <- cor(a3)
corrplot(corMatrix3, method="circle", type="lower", addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=TRUE, sig.level = 0.05, insig = "blank")

glm.fit <- glm(a$diagnosis~ a$radius_mean + a$perimeter_mean + a$area_mean+
                 a$compactness_mean + a$concavity_mean + a$concave_points_mean+
                 a$radius_se + a$perimeter_se + a$area_se + a$radius_worst + a$area_worst + 
                 a$concavity_worst + a$concave_points_worst, family ="binomial", a)
summary(glm.fit)

glm.fit1 <- glm(a$diagnosis~ a$radius_mean +a$texture_mean + a$perimeter_mean +  a$area_mean +
                  a$smoothness_mean + a$compactness_mean +  a$concavity_mean + 
                  a$concave_points_mean +  a$symmetry_mean + a$fractal_dimension_mean, 
                family ="binomial", a)
summary(glm.fit1)

glm.fit2 <- glm(a$diagnosis~ a$radius_se + a$texture_se + a$perimeter_se + a$area_se + 
                  a$smoothness_se + a$compactness_se + a$concavity_se + a$concave_points_se +
                  a$symmetry_se + a$fractal_dimension_se, family ="binomial", a)      
summary(glm.fit2)

glm.fit3 <- glm(a$diagnosis~ a$radius_worst + a$texture_worst + a$texture_worst + 
                  a$area_worst + a$smoothness_worst + a$compactness_worst + a$concavity_worst +
                  a$concave_points_worst + a$symmetry_worst + a$fractal_dimension_worst,
                family ="binomial", a)      
summary(glm.fit3)
glm.fit4 <- glm(a$diagnosis~ a$texture_mean + a$area_mean+ a$smoothness_mean + a$concave_points_mean
                + a$compactness_se +a$fractal_dimension_se + a$smoothness_worst + a$concave_points_worst+
                  a$radius_se + a$area_se + a$texture_worst)
summary(glm.fit4)

glm.fit5 <- glm(a$diagnosis~ a$area_mean+ a$smoothness_mean +  a$smoothness_worst + 
                  a$concave_points_worst+ a$radius_se + a$area_se + a$texture_worst)
summary(glm.fit5)

a[, 33] <- predict.glm(glm.fit5, new_data = a, method ="response")
a[, 34] <- (a[, 2]-a[, 33])^2
RMSE = mean(a$V34)
acc = 1-RMSE

sam <- sample(1:nrow(a),size=nrow(a)*0.7,replace = FALSE)

# Training set
train_set <- a[sam,]   # 70% training data
test_set <- a[-sam,]   # 30% test data
diagnosis<-as.factor(a$diagnosis)
pred<-round(predict(glm.fit5,new_data=a))
confusionMatrix(factor(pred, levels=0:1), factor(a$diagnosis,levels=0:1))
#Decision Tree Model


model_tree<- rpart(diagnosis ~ area_mean+ smoothness_mean +  smoothness_worst + 
                     concave_points_worst+ radius_se + area_se + texture_worst, data = train_set   )



summary(model_tree)
plot(model_tree, uniform=TRUE, 
     main="Classification Tree")
text(model_tree,use.n=TRUE, all=TRUE, cex=.8)




