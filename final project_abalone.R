setwd('/Users/xiaozihan/Documents/UCDavis/2017fall/STA_206/final project')
df_ab <- read.csv('abalone.txt', header = F)
head(df_ab)
#clean the data with correct colnames and value
df_ab[, c(1, 9)] <- df_ab[, c(9, 1)]
colnames(df_ab) <- c('rings', 'length', 'diameter', 'height', 'whole_weight',
                     'shucked_weight', 'viscera_weight', 'shell_weight', 'sex')
df_ab[, 1] <- df_ab[, 1]+1.5
#detect the missing values
sum(is.na(df_ab))
#check whether the class of each column is correct or not
sapply(df_ab, class)
fit <- lm(rings ~ ., data = df_ab)
par(mfrow = c(1,1))
#Check response variable should be transformed or not
library(MASS)
boxcox(fit)#boxcox shows that a log transform should be applied
hist(df_ab$rings, main = 'histogram of rings')
hist(log(df_ab$rings), main = 'histogram of response variable with log transformation')#draw a histgram of log response
hist(1/(df_ab$rings))
hist(sqrt(df_ab$rings))
hist((df_ab$rings)^-0.3)
df_ab[, 1] <- log(df_ab[, 1])
colnames(df_ab)[1] <- 'log_age'
fit1 <- lm(log_age ~ ., data = df_ab)
pairs(df_ab)
#draw histgrams to the rest of quantitative variables and a pie plot of catagorical variables
par(mfrow=c(3, 3))
hist(df_ab$length, main = 'histogram of length')
hist(df_ab$diameter, main = 'histogram of diameter')
hist(df_ab$height, main = 'histogram of height')
hist(df_ab$whole_weight, main = 'histogram of whole weight')
hist(df_ab$shucked_weight, main = 'histogram of shucked weight')
hist(df_ab$viscera_weight, main = 'histogram of viscera weight')
hist(df_ab$shell_weight, main = 'histogram of shell weight')

par(mfrow = c(1,1))
boxplot(df_ab$log_age~df_ab$sex, main = 'boxplot of age by sex')
pie(table(df_ab$sex), labels = c('Female','Infant', 'Male'), main = 'pie plot for sex')
summary(fit1)
anova(fit1)
#study the multicollinearity between X variables
correlation_matrix=cor(df_ab[c(2, 3, 4, 5, 6, 7, 8)])
set.seed(100)
n = nrow(df_ab)/2+0.5
ind = sample(1:nrow(df_ab), n, replace=FALSE)
train = df_ab[ind, ]  #training set
valid = df_ab[-ind, ]  #validation set
train <- train[, -3]#drop diameter which is highly collinearity with length
df_ab3 <- df_ab
df_ab3[ind, ncol(df_ab)+1] <- 'train'
df_ab3[-ind, ncol(df_ab)+1] <- 'valid'
colnames(df_ab3)[ncol(df_ab)+1] <- 'data_type'
#draw boxplot between train and valid data
par(mfrow = c(2, 2))
for (i in c(1:8)){
        lab <- colnames(df_ab3)[i]
        main_name <- paste('boxplot of', lab , 'by data type',sep=" ")
        boxplot(df_ab3[, i]~df_ab3$data_type,main=main_name)
}
par(mfrow=c(1,2))
pie(table(train$sex), labels = c('Female','Infant', 'Male'), main = 'pie plot for sex in training data')
pie(table(valid$sex), labels = c('Female','Infant', 'Male'), main = 'pie plot for sex in validation data')
#stepwise
none_mod = lm(log_age~1, data = train)
full_mod = lm(log_age~., data = train)
full1 <- lm(log_age~., data = train)
n = nrow(train)
stepAIC(none_mod, scope=list(upper=full_mod), direction="both", k=2)
model_fit1 <- lm(log_age ~ shell_weight + shucked_weight + length + 
                           sex + height + whole_weight + viscera_weight, data = train)

an_m1 <- anova(model_fit1)
summary(model_fit1)
par(mfrow = c(1,1))
plot(model_fit1, which = 1)
plot(model_fit1, which = 2)
#external validation with mspe
ntrain <- nrow(train)
p <- length(model_fit1$coefficients)
train_model <- model_fit1
valid_model <- lm(log_age ~ shell_weight + shucked_weight + length + 
                            sex + height + whole_weight + viscera_weight, data = valid)
sse_t = sum((train_model$residuals)^2)
sse_v = sum(train_model$residuals^2)
Radj_t = summary(train_model)$adj.r.squared
Radj_v = summary(train_model)$adj.r.squared
train_sum = c(sse_t,Radj_t)
valid_sum = c(sse_v,Radj_v)
criteria = rbind(train_sum,valid_sum)
colnames(criteria) = c("SSE","R2_adj")
newdata = valid[, -1]
y.hat = predict(train_model, newdata)
MSPE = mean((valid$log_age - y.hat)^2)
sse_t/(nrow(train))
(MSPE - sse_t/(nrow(train)))/mse_t
cp = sse_t/(summary(full2)$sigma^2) - (ntrain-2*p)
aic = ntrain*log(sse_t/(nrow(train)))+2*p
bic = ntrain*log(sse_t/(nrow(train)))+log(ntrain)*p
mse_t <- an_m1$`Mean Sq`[8]
c1 <- c(p, cp, sse_t, mse_t, MSPE, aic, bic, Radj_t)
#second-order model
train <- train[, c('log_age','length', 'height', 'whole_weight', 'shell_weight', 'shucked_weight', 'viscera_weight', 'sex')]
full2 <- lm(log_age ~ .^2 + I(shell_weight*shell_weight) + I(shucked_weight*shucked_weight) + I(whole_weight*whole_weight) +
                   I(viscera_weight*viscera_weight), data = train)
none_mod = lm(log_age~1, data = train)
full_mod = full2
length(full2$coefficients)
#aic
stepAIC(none_mod, scope=list(upper=full_mod), direction="forward", k=2)
model_fit2 <- lm(formula = log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                         I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                         length + height + viscera_weight + shucked_weight:sex + shucked_weight:whole_weight + 
                         shell_weight:length + shell_weight:whole_weight + sex:height + 
                         shucked_weight:height + shell_weight:height + length:height + 
                         whole_weight:length + shell_weight:shucked_weight + whole_weight:height, 
                 data = train)
#bic
stepAIC(none_mod, scope=list(upper=full_mod), direction="forward", k=log(n))
model_fit3 <- lm(formula = log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                         I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                         length + height + shucked_weight:sex + shucked_weight:whole_weight + 
                         shell_weight:length + shell_weight:whole_weight, data = train)
anova(model_fit2)
summary(model_fit2)
par(mfrow = c(1,1))
plot(model_fit2)
length(model_fit2$coefficients)
an_m3 <- anova(model_fit3)
summary(model_fit3)
plot(model_fit3)
length(model_fit3$coefficients)
#model validation for model fit 2
train1 <- lm(log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                     I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                     length + height + viscera_weight + shucked_weight:sex + shucked_weight:whole_weight + 
                     shell_weight:length + shell_weight:whole_weight + sex:height + 
                     shucked_weight:height + shell_weight:height + length:height + 
                     whole_weight:length + shell_weight:shucked_weight + whole_weight:height, data = train)
valid1 <- lm(log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                     I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                     length + height + viscera_weight + shucked_weight:sex + shucked_weight:whole_weight + 
                     shell_weight:length + shell_weight:whole_weight + sex:height + 
                     shucked_weight:height + shell_weight:height + length:height + 
                     whole_weight:length + shell_weight:shucked_weight + whole_weight:height, data = valid)
an_m2 <- anova(train1)
summary(train1)
anova(valid1)
summary(valid1)
sse_t1 = sum(train1$residuals^2)
sse_v1 = sum(valid1$residuals^2)
Radj_t1 = summary(train1)$adj.r.squared
Radj_v1 = summary(valid1)$adj.r.squared
train_sum1 = c(sse_t1,Radj_t1)
valid_sum1 = c(sse_v1,Radj_v1)
criteria1 = rbind(train_sum1,valid_sum1)
colnames(criteria1) = c("SSE","R2_adj")

#model validation for model fit 3
train2 <- lm(formula = log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                     I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                     length + height + shucked_weight:sex + shucked_weight:whole_weight + 
                     shell_weight:length + shell_weight:whole_weight, data = train)
valid2 <- lm(formula = log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                     I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                     length + height + shucked_weight:sex + shucked_weight:whole_weight + 
                     shell_weight:length + shell_weight:whole_weight, data = valid)
#sse
sse_t2 = sum(train2$residuals^2)
sse_v2 = sum(valid2$residuals^2)
Radj_t2 = summary(train2)$adj.r.squared
Radj_v2 = summary(valid2)$adj.r.squared
train_sum2 = c(sse_t2,Radj_t2)
valid_sum2 = c(sse_v2,Radj_v2)
criteria2 = rbind(train_sum2,valid_sum2)
colnames(criteria2) = c("SSE","R2_adj")
#Get MSPE_v from new data for model fit 2
newdata = valid[, -1]
y.hat1 = predict(train1, newdata)
MSPE1 = mean((valid$log_age - y.hat1)^2)
sse_t1/(nrow(train))
(MSPE1 - sse_t1/(nrow(train)))/(sse_t1/(nrow(train)))
mse_t1 <- an_m2$`Mean Sq`[21]
#Get MSPE_v from new data for model fit 3
y.hat2 = predict(train2, newdata)
MSPE2 = mean((valid$log_age - y.hat2)^2)
sse_t2/(nrow(train))
(MSPE2 - (sse_t2/(nrow(train))))/(sse_t2/(nrow(train)))
mse_t2 <- an_m3$`Mean Sq`[13]
#cp aic bic
p1 <- length(model_fit2$coefficients)
cp1 = sse_t1/summary(full2)$sigma^2 - (ntrain-2*p1)
aic1 = ntrain*log((sse_t1/(nrow(train))))+2*p1
bic1 = ntrain*log((sse_t1/(nrow(train))))+log(ntrain)*p1
c2 <- c(p1, cp1, sse_t1, mse_t1, MSPE1, aic1, bic1, Radj_t1)
p2 <- length(model_fit3$coefficients)
cp2 = sse_t2/summary(full2)$sigma^2 - (ntrain-2*p2)
aic2 = ntrain*log((sse_t2/(nrow(train))))+2*p2
bic2 = ntrain*log((sse_t2/(nrow(train))))+log(ntrain)*p2
c3 <- c(p2, cp2, sse_t2, mse_t2, MSPE2, aic2, bic2, Radj_t2)
#table
criterion_table <- rbind(c1, c2, c3)
colnames(criterion_table) <- c('p', 'cp', 'sse', 'mse', 'MSPE', 'aic', 'bic', 'Radj')
rownames(criterion_table) <- c('model1', 'model2', 'model3')
#model diagnostics
fitmodel <- lm(log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                       I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                       length + height + viscera_weight + shucked_weight:sex + shucked_weight:whole_weight + 
                       shell_weight:length + shell_weight:whole_weight + sex:height + 
                       shucked_weight:height + shell_weight:height + length:height + 
                       whole_weight:length + shell_weight:shucked_weight + whole_weight:height, data = df_ab)
summary(fitmodel)
anova(fitmodel)
length(fitmodel$coefficients)
par(mfrow = c(1,1))
plot(fitmodel, which = 1)
plot(fitmodel, which = 2)
plot(fitmodel, which = 4)#cook distance, influencial case
n <- nrow(df_ab)
#Y outliers
p = length(fitmodel$coefficients)
stu.res.del = studres(train1)
head(sort(abs(stu.res.del), decreasing=TRUE))
qt(1-.1/(2*n), n-3-1) #Bonferroni’s Threshold
#X outliers
h = as.vector(influence(fitmodel)$hat)
index.X = which(h>(2*p/n))
#cook distance
#check whether the influencial case is significantly influencial or not
res <- residuals(fitmodel)
cook.d = res^2*h/(p*0.027*(1-h)^2)
cook.max = cook.d[which(cook.d==max(cook.d))]
pf(cook.max,p,n-p)
#kick out the influencial case and refit the model
ab_clean <- df_ab[-2052, ]
fitmodel <- lm(log_age ~ shell_weight + I(shucked_weight * shucked_weight) + 
                       I(shell_weight * shell_weight) + shucked_weight + sex + whole_weight + 
                       length + height + viscera_weight + shucked_weight:sex + shucked_weight:whole_weight + 
                       shell_weight:length + shell_weight:whole_weight + sex:height + 
                       shucked_weight:height + shell_weight:height + length:height + 
                       whole_weight:length + shell_weight:shucked_weight + whole_weight:height, data = ab_clean)
summary(fitmodel)
anova(fitmodel)
plot(fitmodel, which = 1)
plot(fitmodel, which = 2)
plot(fitmodel, which = 4)#cook distance, influencial case
n <- nrow(ab_clean)
res <- residuals(fitmodel)
h1 <- as.vector(influence(fitmodel)$hat)
cook.d = res^2*h1/(p*0.027*(1-h1)^2)
cook.max = cook.d[which(cook.d==max(cook.d))]
pf(cook.max,p,n-p)#no significant outliers 

