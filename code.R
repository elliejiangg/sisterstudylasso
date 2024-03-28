#Libraries
library(glmnet)
library(readxl)
library(data.table)
library(ggplot2)

#upload file
log2_abundance_189_chems <- read_excel("log2_abundance_189_chems.xlsx")
chemicals <- log2_abundance_189_chems[,2:191]


#training / testing data

size = floor(0.8 * nrow(chemicals))

set.seed(100)
train_ind = sample(seq_len(nrow(chemicals)), size = size)

train = chemicals[train_ind,]
x_train = train[,1:189]
x_train = sapply(x_train, as.numeric)
y_train = train[,190]
#ctrl = 0, case = 1
y_train[y_train == "case"] <- "1"
y_train[y_train == 'ctrl']<- "0"
y_train = sapply(y_train, as.numeric)

test = chemicals[-train_ind,]
x_test = test[,1:189]
x_test = sapply(x_test, as.numeric)
y_test = test[,190]
y_test[y_test == "case"] <- "1"
y_test[y_test == 'ctrl']<- "0"
y_test = sapply(y_test, as.numeric)



#preparing data sets for cross validation
chemnum = sapply(chemicals, as.numeric)
ynum = chemicals[,190]
ynum[ynum == "case"] <- "1"
ynum[ynum == 'ctrl']<- "0"
ynum = sapply(ynum, as.numeric)
y = data.frame(ynum)
new_data =  as.matrix(chemnum[,-190])
cv_model = cv.glmnet(as.matrix(new_data), ynum,
                     alpha = 1, nfolds = 10)

#cross validation model
plot(cv_model)

optimal_model = glmnet(as.matrix(chemnum[, -190]), ynum, alpha = 1, lambda = cv_model$lambda.min)



#getting the optimal lambda
predictions = predict(optimal_model, newx = new_data, s = cv_model$lambda.min)

fitt = glmnet(x_train, y_train, alpha = 1, lambda = predictions,
             family = "binomial")

plot(fitt, xvar = 'lambda', label = T )
#feature selection
c = coef(cv_model, s = 'lambda.min', exact = TRUE)
inds = which(c!=0)
variables = row.names(c)[inds]
variables = variables [!variables %in% '(Intercept)']
variables


