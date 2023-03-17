

#dataset exported from online source

# import the dataset directly to r 

d = GamingStudy_data
n = nrow(d) 


# Data processing 

# attributes for classification
# the first row is just names of attributes, so that needs to be removed (getting row number 2 through the end)
# In the dataset, all inputs are string, therefore as.numeric is required for the attributes that need to be integers (age, hrs)

age = floor(as.numeric(d[2:n, 43])/10)

platform = d[2:n, 17] #platform used for gaming (pc, console, mobile)

hrs = as.numeric(d[2:n, 18]) #hours spent gaming 

work = d[2:n, 44] #work status (employed, unemployed, student)


# The playstyle, whyplay, and residence have way too many types of inputs (Some of inputs have the same meaning, but since they are in form of 
# string, they are recognized as different inputs simply due to small character difference)

# In order to reduce number of different inputs, we will just take popular inputs and put the rest of them in "other" or categorize by keywords 


playstyle = d[2:n, 49] #playstyle (multi player online, single, both)
playstyle <- ifelse(grepl("Multiplayer", playstyle), "Multiplayer", ifelse(grepl("Singleplayer", playstyle), "Singleplayer", "Other"))



whyplay = d[2:n, 20]#reasons for playing game
whyplay <- ifelse(grepl("improving", whyplay), "improving", 
                  ifelse(grepl("winning", whyplay), "winning", 
                         ifelse(grepl("having fun", whyplay), "having fun", 
                                ifelse(grepl("relaxing", whyplay), "relaxing", "Other"))))






# for residence attribute, we will categorize every countries by regions since there are over 100 different types of inputs

residence = d[2:n, 47]

# loop and find all types of residence inputs
country = rep(0, 195)
x = 1
for(i in 1:n){
  c = residence[i]
  if(c %in% country == FALSE){
    country[x] = c
    x = x + 1
  }
}

#there are unknown answers which will be ommited when put in the data table
NorthA = c("USA", "Canada", "Mexico", "Dominican Republic", "Nicaragua", "Grenada", "Puerto Rico", "Costa Rica", "Belize", "Jamaica")
EU = c("Germany", "Finland", "UK", "Bosnia and Herzegovina", "Ireland", "Romania", "Sweden", "Greece", "Turkey", "Belgium", "Latvia", "Austria",
       "Croatia", "Switzerland", "Netherlands", "Denmark", "Portugal", "France", "Estonia", "Russia", "Czech Republic", "Lithuania", "Norway", 
       "Poland", "Serbia", "Spain", "Slovakia", "Bulgaria", "Italy", "Slovenia", "Ukraine", "Macedonia", "Hungary", "Iceland", "Luxembourg", 
       "Algeria", "Belarus", "Malta", "Albania", "Guadeloupe", "Liechtenstein", "Georgia", "Faroe Islands", "Moldova", "Montenegro", 
       "Republic of Kosovo", "Gibraltar ")
EAsia = c("South Korea", "Japan", "China", "Hong Kong", "Taiwan")
OC = c("Australia", "New Zealand ", "Fiji")
SEAsia = c("Malaysia", "Singapore", "Philippines", "Thailand", "Vietnam", "Indonesia", "Brunei")
Africa = c("South Africa", "Morocco", "Tunisia", "Egypt", "Namibia", "Syria")
SAsia = c("India", "Bangladesh")
SouthA = c("Argentina", "Brazil", "Venezuela", "Chile", "Peru", "Uruguay", "Colombia", "Trinidad & Tobago", "Panama", "Guatemala", "Honduras",
           "Bolivia", "El Salvador", "Ecuador", "St Vincent")
WAsia = c("Saudi Arabia", "Jordan", "Israel", "Qatar", "Cyprus", "UAE", "Lebanon", "Kuwait", "Bahrain", "Palestine")
CAsia = c("Kazakhstan", "Mongolia", "Pakistan")


# Over 93% of people are from either NA, EU, or OC, so it will be put into 4 categories (three main regions plus other)

residence <- ifelse(residence %in% NorthA, "NorthA", ifelse(residence %in% EU, "EU", ifelse(residence %in% OC, "OC", "Other")))




# GAD (general anxiety disorder questionnaire) -> y values
# benchmark is 10. If total points of seven GAD question is equal to or grater than 10, person is condiered to have anxiety

G = d[2:n, 3:9]

GAD = rep(0, n - 1)
for(i in 1:n-1){
  x = as.numeric(G[i, ])
  s = sum(x)
  g = ifelse(s >= 10, "Y", "N")
  GAD[i] = g
}


# The table consisting of all necessary attributes and all NA's ommited

table = na.omit(data.frame(age, residence, work, platform, hrs, whyplay, playstyle, GAD))
n = nrow(table)

# redefining each variable after every row with NA removed

age = table[, 1]
residence = table[, 2]
work = table[, 3]
platform = table[, 4]
hrs = table[, 5]
whyplay = table[, 6]
playstyle = table[, 7]
GAD = table [, 8]



# Classification

# naive bayes classification 

library(e1071)

ind = sample(2, nrow(table), replace = T, prob = c(0.9, 0.1))

train = table[ind == 1, ]
test = table[ind == 2, ]
model = naiveBayes(GAD~. ,  data = train)
pred = predict(model, train, type = 'class')
table(pred)

p1 = predict(model, train, type = 'class')
acc_train = sum(diag(table(p1, train$GAD))/sum(table(p1, train$GAD)))
acc_train

p2 = predict(model, test, type = 'class')
acc_test = sum(diag(table(p2, test$GAD))/sum(table(p2, test$GAD)))
acc_test

TP = 0
FP = 0
FN = 0
TN = 0


p2 = as.character(p2)

for(i in 1:length(p2)){
  if(p2[i] == "Y" && test$GAD[i] == "Y"){
    TP = TP + 1
  }
  if(p2[i] == "Y" && test$GAD[i] == "N"){
    FP = FP + 1
  }
  if(p2[i] == "N" && test$GAD[i] == "Y"){
    FN = FN + 1
  }
  if(p2[i] == "N" && test$GAD[i] == "N"){
    TN = TN + 1
  }
  
}

TPR = TP/(TP + FN)
TPR
FPR = FP/(TN + FP)
FPR
plot(FPR, TPR, xlim=c(0, 1), ylim=c(0,  1))
abline(0, 1)

Accuracy = (TP + TN)/(TP + FN + FP + TN)
Accuracy

# 0.6921



# knn classifier

m = as.matrix(dist(table[,1:7]))
class = table[,8]

classhat = rep(0,n)
for (i in 1:n) {
  s.indices = order(m[i,])[2:9] 
  s.labels = table[s.indices, 8]
  c1 = sum(s.labels == "N")
  c2 = sum(s.labels == "Y")
  
  classhat[i] = which.max(c(c1, c2))
}

table(classhat)
classhat
for(i in 1:n){
  x = ifelse(classhat[i] == 1, "N", "Y")
  classhat[i] = x
}
classhat
class
cat ("error rate = ", sum(class != classhat)/n,"\n") 


TP = 0
FP = 0
FN = 0
TN = 0

for(i in 1:n){
  if(classhat[i] == "Y" && class[i] == "Y"){
    TP = TP + 1
  }
  if(classhat[i] == "Y" && class[i] == "N"){
    FP = FP + 1
  }
  if(classhat[i] == "N" && class[i] == "Y"){
    FN = FN + 1
  }
  if(classhat[i] == "N" && class[i] == "N"){
    TN = TN + 1
  }
  
}

TPR = TP/(TP + FN)
TPR
FPR = FP/(TN + FP)
FPR
plot(FPR, TPR, xlim=c(0, 1), ylim=c(0,  1))
abline(0, 1)


Accuracy = (TP + TN)/(TP + FN + FP + TN)
Accuracy 
# 0.8223 




# decision tree

# rpart is a machine learning library that is used for building classification tree

library(rpart)
fit= rpart(GAD ~  age + residence + work + platform + hrs + whyplay + playstyle, 
           data = table, method ="class",control =rpart.control(minsplit =3,minbucket=1, cp=0.0001, maxdepth = 5))
print(fit)








# neural network 


library(neuralnet)
library(caret)

# neuralnet function carries out matirx multiplication operation, therefore all attribute inputs must be an integer
# we will need to redefine all attributes

residence <- ifelse(residence == "NorthA", 1, ifelse(residence == "EU", 2, ifelse(residence == "OC", 3, 4)))

whyplay <- ifelse(whyplay == "having fun", 1, 
                  ifelse(whyplay == "relaxing", 2, 
                         ifelse(whyplay == "winning", 3, 
                                ifelse(whyplay == "improving", 4, 5))))

work <- ifelse(grepl("Employed", work), 1, ifelse(grepl("Unemployed", work), 2, 3))

platform <- ifelse(platform == "PC", 1, ifelse(platform == "Console (PS, Xbox, ...)", 2, 3))

playstyle <- ifelse(grepl("Singleplayer", playstyle), 1, ifelse(grepl("Multiplayer", playstyle), 2, 3))


# new table after all attributes are converted to integers
table = na.omit(data.frame(age, residence, work, platform, hrs, whyplay, playstyle, GAD))
n = nrow(table)

str(table)
set.seed(123)

GAD = table[, 8]
nnet = neuralnet(GAD~.,data = table, act.fct = "logistic", linear.output = FALSE)
plot(nnet)
test = table[sample(2, nrow(table), replace = T, prob = c(0.9, 0.1)) == 2, ]
Predict = compute(nnet, test)
prob = Predict$net.result
sum(prob[, 2] != 0.1730472)

pred = rep(0, n)
for(i in 1:n){
  n = Predict$net.result[i, 1]
  y = Predict$net.result[i, 2]
  a = ifelse(y > n, "Y", "N")
  pred[i] = a
}

e = sum(pred != GAD)/nrow(table)
e
Accuracy = 1 - e
Accuracy
