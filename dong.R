# 필요한 패키지 불러오기
install.packages("mlbench")
install.packages("dplyr")
install.packages("caret")
install.packages("e1071")
install.packages("FUN")
install.packages("FNN")
install.packages("magrittr")
library(mlbench)
library(dplyr)
library(caret)
library(e1071)
library(class)
library(gmodels)
library(FNN) 
library(magrittr)
library(gmodels)

# 데이터 불러오기
dat = BreastCancer

# 1(1) 결측값 제거
dat2 <- na.omit(dat)

# ID 변수 제거(ID는 분석에 필요가 없어서 제거)
dat2 <- dat2[,-1]


# 1(1)

# 데이터에서 Class benign이 들어간 행만 200개 추출
A <- subset(dat2, dat2$Class == "benign")
A1 <- A[1:200,]

# 데이터에서 Class malignant가 들어간 행만 200개 추출
B <- subset(dat2, dat2$Class == "malignant")
B1 <- B[1:200,]

# rbind로 행 결합 및 새로운 데이터 만들기
Data <- rbind(A1,B1) 

# Data에 sq라는 1~400의 값을 가지는 열 추가
Data$sq <- 1:400
View(Data)

# 1(2)
d1 <- Data$sq %% 2 == 0
d2 <- Data$sq %% 2 == 1

d3 <- as.numeric(d1)
s1 <- d3[d3==0]

d4 <- as.numeric(d2)
s2 <- d4[d4==1]
View(Data)

#### 1이 짝수 2가 홀수 
Data_knn <- as.numeric(unlist(Data))
itx <- ifelse(Data$sq%%2==0,1,2)

train.data <- Data[itx==1, 1:9]
train.class <- Data[itx==1, 10]
train.data <- as.numeric(unlist(train.data))
train.class <- as.numeric(unlist(train.class))
str(train.data)
length(train.data)

test.data <- Data[itx==2,  1:9]
test.class <- Data[itx==2, 10]
as.numeric(unlist(test.data))
str(test.data)
length(test.data)

model <- knn(train.data, test.data, train.class, k=3)
x = data.frame(train_class,model)
table(x)
CrossTable(x=test.class, model, prop.chisq = FALSE)

# Accuracy : 0.445
# Recall : 0.43
# Precision : 0.44

# 1(3) k가 5일때 가장 적합한 모델
index <- ifelse(Data == seq(from=2, to=400, by=2), "1","2")
View(index)
train_data <- Data[seq(from=2, to=400, by=2),-10]
train_class <- Data[seq(from=2, to=400, by=2),10]
test_data <- Data[seq(from=1, to=400, by=2),-10]

fold <- caret::trainControl(method = "CV", number = 10)
model2 <- caret::train(Class~. ,data = Data, 
                         method = "knn",
                         trControl = fold,
                         tuneLength=25)
model2
plot(model2)


k.fold <- 10
folds <- createFolds(Data$Class, k = k.fold)
folds

kk.seq = seq(1, 15, by = 1)

names(kk.seq) <- sapply(kk.seq, function(kk){
  paste("k = ", kk)
})

kk.result <- sapply(kk.seq, function(kk){
  xval.result <- sapply(folds, function(itx){
    df.train.i <- Data[-itx, ]
    df.test.i <- Data[itx, ]
    model.i <- knn3(Class ~., data = df.train.i, k = kk)
    predict.i <- predict(model.i, df.test.i, type ="class")
    accuracy.i <- sum(predict.i == df.test.i$Class) / length(predict.i)
    return(accuracy.i)
  })
  print(xval.result)
  return(mean(xval.result))
})

print(kk.result)