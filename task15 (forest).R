#ЗАДАЧА 1
#Установите библиотеку titanic с данными пассажиров Титаника. В хелпе описания переменных нет, зато оно есть в гугле:
#Survived: Survived (1) or died (0)
#Pclass: Passenger’s class
#Name: Passenger’s name
#Sex: Passenger’s sex
#Age: Passenger’s age
#SibSp: Number of siblings/spouses aboard
#Parch: Number of parents/children aboard
#Ticket: Ticket number
#Fare: Fare
#Cabin: Cabin
#Embarked: Port of embarkation
#Не будем рассматривать Ticket, Cabin и Name – они не очень информативны.
#Предскажите выживание пассажиров с помощью трёх методов «из коробки»:
#a) Логистической регрессии без регуляризации
#b) Бэггинга
#c) Случайного леса с m = 2
#Сравните AUC этих методов на тестовой выборке. Нарисуйте РОК-кривые и АУКи.
#Вместе с дз я прислал скрипт. Пожалуйста разберитесь с обработкой данных и продолжите делать задание в нём.
#Важно: предикт леса должен вернуть не класс, а вероятность. См. хелп.

library(randomForest)
library(titanic)
library(pROC)

df=titanic_train
df[c("Name", "Ticket","Cabin")] = NULL
df$Embarked[df$Embarked == ""]=NA 
df=df[complete.cases(df),] #Убираем Na: лес их не съест
df$Embarked=factor(df$Embarked) #Превращаем строчки в факторы. См. Вики "Фиктивные переменные"
df$Sex=factor(df$Sex)

set.seed(7)
N=NROW(df)
random_subsample = sample(1:N,size = 0.7*N)
df_train=df[random_subsample , ]
df_test=df[-random_subsample , ]

#ЛОГ РЕГРЕССИЯ
mod_glm=glm(Survived~.,data=df_train, family = binomial(link = "logit")) 
pred_glm=predict(mod_glm, newdata = df_test ,type = "response")
model_roc1=roc(predictor = pred_glm, response = df_test$Survived)
plot(model_roc1, col="green")

#БЭГГИНГ
mod_bg=randomForest(factor(Survived)~.,data = df_train, mtry = 8, ntree=1000) 
pred_bg=predict(mod_bg, newdata=df_test, type = "prob")
model_roc2=roc(predictor=pred_bg[,1], response = df_test$Survived)
plot(model_roc2,add=TRUE, col="blue")

#ЛЕС
mod_rf=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=1000) 
pred_rf=predict(mod_rf, newdata=as.data.frame(df_test), type = "prob")
model_roc3=roc(predictor=pred_rf[,1], response = df_test$Survived)
plot(model_roc3,add=TRUE, col="red")

#model_roc1$auc = 0.8615
#model_roc2$auc = 0.8702
#model_roc3$auc = 0.8811
legend("bottomright",legend=c("glm AUC = 86.15 %","bg AUC = 87.02 %","rf AUC = 88.11 %"),col=c("green","blue","red"),lwd=2)


#ЗАДАЧА 2
#Сравните на тестовой выборке лес с m = 2, обученный на 10, 100, 500, 1000, 5000 и 10000 деревьях. Можете добавить свои значения, если интересно. Нарисуйте их на рисунке.
#Достаточно ли стандартных 500 деревьев?

#AUC получился максимальный на 500(сначала увеличивается, потом уменьшается), последние (5000б 10000) работали медленнее, 500 - достаточно (а вообще достаточно и 100)
mod_rf2=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=10) 
pred_rf2=predict(mod_rf2, newdata=as.data.frame(df_test), type = "prob")
model_roc4=roc(predictor=pred_rf2[,1], response = df_test$Survived)
plot(model_roc4, col=1)
model_roc4$auc
mod_rf2=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=100)
pred_rf2=predict(mod_rf2, newdata=as.data.frame(df_test), type = "prob")
model_roc4=roc(predictor=pred_rf2[,1], response = df_test$Survived)
plot(model_roc4,add=TRUE, col=2)
model_roc4$auc
mod_rf2=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=500)
pred_rf2=predict(mod_rf2, newdata=as.data.frame(df_test), type = "prob")
model_roc4=roc(predictor=pred_rf2[,1], response = df_test$Survived)
plot(model_roc4,add=TRUE, col=3)
model_roc4$auc
mod_rf2=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=1000)
pred_rf2=predict(mod_rf2, newdata=as.data.frame(df_test), type = "prob")
model_roc4=roc(predictor=pred_rf2[,1], response = df_test$Survived)
plot(model_roc4,add=TRUE, col=4)
model_roc4$auc
mod_rf2=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=5000)
pred_rf2=predict(mod_rf2, newdata=as.data.frame(df_test), type = "prob")
model_roc4=roc(predictor=pred_rf2[,1], response = df_test$Survived)
plot(model_roc4,add=TRUE, col=5)
model_roc4$auc
mod_rf2=randomForest(factor(Survived)~.,data = df_train, mtry = 2, ntree=10000)
pred_rf2=predict(mod_rf2, newdata=as.data.frame(df_test), type = "prob")
model_roc4=roc(predictor=pred_rf2[,1], response = df_test$Survived)
plot(model_roc4,add=TRUE, col=6)
model_roc4$auc