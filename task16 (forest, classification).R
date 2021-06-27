#ЗАДАЧА 1
#Возьмите начало скрипта из этого документа после условий задачи. Вы знаете Х, Y и целевую переменную Z. Познакомьтесь со способом визуализации дискретных переменных.
#a) Предположим, вы угадали форму зависимости от X и Y. Обучите на их квадратах логрегрессию на Z
#b) Предположим, вы не собираетесь угадывать зависимости. Обучите лес на сырых X иY
#c) Вычислите и сравните аук-роки обеих моделей
#d) Придумайте, как нарисовать предсказания. Подсказка: используйте рок-кривую

library(pROC)
library(randomForest)
set.seed(7)
X=rnorm(1000) 
Y=rnorm(1000) 
E=rnorm(1000) 
Z=as.numeric(X^2+Y^2+E^2<2)
X_test=rnorm(1e4)
Y_test=rnorm(1e4)
E_test=rnorm(1e4) 
Z_test=as.numeric(X_test^2+Y_test^2+E_test^2<2)
plot(X,Y,col=Z+3,pch=16)

df_train=data.frame(Z,X,Y)
df_test=data.frame(Z_test,X_test,Y_test)
colnames(df_test)=c("Z","X","Y")

X2=X*X
Y2=Y*Y
Z2=Z
df_train_glm=data.frame(Z2,X2,Y2)
X2=X_test*X_test
Y2=Y_test*Y_test
Z2=Z_test
df_test_glm=data.frame(Z2,X2,Y2)


mod_glm=glm(Z2~., data = df_train_glm, family = binomial(link = "logit"))
pred_glm=predict(mod_glm, newdata = as.data.frame(df_test_glm) ,type = "response")
model_roc_glm=roc(predictor = pred_glm, response = df_test_glm$Z2)
plot(model_roc_glm, col="green")

mod_forest=randomForest(factor(Z)~.,data = df_train) 
pred_forest=predict(mod_forest, newdata=as.data.frame(df_test), type = "prob")
model_roc_forest=roc(predictor=pred_forest[,1], response = df_test$Z)
plot(model_roc_forest,add=TRUE, col="red")

model_roc_forest$auc #0.8655
model_roc_glm$auc #0.8871
legend("bottomright",legend=c("glm AUC = 88.71 %","forest AUC = 86.55 %"),col=c("green","red"),lwd=2)


th=model_roc_glm$thresholds
s=model_roc_glm$sensitivities
sp=model_roc_glm$specificities
min_ind=which.min(abs(s-sp))
optimal_th=th[min_ind]
ind=which(model_roc_glm$predictor > optimal_th)
X1=X_test[ind]
X2=X_test[-ind]
Y1=Y_test[ind]
Y2=Y_test[-ind]
plot(X2,Y2,col="blue",pch=16)
points(X1,Y1,col="green",pch=16)


