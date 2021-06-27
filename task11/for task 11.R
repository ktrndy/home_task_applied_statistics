set.seed(7) #Фиксируем случайную генерацию
X=rnorm(200)
Z=rnorm(200)


Y=Z+X^2+2*sin(3*X) #настоящая зависимость
plot(X,Y, pch=16)

#Истинная модель#####
data=data.frame(Y,X,X^2,sin(3*X)) #Предположим, что мы угадали факторы
mod_real=glm(Y~., data = data)
summary(mod_real)

predict_lm=predict(mod_real, newdata = data)
plot(X,predict_lm, type = "l") #Для отрисовки красивой линии предсказания надо отсортировать

plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #Последняя цифра в rgb() задаёт полупрозрачность
lines(X[order(X)],predict_lm[order(X)], type = "l", col=2,lwd=3) #Прелесть

library(glmnet) #Попробуем регуляризацию

data_matrix=cbind(X,X^2,sin(3*X)) #ВНИМАНИЕ! Glmnet ест только матрицы и только не в виде формулы

cross_validation=cv.glmnet(x = data_matrix, y = Y, alpha = 0, nfolds = 10) #Запускаем кросс-валидацию на десяти фолдах
plot(cross_validation) #Лучшая лямбда равно нулю, регуляризация не нужна

model_L2=glmnet(x = data_matrix, y = Y, alpha = 0, lambda = 0) #Обучаем с подобранной лямбдой
coef(model_L2) #Можем посмотреть коэффициенты

predict_L2=predict(model_L2, newx = data_matrix) #Делаем предсказание


plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #Последняя цифра в rgb() задаёт полупрозрачность
lines(X[order(X)],predict_lm[order(X)], type = "l", col=2,lwd=3) #Прелесть
lines(X[order(X)],predict_L2[order(X)], type = "l", col=4,lwd=3, lty=2) #Никаких отличий


#Оверфитнем#####

data=data.frame(Y,poly(X,degree = 10, raw = T)) #функция poly() делает из данных полиномы нужной степени.
#Это явно неверные предикторы####

#Вам может понадобиться это: options(scipen=999); оно даст возможность печатать цифру 6e-3 как 0.006

#Проведите анализ в этом случае#
