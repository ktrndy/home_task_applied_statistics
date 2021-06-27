# Смотри доп. скрипт. Наша задача - посмотреть, как работает регуляризация на совсем явном и красивом примере.

#ЗАДАЧА 1
#Изучите начало скрипта. Убедитесь, что в случае верно выбранных предикторов и однородных данных регуляризация не нужна.
#a. Проведите регуляризацию в случае неверно выбранных предикторов. Отрисуйте финальный график как в первом случае.
#b. Сравните коэффициенты моделей, что изменилось? В качестве ответа пришлите рисунок и ответ про коэффициенты. Код - необязательно.

set.seed(7) #Фиксируем случайную генерацию
X=rnorm(200)
Z=rnorm(200)
Y=Z+X^2+2*sin(3*X) #настоящая зависимость
plot(X,Y, pch=16)

data=data.frame(Y,poly(X,degree = 10, raw = T)) #Предположим, что модель такая   
mod_1=glm(Y~., data = data)
summary(mod_1)
predict_lm=predict(mod_1, newdata = data)  #по коэффициентикам из предложенной модели строим предсказательный Y
plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #Последняя цифра в rgb() задаёт полупрозрачность
lines(X[order(X)],predict_lm[order(X)], type = "l", col=2,lwd=3) #(отсортировали значения, чтобы адекватный график был)

#регуляризация
data_matrix=cbind(poly(X,degree = 10, raw = T)) 
cross_validation=cv.glmnet(x = data_matrix, y = Y, alpha = 0, nfolds = 10)
plot(cross_validation)
l=cross_validation$lambda.min
model_L2=glmnet(x = data_matrix, y = Y, alpha = 0, lambda = l) #Обучаем с подобранной лямбдой
coef(model_L2) 

predict_L2=predict(model_L2, newx = data_matrix)
plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #Последняя цифра в rgb() задаёт полупрозрачность
lines(X[order(X)],predict_lm[order(X)], type = "l", col=2,lwd=3) 
lines(X[order(X)],predict_L2[order(X)], type = "l", col=4,lwd=3, lty=2)

#ЗАДАЧА 2 
#Сгенерируйте по тому же правилу новый датасет из тысячи наблюдений. Постройте на нём предсказание для уже обученных моделей. Нарисуйте предсказания. Сравните коэффициенты детерминации на выборке из первого задания и на заново сгенерированной.

#для R^2
X=rnorm(1000)
Z=rnorm(1000)
Y=Z+X^2+2*sin(3*X)
data_matrix1=data.frame(poly(X,degree = 10, raw = T))
predict1=predict(mod_1, newdata = as.data.frame(data_matrix1))
predict_L21=predict(model_L2, newx = as.matrix(data_matrix1))
sum((predict1-mean(Y))^2)/sum((Y-mean(Y))^2) #0.1632097
sum((predict_L21-mean(Y))^2)/sum((Y-mean(Y))^2) #0.2838604

plot(X,Y, col=rgb(0,0,0,0.3),pch=16) #Последняя цифра в rgb() задаёт полупрозрачность
lines(X[order(X)],predict1[order(X)], type = "l", col=2,lwd=3) 
lines(X[order(X)],predict_L21[order(X)], type = "l", col=4,lwd=3, lty=2)


#модель без регуляризации на 9800% хуже, чем предсказание простым средним
