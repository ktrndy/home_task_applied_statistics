library(glmnet)

#ЗАДАНИЕ 1
#Сделайте предыдущее дз с L1-штрафом.

set.seed(7) #фиксируем случайную генерацию
X=rnorm(200)
Z=rnorm(200)
Y=Z+X^2+2*sin(3*X) #настоящая зависимость
data_matrix=cbind(poly(X,degree = 10, raw = T)) #предполагаем, что Y - полином 10й степени от X 

#L1-регуляризация
cross_validation=cv.glmnet(x = data_matrix, y = Y, alpha = 1, nfolds = 10) #L1-рег, поэтому alpha=1, а не 0
plot(cross_validation)
l=cross_validation$lambda.min
model_L1=glmnet(x = data_matrix, y = Y, alpha = 1, lambda = l) #обучаем с подобранной лямбдой
coef(model_L1) 
predict_L1=predict(model_L1, newx = data_matrix)
plot(X,Y, col=rgb(0,0,0,0.3),pch=16) 
lines(X[order(X)],predict_L1[order(X)], type = "l", col=4,lwd=3, lty=2) #красивая штука, похожа на обычную модель, без рег., но должен быть подвох

#посчитаем R^2 (для L2 было около 0.3743894), нарисуем
X1=rnorm(1000)
Z1=rnorm(1000)
Y1=Z1+X1^2+2*sin(3*X1)
data_matrix1=data.frame(poly(X1,degree = 10, raw = T))
predict1=predict(model_L1, newx = as.matrix(data_matrix1))
1-sum((predict1-Y1)^2)/sum((Y1-mean(Y1))^2) #-3.164544 всё грустно, хуже, чем с L2-рег.
plot(X1,Y1, col=rgb(0,0,0,0.3),pch=16) 
lines(X1[order(X1)],predict1[order(X1)], type = "l", col=2,lwd=3) #всё очень плохо, график(кроме левой части) похож на модель без рег. из прошлого дз 

#ЗАДАНИЕ 2
#Попытайтесь осознать, что происходит. Для этого: внимательно посмотрите на размер лямбда-штрафа в L1 и в L2 случае. 

#лямбда для L2 из прошлого дз l=1.962212
#лямбда для L1 l=0.002363491 маленькая, из-за чего уравнение для бетты, которое минимизируем, похоже на обычное, без штрафов 

#ЗАДАНИЕ 3
#Примените такую же лямбда-решёточку для дз с L2 регуляризацией, вручную пропихнув её в кросс-валидацию. Наконец, докажите, что регуляризация неидеальна, а то, что мы получили в предыдущей домашке - обман :)

model_L2=glmnet(x = data_matrix, y = Y, alpha = 0, lambda = l)
predict_L2=predict(model_L2, newx = data_matrix)
plot(X,Y, col=rgb(0,0,0,0.3),pch=16) 
lines(X[order(X)],predict_L2[order(X)], type = "l", col=4,lwd=3) 

#посчитаем R^2, нарисуем
predict2=predict(model_L2, newx = as.matrix(data_matrix1))
plot(X1,Y1, col=rgb(0,0,0,0.3),pch=16) 
lines(X1[order(X1)],predict2[order(X1)], type = "l", col=2,lwd=3)  #ещё больше похоже на обычное, без штрафов
1-sum((predict2-Y1)^2)/sum((Y1-mean(Y1))^2) #0.229146 хотя тут не совсем плохо вроде

#для наших данных L2 работает и всё непллохо, но результат зависит от лямбды, которую получаем из кросс-валидации, если подставляем не ту лямбду, то и результат может быть совсем неочень.
#L1 совсем не подходит, R2<0, из теории: штрафы в L1 начисляются лишь за признаки с большим значением коэффициентов. В отличии от L2, тут коэффициенты могут! обнуляются. У нас обнулились при 6,7,8 степенях, из-за чего могли потерять не малую часть "объяснения". 
coef(model_L1) 

cross_validation1=cv.glmnet(x = data_matrix, y = Y, alpha = 0, nfolds = 100, lambda=c(0,exp(seq(from=-10, to = 1, length.out = 100)))) 
plot(cross_validation1)
l1=cross_validation1$lambda.min
model_L21=glmnet(x = data_matrix, y = Y, alpha = 0, lambda = l1)
predict21=predict(model_L21, newx = as.matrix(data_matrix1))
1-sum((predict21-Y1)^2)/sum((Y1-mean(Y1))^2)
#нашли глобальный максимум, получили по лицу, регуляризация не получилась)))