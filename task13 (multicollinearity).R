#ЗАДАНИЕ 1
#Зафиксируйте сид 7, потому что мне нравится это число. Сгенерируйте выборку из 200 N(0,1). Установите пакет car и прочитайте мануал по vif() – они применяются неочевидно.

set.seed(7)
X=rnorm(200)
Y=rnorm(200)

#a. Проверьте на мультиколлинеарность с помощью ВИФов полиномы:

# 3 СТЕПЕНЬ 
data1 = data.frame(Y, poly(X,degree = 3, raw = T))
mod1 = glm(Y~., data = data1)
summary(mod1)
vif(mod1) #вифы ок, нет мультиколлинеарности

# 5 СТЕПЕНЬ 
data2 = data.frame(Y, poly(X, degree = 5, raw = T))
mod2 = glm(Y~., data = data2)
vif(mod2) #есть большие, убираем фактор с максимальным: X^3
mod2 = glm(Y~X1+X2+X4+X5, data = data2)
vif(mod2) #всё ещё есть большие, убираем X^4
mod2 = glm(Y~X1+X2+X5, data = data2)
vif(mod2) #теперь всё красиво 
#итог: у полинома 5 степени мультиколлинеарность есть

# 10 СТЕПЕНЬ 
data3 = data.frame(Y, poly(X,degree = 10, raw = T))
mod3 = glm(Y~., data = data3)
vif(mod3) #вифы слишком большие, мультиколлинеарность есть


#b. Проверьте на мультиколлинеарность синусы вида sin(kx), где:

# SIN 3
seq4 = seq(from = 1, by = 3, length.out = 3)
data4 = data.frame(Y, sin(X %*% t(seq4)))
mod4 = glm(Y~., data = data4)
vif(mod4) #вифы ок, нет мультиколлинеарности

# SIN 5
seq5 = seq(from= 1, by= 3, length.out = 5)
data5 = data.frame(Y, sin(X %*% t(seq5)))
mod5 = glm(Y~., data = data5)
vif(mod5) #вифы ок, нет мультиколлинеарности

# SIN 10
seq6 = seq(from= 1, by= 3, length.out= 10)
data6 = data.frame(Y, sin(X %*% t(seq6)))
mod6 = glm(Y~., data = data6)
vif(mod6) #вифы ок, нет мультиколлинеарности

#На каких выборках из этих шести можно применять регрессию, а на каких – не стоит и почему.
#на полиноме 10й степени - не стоит, в 5 можно избавиться от пары факторов, sin можно


#ЗАДАНИЕ 2
#Избавьтесь от мультиколлинеарности в предыдущем задании, используя правило «максимальный ВИФ меньше 5».

X=rnorm(200)
Z=rnorm(200)
Y=Z+X^2+2*sin(3*X) #настоящая зависимость
data=data.frame(Y,poly(X,degree = 10, raw = T)) #Предположим, что модель такая   
mod=glm(Y~., data = data)
vif(mod) #убираем максимальный X^8
mod=glm(Y~X1+X2+X3+X4+X5+X6+X7+X9+X10, data = data)
vif(mod) #убираем максимальный X^7
mod=glm(Y~X1+X2+X3+X4+X5+X6+X9+X10, data = data)
vif(mod) #убираем максимальный X^6
mod=glm(Y~X1+X2+X3+X4+X5+X9+X10, data = data)
vif(mod) #убираем максимальный X^5
mod=glm(Y~X1+X2+X3+X4+X9+X10, data = data)
vif(mod) #убираем максимальный X^4
mod=glm(Y~X1+X2+X3+X9+X10, data = data)
vif(mod) #убираем максимальный X^3
mod=glm(Y~X1+X2+X9+X10, data = data)
vif(mod) #всё хорошо, мультиколлинеарности нет
#нарисуем, посчитаем R2
X1=rnorm(1000)
Z1=rnorm(1000)
Y1=Z1+X1^2+2*sin(3*X1)
X2=X1^2
X9=X1^9
X10=X1^10
data1=data.frame(X1,X2,X9,X10)
pred1=predict(mod, newdata = as.data.frame(data1))
plot(X1,Y1, col=rgb(0,0,0,0.3),pch=16) 
lines(X1[order(X1)],pred1[order(X1)], type = "l", col=3,lwd=3) 
1-sum((pred1-Y1)^2)/sum((Y1-mean(Y1))^2) #0.3704375
#применим L2-регуляризацию
data_matrix=cbind(X^1,X^2,X^9,X^10) 
cross_validation1=cv.glmnet(x = data_matrix, y = Y, alpha = 0, nfolds = 100, lambda=c(0,exp(seq(from=-10, to = 1, length.out = 100)))) 
l=cross_validation1$lambda.min
model_L2=glmnet(x = data_matrix, y = Y, alpha = 0, lambda = l)
pred_L2=predict(model_L2, newx = as.matrix(data_matrix))
#дорисуем, посчитаем R2
data_matrix1=cbind(X1,X2,X9,X10) 
predict1=predict(model_L2, newx = as.matrix(data_matrix1))
lines(X1[order(X1)],predict1[order(X1)], type = "l", col=2,lwd=3)
1-sum((predict1-Y1)^2)/sum((Y1-mean(Y1))^2) #0.1960211

#ИТОГ: убрали мультиколлинеарность, предсказания хорошие, R2 до L2 регуляризации:0.3704375, после:0.1960211


#ЗАДАНИЕ 3
#В R изначально вшиты несколько интересных выборок. Сейчас нам интересна выборка mtcars. Он уже находится в переменной mtcars. Узнайте, что в нём содержится через мануал.
#a. Избавьтесь от мультиколлинеарности, не вдумываясь в данные.
#b. Посмотрите с точки зрения смысла, какие переменные вылетели и почему.

A=mtcars
mod = glm(mpg ~., data=A) #расход топлива
vif(mod) #убираем disp рабочий объём двигателя
mod = glm(mpg ~cyl+hp+drat+wt+qsec+vs+am+gear+carb, data=A) 
vif(mod) #убираем cyl число цилиндров 
mod = lm(mpg ~hp+drat+wt+qsec+vs+am+gear+carb, data=A) 
vif(mod) #все меньше 10, можем ещё уменьшить, убираем wt вес
mod = glm(mpg ~hp+drat+qsec+vs+am+gear+carb, data=A) 
vif(mod) #убираем hp лошадиные силы
mod = glm(mpg ~drat+qsec+vs+am+gear+carb, data=A) 
vif(mod) #теперь все меньше 5
#Остались: передний мост..., вид двигателя, коробки передач, количество передних передач, количество карбюраторов


#ПРАВКИ!!!!!!!
#mod = glm(mpg ~., data=A) - это не ок
#надо mod = glm(Z ~., data=A), где Z - какой-нить шум случайный

#возьми 10000 точек 
#регрессия снова взорвется :)
#Х^10 - это всегда плохо: даже когда явной мультиколлинеарности нет она может появиться при перегенерации!!!
#ВСЁ ТЕПЕРЬ ХОРОШО: ДО 0.3672226, ПОСЛЕ: 0.3680915!!!!!!

